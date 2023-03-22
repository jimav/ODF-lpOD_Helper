# License: Public Domain or CC0
# See https://creativecommons.org/publicdomain/zero/1.0/
# The author, Jim Avera (jim.avera at gmail) has waived all copyright and
# related or neighboring rights to the content of this file.
# Attribution is requested but is not required.
# -----------------------------------------------------------------------------
# Please note that ODF::lpOD, as of v1.126, has a more restrictive license
# (your choice of GPL 3 or Apache 2.0).
# -----------------------------------------------------------------------------

use strict; use warnings; use feature qw(switch state say);

# We only call ODF::lpOD (and hence XML::Twig), and if we get warnings
# we want to die to force immediate resolution.
# If somebody is launching a moon probe or controlling an artificial heart
# they should audit all libraries they use for 'user warnings FATAL ...'
# and remove such from their private copies of the code.
use warnings FATAL => 'all';

=encoding utf8

=head1 NAME

ODF::lpOD_Helper - ease-of-use wrapper for ODF::lpOD

=head1 SYNOPSIS

  use ODF::LpOD;
  use ODF::LpOD_Helper qw/:chars :DEFAULT/;
  use feature 'unicode_strings';

  Sorry, no examples yet... TODO TODO FIXME

  The following APIs are exported by default:

    Hsearch -- find a possibly-segmented string
    Hreplace -- find and replace strings with high-level style specs
    automatic_style
    common_style
    self_or_parent
    gen_table_name
    fmt_match fmt_node fmt_tree (debug print utilities)

=head1 DESCRIPTION

ODF::lpOD_Helper enables transparent Unicode support
and provides higher-level text search & replace which
can match segmented text including tabs, newlines, and multiple spaces.

Styles may be specified with a high-level notation and
the necessary ODF styles are automatically created and fonts registered.

ODF::lpOD by itself can be inconvenient for text operations because

=over

=item 1.

Method arguments must be passed as encoded binary octets,
rather than character strings (see 'man perlunicode').

=item 2.

I<search()> can not match segmented strings, and so
can not match text which LibreOffice has fragmented for it's own
internal purposes (such as "record changes"), nor can searches
match tab, newline or multiple spaces.

=item 3.

I<replace()> can not replace text stored in multiple segments, and
will illegally store \t, \n, or consecutive spaces
embedded in a single #PCDATA node rather then using the special
ODF objects.

=back

C<ODF::lpOD_Helper>
also fixes a bug causing spurrious S<"Unknown method DESTROY">
warnings (L<https://rt.cpan.org/Public/Bug/Display.html?id=97977>)

=cut

package ODF::lpOD_Helper;

# VERSION
# DATE

our @EXPORT = qw(
  __disconnected_style
  automatic_style common_style
  self_or_parent
  fmt_match fmt_node fmt_tree
  gen_table_name
);
our @EXPORT_OK = qw(
  hashtostring
  $auto_pfx
);

use ODF::lpOD;
BEGIN {
  # https://rt.cpan.org/Public/Bug/Display.html?id=97977
  no warnings 'once';
  no strict 'refs';
  *{"ODF::lpOD::Element::DESTROY"} = sub {}
    unless defined &ODF::lpOD::Element::DESTROY;
}

require Exporter;
use parent 'Exporter';
sub import {
  my $class = shift;
  my @exporter_args = grep{$_ ne ":chars"} @_;
  if (@exporter_args < @_) {
    lpod->Huse_character_strings();
  }
  __PACKAGE__->export_to_level(1, $class, @exporter_args);
}

use constant lpod_helper => 'ODF::lpOD_Helper';

use Carp;
sub oops(@) { unshift @_, "oops! "; goto &Carp::confess; }
use Data::Dumper::Interp qw/ivis ivisq vis visq dvis u/;
use Scalar::Util qw/refaddr blessed reftype weaken isweak/;
use List::Util qw/min max first any all none reduce max sum0/;

# State information for generating & reusing styles is stored per-document.
# A weakened ref to the doc object is saved; it will become undef automatically
# when the doc object is DESTROYed; this is how we know to forget previous
# state if the same memory address has been reused for a new Document object.
#
our %perdoc_state;  # "address" => [ { statehash }, $doc_weakened ]
my $auto_pfx = "auto";
sub get_perdoc_hash($) {
  my $doc = shift;
  confess "not a Document" unless ref($doc) eq "ODF::lpOD::Document";
  my $addr = refaddr($doc);
  my $aref;
  if (($aref = $perdoc_state{$addr})) {
    $aref = undef if !defined($aref->[1]); # object destroyed?
  }
  unless($aref) {
    $perdoc_state{$addr} = $aref = [ {}, $doc ];
    weaken($aref->[1]);
  }
  oops unless isweak($aref->[1]);
  return $aref->[0];
}

sub fmt_node(_;$); # forward
sub fmt_tree(_;@);
sub fmt_match(_);
sub self_or_parent($$);
sub hashtostring($);

my $textonly_prop_re = qr/^(?:font|size|weight|style|variant|color
                              |underline[-\ _](?:color|width|mode|style)
                              |underline|display|language|country
                              |style:font-name|fo-.*
                           )$/x;
my $paraonly_prop_re = qr/^(?:align|align-last|indent|widows|orphans|together
                              |margin|margin[-\ _](?:left|right|top|bottom)
                              |border|border[-\ _](?:left|right|top|bottom)
                              |padding|padding[-\ _](?:left|right|top|bottom)
                              |shadow|keep[-\ _]with[-\ _]next
                              |break[-\ _](?:before|after)
                           )$/x;
my $textorpara_prop_re = qr/^(?:name|parent|clone)$/;

my $text_prop_re = qr/${textorpara_prop_re}|${textonly_prop_re}/;
my $para_prop_re = qr/${textorpara_prop_re}|${paraonly_prop_re}/;

my $table_prop_re = qr/^(?:width|together|keep.with.next|display
                           |margin|margin[-\ _](?:left|right|top|bottom)
                           |break|break[-\ _](?:before|after)
                           |fo:.*     # assume it's ok if retrieved
                           |style:.*  # assume it's ok if retrieved
                           |table.*   # assume it's ok if retrieved
                        )$/x;

# Translate some single-item abbreviated properties
sub __unabbrev_props($) {
  state $abbr_props = {
    "center"      =>  [align => "center"],
    "left"        =>  [align => "left"],
    "right"       =>  [align => "right"],
    "bold"        =>  [weight => "bold"],
    "italic"      =>  [style => "italic"],
    "oblique"     =>  [style => "oblique"],
    "normal"      =>  [style => "normal", weight => "normal"],
    #?? "normal"      =>  [style => "normal", weight => "normal", variant => "normal"],
    "roman"       =>  [style => "normal"],
    "small-caps"  =>  [variant => "small-caps"],
    "normal-caps" =>  [variant => "normal"], #???
  };
  my $input = shift;
  my $output = [];
  for(my $i=0; $i <= $#$input; ++$i) {
    local $_ = $input->[$i];
    if (my $pair=$abbr_props->{$_}) { push @$output, @$pair; }
    elsif (/^(\d[\.\d]*)(pt|%)$/)   { push @$output, "size" => $_; }
    elsif (/^\d[\.\d]*$/)           { push @$output, "size" => "${_}pt"; }
    elsif ($i < $#$input)           { push @$output, $_, $input->[++$i]; }
    else                            { oops(ivis 'Unrecognized abbrev prop $_ (input=$input ix=$i)') }
  }
  return $output;
}

# Create a style.  Paragraph properties may include recognized text
# style properties, which are internally segregated and put into the
# required 'area' property and text style.  Fonts are registered as needed.
sub __disconnected_style($$@) {
  my ($context, $family, @input_props) = @_;
  my %props = @{ __unabbrev_props(\@input_props) };

  # Work around ODF::lpOD::odf_create_style bug which deletes {parent} in
  # a cloned style
  if (my $clonee = $props{clone}) {
    if (my $clonee_parent = $clonee->get_parent_style) {
      oops if $props{parent};
      $props{parent} = $clonee_parent;
    }
  }

  my $doc = $context->get_document;
  my $object;
  if ($family eq 'paragraph') {
    my (@pprops, @tprops);
    while(my ($key, $val) = each %props) {
      if    ($key =~ /${textonly_prop_re}/) { push @tprops, $key, $val; }
      elsif ($key =~ /${para_prop_re}/)     { push @pprops, $key, $val; }
      else { croak "Unrecognized paragraph pseudo-property '$key'" }
    }
    $object = odf_create_style('paragraph', @pprops);
    if (@tprops) {
      my $ts = automatic_style($context, 'text', @tprops);
      $object->set_properties(area => 'text', clone => $ts);
    }
  }
  elsif ($family eq 'text') {
    while(my ($key, $val) = each %props) {
      croak "Unk text prop '$key'\n"
        unless $key =~ /${text_prop_re}/ || $key eq "name";
      if ($key eq "font") {
        unless ($context->get_document_part()->get_font_declaration($val)) {
          $doc->set_font_declaration($val);
        }
      }
    }
    $object = odf_create_style('text', %props);
  }
  elsif ($family eq 'table') {
    while(my ($key, $val) = each %props) {
      croak "Unk table prop '$key'\n"
        unless $key =~ /${table_prop_re}/ || $key eq "name";
    }
    $object = odf_create_style('table', %props);
  }
  else { die "style family '$family' not (yet) supported" }
  return $object;
}

# Like ODF::lpOD get_text methods except:
#   Tab, line break and space objects are translated
# (this is a function, not a method)

# Get text, expanding tab/newline/space objects to corresponding text.
# Although ODF::lpOD::TextElement::get_text() does this
sub __element2vtext($) {
  my $node = shift;

  # Derived from ODF::lpOD::TextElement::get_text
  my $text;
  if ($node->get_tag eq 'text:tab')
          {
          $text = $ODF::lpOD::Common::TAB_STOP;
          }
  elsif ($node->get_tag eq 'text:line-break')
          {
          $text = $ODF::lpOD::Common::LINE_BREAK;
          }
  elsif ($node->get_tag eq 'text:s')
          {
          $text = "";
          my $c = $node->get_attribute('c') // 1;
          $text .= " " while $c-- > 0;
          }
  elsif ($node->get_tag eq '#PCDATA')
          {
          $text = $node->get_text();
          }
  else    {
          confess "not a leaf";
          }
  return $text;
}

sub __canon_options_arg {
  # Called with "&__canon_options_arg;" after shifting off other args.
  # Converts all forms to [ref to array of key => value pairs]
  return [] unless @_;
  return \@_          if ref($_[0]) eq "";  # linear key => value, ...
  return $_[0]        if ref($_[0]) eq "ARRAY" && @_ == 1;
  return [ %{$_[0]} ] if ref($_[0]) eq "HASH"  && @_ == 1;
  confess "Incorrect or extraneous argument(s): ", avis(@_);
}

###############################################################

=head2 The ':chars' import tag

This makes all ODF::lpOD methods accept and return character
strings rather than encoded binary.

You will B<always> want this unless
your application really, really needs to pass un-decoded octets
directly between file/network resources and ODF::lpOD without
looking at the data along the way.  See C<ODF::lpOD_Helper::Unicode>.
Not enabled by default to avoid breaking old programs.

Currently B<:chars> has global effect but might someday become
scoped; to be safe put C<use ODF::lpOD_Helper ':chars'>
at the top of every file.

=head1 METHODS

"Hxxx" methods are installed into appropriate ODF::lpOD packages
so they can be called the same way as native ODF::lpOD methods
('B<H>' denotes extensions from ODF::lpOD_B<H>elper).

=cut

sub ODF::lpOD::Common::Huse_character_strings() {
  # TODO: Can we make this a pragmata which only affects it's scope?
  #
  #   ODF::lpOD::Common::input/output_conversion() methods would
  #   need to use (caller(N))[10] to locate the user's %^H hash
  #   to find out whether to decode/encode; "N" is not fixed, so
  #   those method would need to walk the stack to find the nearest
  #   caller not inside ODF::lpOD::something.
  #
  #   If ODF::lpOD_Helper is someday merged into ODF::lpOD this would
  #   but ugly but reasonably straightforward.
  #
  #   As a separate module ODF::lpOD_Helper might be able to patch
  #   Perl's symbol table to replace those methods using
  #      *ODF::lpOD::Common::input_conversion = &replacement;
  #   however Perl caches method lookups, so if the user's program
  #   managed to call ODF::lpOD methods before loading ODF::lpOD_Helper
  #   then the overrides might not be effective.  It's better to not
  #   go down that rabbit hole!

  $ODF::lpOD::Common::INPUT_CHARSET = undef;
  $ODF::lpOD::Common::OUTPUT_CHARSET = undef;
}


###############################################################

##=head2 @matches = $context->Hsearch($expr)
##
##=head2 $match = $context->Hsearch($expr, OPTIONS)
##
##Locates occurences of C<$expr> in the "virtual text" in C<$context>.
##<$expr> may be a qr/regex/ or plain string, and may contain spaces,
##tabs, and/or newlines.
##
##The "virtual text" is the combined text as if spaces, tabs, and newlines were
##stored internally as text rather than special objects.
##
##C<$context> may be a leaf (#PCDATA) or a container
##or ancestor (paragraph, table cell, or even the document body)
##of the node(s) to search.
##
##OPTIONS may be
##
##  Starting_pos => offset  # start searching that far in
##
##RETURNS:
##
##  In array context: A list of zero or more match hashrefs
##
##  In scalar context: undef or the match hashref
##                     (dies if there were multiple matches)
##
##Each match hash contains:
##
##  {
##    match           => The matched text
##    offset          => Offset in the virtual string (including \t etc.)
##    segments        => [ list of text nodes containing the match ]
##    fseg_offset     => offset into the first node of the start of match
##    lseg_end_offset => offset of end+1 of the match in the last node
##  }
##
##=head2 $context->Hsubstitute([content], [OPTIONS]) # ODF::lpOD::Element method
##
##Replace some or all of the "virtual text" in or below C<$context>,
##optionally inserting new character style spans.
##Elements which end up with empty text are removed.
##
##Leaves are always deleted and possibly replaced with other nodes.
##
##[content] is a (ref to a) list of items which specifies the new content,
##which may include formatting specifiers.
##
##Each C<content> element is either
##
##=over
##
##=item * A reference to a [format PROPs]
##
##=item * A text string which may include spaces, tabs and newlines.
##
##=back
##
##Each [format PROPs] specifies a I<character style>
##which will be applied only to the immediately-following text string(s).
##
##Each PROP is itself either a [key => value] sublist,
##or a string holding one of the following abbreviations:
##
##  "center"      means  [align => "center"]
##  "left"        means  [align => "left"]
##  "right"       means  [align => "right"]
##  "bold"        means  [weight => "bold"]
##  "italic"      means  [style => "italic"]
##  "oblique"     means  [style => "oblique"]
##  "normal"      means  [style => "normal", weight => "normal"]
##  "roman"       means  [style => "normal"]
##  "small-caps"  means  [variant => "small-caps"]
##  "normal-caps" means  [variant => "normal"], #??
##
##  <NUM>         means  [size => "<NUM>pt],   # bare number means point size
##  "<NUM>pt"     means  [size => "<NUM>pt],
##  "<NUM>%"      means  [size => "<NUM>%],
##
##Internally, an ODF "automatic" Style is created for
##each unique combination of PROPs, re-using styles when possible.
##Fonts are automatically registered.
##
##An ODF Style which already exists (or will be created) may be specified
##by a list containing a single PROP like this:
##
##  [ [style-name => (name of style)] ]
##
##[OPTIONS] may contain:
##
##  Search => string or qr/regex/
##
##=over 2
##
##Find an existing string and replace it, preserving existing spans.
##The string must be contained within one paragraph but may be segmented.
##(Without 'Search' the entire text content is replaced.)
##
##=back
##
##  Chomp => TRUE            # Remove \n from the end of content
##
##  Starting_pos => offset   # As in Hsearch
##
##
##RETURNS: Match hashref(s), the same as C<Hsearch>.
##
##Note: C<Hsubstitute> is conceptually like a
##combination of ODF::lpOD's C<search()>
##and C<replace()> or C<set_text()>, and C<set_span()>
##but the search can match segmented text including spaces/tabs/newlines
##and may be invoked directly on a leaf node.
##
##=cut
##
### $context->Hsubstitute([content], [OPTIONS])
##sub ODF::lpOD::Element::Hsubstitute {
##  my ($context, $content_arg, $options_arg) = @_;
##  my @content = @$content_arg;
##  my %opts    = @$options_arg;
##
##  my $Chomp        = delete $opts{Chomp};
##  my $Search_only  = delete $opts{Search_only};
##  my $Search       = delete $opts{Search} // $Search_only // qr/.+/s;
##  my $Starting_pos = delete $opts{Starting_pos} // 0;
##  my $debug        = delete $opts{debug};
##  # my $Onceonly    = delete $opts{Onceonly}; # future
##  croak "Invalid option ",avis(keys %opts) if %opts;
##
##  while (@content and !ref($content[$#content]) and $content[$#content] eq "") {
##    pop @content;
##  }
##  if ($Chomp and @content and !ref $content[$#content]) {
##    $content[$#content] =~ s/\n\z//s;
##  }
##
##  $Search = qr/\Q${Search}\E/s unless ref($Search) eq 'Regexp';
##
##my $show = $debug || 0; #("@content" =~ /NOTES|OTHER.*INFO/);
##
##  my @rlist;
##  # N.B. Paragraphs can be indirectly nested (via frames), so we
##  # need to avoid visiting the same leaf text node more than once.
##  # [Mar2023: **what does this mean? Example?]
##  # TODO: Re-write this to explicitly traverse the tree using XML::Twig
##  #   and directly visit text nodes only once.
##  my %seen;
##  PARA:
##  foreach my $para ($context->descendants_or_self(qr/text:(p|h)/)) {
##    my @segments;
##    my $oldtext = "";
##    my $skipped_chars = 0;
##    my @elements
##      = $para->descendants_or_self(qr/^(#PCDATA|text:tab|text:line-break|text:s)$/);
##    foreach my $e (@elements) {
##      next if $seen{$e}; # incremented below after possible re-visit
##      my $etext = __element2vtext($e);
##      if (length($oldtext)==0
##            && ($skipped_chars + length($etext) <= $Starting_pos)) {
##        # Don't store text we will not search
##        $skipped_chars += length($etext);
##      } else {
##        push @segments, { obj => $e,
##                          offset => length($oldtext),
##                          length => length($etext),
##                          ix     => scalar(@segments),
##                        };
##        $oldtext .= $etext;
##      }
##    }
##    next unless @segments;
##
##    my $prev_repl;
##
##    oops("Starting_pos > length of text") if $Starting_pos > length($oldtext);
##    pos($oldtext) = $Starting_pos;
##
##    while ($oldtext =~ /\G.*?(${Search})/mgsc) {
##      my $start_off = $-[1];
##      my $end_off   = $+[1];
##      my ($fsi, $lsi);
##      foreach (@segments) {
##        $fsi = $_->{ix} if (! defined $fsi) &&
##          $_->{offset} <= $start_off && $start_off < $_->{offset}+$_->{length};
##        $lsi = $_->{ix} if (! defined $lsi) &&
##          $_->{offset} < $end_off && $end_off <= $_->{offset}+$_->{length};
##      }
##      oops unless defined $fsi and defined $lsi;
##      my %r = (
##               match           => $1,
##               offset          => $start_off + $skipped_chars,
##               segments        => [ map{$_->{obj}} @segments[$fsi..$lsi] ],
##               fseg_offset     => $start_off - $segments[$fsi]->{offset},
##               lseg_end_offset => $end_off - $segments[$lsi]->{offset},
##              );
##      unless ($Search_only) {
##        if ($prev_repl) {
##          # This is the second match within the same paragraph...
##          # @segments may have become invalid when we inserted replacement
##          # content for the previous match; so re-get the text anew and
##          # search again, starting immediately after the replaced content
##          # from the previous match.
##          my $repl_length = 0;
##          foreach(@content) { $repl_length += length($_) if ! ref }
##          $Starting_pos = $rlist[$#rlist]->{offset} + $repl_length;
##          redo PARA;
##        }
##        # Insert all the new content (possibly multiple nodes) in place of
##        # the segment containing the start of the match, and delete the
##        # other segments as well.
##        # Anything before or after the match in the deleted segments is
##        # saved and put back with the new content.
##        my $before = substr($oldtext,
##                            $segments[$fsi]->{offset},
##                            $r{fseg_offset});
##        my $after  = substr($oldtext,
##                            $end_off,
##                            $segments[$lsi]->{length} - $r{lseg_end_offset});
##
##if ($show) {
##  warn dvis('### BEFORE: $Search @content $fsi $lsi $start_off $end_off $before $after $oldtext\n');
##  for my $i (0..$#segments) {
##    my %h = %{ $segments[$i] };
##    $h{obj} = fmt_node($h{obj});
##    warn ivisq "     segments[$i] = \%h\n";
##  }
##}
##
##        $r{segments} = [
##          $segments[$fsi]->{obj}->Hinsert_multi(
##                                    [$before, @content, $after],
##                                    [position => NEXT_SIBLING] )
##        ];
##        for my $i (reverse $fsi .. $lsi) {
##          $segments[$i]->{obj}->delete;
##        }
##        if (@{$r{segments}} == 0) {
##          # No content - the node was deleted with no replacement.
##          # Return the containing paragraph so the caller can use it to
##          # find the context, e.g. a containing table cell.
##          #
##          # However if the paragraph is now completely empty, delete it
##          # too and return *its* parent.
##          if (($para->get_text//"") ne "") {
##            @{$r{segments}} = ( $para // oops );
##          } else {
##            @{$r{segments}} = ( $para->parent // oops );
##            $para->delete;
##          }
##        }
##if ($show) {
##  warn "### AFTER :",
##  map{ " segment: ".fmt_node($_)."\n" } @{ $r{segments} };
##}
##        $prev_repl = 1;
##      }
##      push @rlist, \%r;
##    }
##    foreach my $e (@elements) { ++$seen{$e} }
##  }
##
##  return @rlist if wantarray;
##  if (!defined wantarray) {
##    croak "'$Search' did not match anything" if @rlist==0;
##  }
##  croak "'$Search' matched ",scalar(@rlist)," times\n" unless @rlist==1;
##  return $rlist[0];
##}
##
### $context->Hsearch($expr, OPTIONS...)
### $context->Hsearch($expr, [OPTIONS...]) # for solidarity with Hsubstutute
##sub ODF::lpOD::Element::Hsearch {
##  my ($context, $expr, @opts) = @_;
##  @opts = @{$opts[0]} if (@opts==1 && ref($opts[0]) eq "ARRAY");
##  $context->Hsubstitute([], [Search_only => $expr, @opts]);
##}

=head2 @matches = $context->Hsearch($expr)

=head2 $match = $context->Hsearch($expr, OPTIONS)

Finds C<$expr> within the "virtual text" of paragraphs
below C<$context> (or C<$context> itself if it is a paragraph).

The "virtual text" is the concatenation of all leaf nodes in
the paragraph, treating the special tab, newline, and space objects
as if they stored normal text.

Each match must be contained within a paragraph,
but may span segments arbitrarily.  
A match may encompass leaves under different spans.

<$expr> may be a plain string or qr/regex/s 
(the /s option allows '.' to match \n).
Spaces, tabs, and newlines in C<$expr> will match the corresponding 
special ODF objects as well as regular text.

C<$context> may be a paragraph or an ancestor such as a table cell, or even the
document body; all contained paragraphs are searched.

OPTIONS may be

  multi  => BOOL    # Allow multiple matches? (FALSE by default)

  offset => NUMBER  # Starting position within the combined virtual
                    # texts of all paragraphs in C<$context>

A hash is returned for each match:

 {
   match    => The matched text
   segments => [ nodes containing the match ]
   offset   => Offset of match in the first segment
   end      => Offset+1 of end of match in the last segment
   voffset  => Offset of match in the combined virtual texts
   vend     => Offset+1 of match-end in the combined virtual texts
 }

If the last segment is a C<text:tab> or C<text:newline> object 
then 'end' will be 1.
If the last segment is a C<text:s> (which can represents several
consecutive spaces), 'end' will be the number of spaces included in the match.

   Para(s) ║ A match in the Paragraph partly   │
   entirely║  within the 'offset' option       │
   ignored ║                                   │
           ║                                   │
   ------------------voffset--►┊               │
   --------------vend-----------------------►  │
           ║                   ┊            ┊  │
           ║              match┊     match  ┊  │
           ║             ║-off►┊  ║---end---►  │
   ╓──╥────╥──╥────╥─────╥────────╥─────────┴──╖
   ║XX║XXXX║XX║XXXX║XX   ║     ┊MA║TCH******┊  ║
   ║XX║XXXX║XX║XXXX║XX┊SE║ARCHED V║IRTUAL TEXT.║
   ╙──╨────╨──╨────╨──┼──╨────────╨────────────╜
    ─OPTION 'offset'─►┊

RETURNS:

In array context, zero or more hashrefs.

In scalar context, a hashref or undef if there was no match,
and croaks if there were multiple matches.

=head2 $context->Hreplace($expr, [content], OPTIONS)

=head2 $context->Hreplace($expr, sub{...},  OPTIONS)

Search and replace. C<$expr> is a string or qr/regex/s as with C<Hsearch>.

In the first form, each matched substring in the virtual text is
replaced with C<[content]>.

In the second form, the specified sub is called for each match, passing
a I<match hashref> as the only argument (see C<Hsearch> for details).

The sub must return one of the following ways:

  return(REPL_CONTINUE) 
  return(REPL_CONTINUE, expr => $newexpr)

    Nothing is done to the matched text; searching continues,
    optionally with a new search target.

  return(REPL_SUBST_CONTINUE, [content]) or
  return(REPL_SUBST_CONTINUE, [content], expr => $newexpr)
    
    The matched text is replaced by [content] and searching continues.
  
  return(REPL_SUBST_STOP, [content], optRESULTS)

    The matched text is replaced with [content] and then "Hreplace"
    terminates, returning optRESULTS if provided.
    
  return(REPL_STOP, optRESULTS) 
    
    "Hreplace" just terminates.

B<[content]> is specified as described for C<Hinsert>.


=head2 $context->Hinsert([content], OPTIONS)

OPTIONS may contain:

The new content is inserted at a position relative to C<$context>
given by a 'position' argument in OPTIONS (default is FIRST_CHILD).
Possible 'position' values are the same as with C<insert_element()>
as described in C<ODF::lpOD::Element>.

=head3 Content Specification

The C<[content]> argument is a list of zero or more elements, 
each of which is either

=over

=item * A text string which may include spaces, tabs and newlines, or

-item * A reference to [list of format PROPs] 

=back

Each [list of format PROPs] describes a I<character style>
which will be applied only to the immediately-following text string(s).

Each PROP is itself either a [key => value] sublist,
or a string holding one of the following abbreviations:

  "center"      means  [align => "center"]
  "left"        means  [align => "left"]
  "right"       means  [align => "right"]
  "bold"        means  [weight => "bold"]
  "italic"      means  [style => "italic"]
  "oblique"     means  [style => "oblique"]
  "normal"      means  [style => "normal", weight => "normal"]
  "roman"       means  [style => "normal"]
  "small-caps"  means  [variant => "small-caps"]
  "normal-caps" means  [variant => "normal"], #??

  <NUM>         means  [size => "<NUM>pt],   # bare number means point size
  "<NUM>pt"     means  [size => "<NUM>pt],
  "<NUM>%"      means  [size => "<NUM>%],

Internally, an ODF "automatic" Style is created for
each unique combination of PROPs, re-using styles when possible.
Fonts are automatically registered.

An ODF Style which already exists (or will be created) may be indicated
by a list containing a single PROP like this:

  [ [style-name => (name of style)] ]

=head3 Spans

In ODF, character styles are applied to text segments by storing the
text segments as children of a B<span> object which specifies the style.

C<Hreplace> will "replicate and flatten" any existing styles so that any
newly inserted styles appear at the top level.  For example, consider
a document which initially looks like this:

  ┌─────┐  ┌─────┐  ┌─────┐   ┌─────┐
  │AAAAA├─►│BBBBB├─►│Span1├──►│CCCCC│
  └─────┘  └─────┘  └──┬──┘   └─────┘
                       │
              ┌────────┘
              ▼
           ┌─────┐  ┌─────┐  ┌─────┐
           │ddddd├─►│eeeee├─►│fffff│
           └─────┘  └─────┘  └─────┘

If the text "eeeee" is replaced by "ZZZ" but covered by a new style,
the following might result from a naive implementation:
  
  ┌─────┐  ┌─────┐  ┌─────┐   ┌─────┐
  │AAAAA├─►│BBBBB├─►│Span1├──►│CCCCC│
  └─────┘  └─────┘  └──┬──┘   └─────┘
                       │
              ┌────────┘
              ▼
           ┌─────┐  ┌─────┐  ┌─────┐
           │ddddd├─►│Span2├─►│fffff│
           └─────┘  └──┬──┘  └─────┘
                       │
                       ▼
                     ┌───┐
                     │ZZZ│
                     └───┘

However the nested spans are undesirable.  After "replicate and flatten",
this is the result:
  
  ┌─────┐  ┌─────┐  ┌──────┐  ┌─────┐  ┌──────┐  ┌─────┐
  │AAAAA├─►│BBBBB├─►│Span1a├─►│Span2├─►│Span1b├─►│CCCCC│
  └─────┘  └─────┘  └──┬───┘  └──┬──┘  └──┬───┘  └─────┘
                       │         │        │
                       ▼         ▼        ▼
                    ┌─────┐   ┌─────┐   ┌─────┐
                    │ddddd│   │(new)│   │fffff│
                    └─────┘   └─────┘   └─────┘

The original C<Span1> was replicated and the new span (C<Span2>) floated
up to the top level.

=cut

sub ODF::lpOD::Element::Hsearch {
  my $context = shift;
  my $expr    = shift;
  my %opts    = @{ &__canon_options_arg };

  my $offset = delete $opts{offset} // 0;
  my $multi  = delete $opts{multi};
  my $debug  = delete $opts{debug};
say "at ",__LINE__ if $debug;
  croak "Invalid option ",avis(keys %opts) if %opts;

  my $regex = ref($expr) eq 'Regexp' ? $expr : qr/\Q${expr}\E/s;

  #  $vtext holds the virtual text from the *current* paragraph
  #  excluding any initial segments which were skipped because
  #  they lie entirely before 'offset' in the combined virtual text:
  # 
  #         ║                                   ║ ║
  #  Para(s)║           Paragraph 'x'           ║ ║   later Paragraph
  #   before║                                   ║ ║
  # ------------------voffset---►┊              ║ ║
  # --------------vend-----------------------►┊ ║ ║
  #         ║                    ┊            ┊ ║ ║
  #         ║              match ┊    match   ┊ ║ ║ match           ║match
  #         ║             ║-off-►┊ ║--end----►┊ ║ ║offset-►┊        ║end►┊
  # ╓──╥────╥──╥────╥─────╥──────┬─╥──────────┴─╖ ╓────────┬──╥─────╥────┴──╖
  # ║XX║XXXX║XX║XXXX║XX   ║      MA║TCHED TEXT┊ ║ ║        MAT║CHED ║TEXT┊  ║
  # ║XX║XXXX║XX║XXXX║XXsea║rched te║xt..........║ ║searched te║xt...║.......║
  # ╙──╨────╨──╨────╨─────╨────────╨────────────╜ ╙───────────╨─────╨───────╜
  #  ─── offset───────►┊         ┊            ┊ ║ ║                         ║
  #         ║skipped║....$vtext (para 'x')......║ ║...$vtext (later para)...║
  #         ║ _chars║            ┊            ┊ ║ ║                         ║
  #         ║       ║─$vtoffset─►┊            ┊ ║ ║                         ║
  #         ║       ║───────────────$vtend───►┊ ║ ║                         ║
  #                                             ║                           ║
  #  ────$totlen_sofar (@end of para x)────────►║                           ║
  #  -----------------------------------$totlen_sofar (para #N)------------►║
  my @matches;
  my %seen;
  my $totlen_sofar = 0;
  PARA:
  foreach my $para ($context->descendants_or_self(qr/text:(p|h)/)) {
say dvis '##Hs START PARA $offset $totlen_sofar $regex $para at ',__LINE__ if $debug;
    my $vtext = "";
    my ($vtoffset, $vtend);
    my $skipped_chars = 0;
    my $vtext_pos;
    my @seginfo;
    my @input_segs = $para->descendants_or_self(
                              qr/^(#PCDATA|text:tab|text:line-break|text:s)$/);
    
    # Add segments (while any remain) to $vtext until a match occurs which
    # ends before the end of $vtext.  This allows the regex to match as much
    # as it can.  
    # N.B. The last seg might be entirely beyond the match.
    SEG:
    while (my $e = shift @input_segs) {
      oops if $seen{$e}++;  # sanity check
      my $etext = __element2vtext($e);
      $totlen_sofar += length($etext);
      if ($totlen_sofar <= $offset) {
        # Don't save text we will not search
        oops unless length($vtext)==0;
        $skipped_chars += length($etext);
say dvis '##Hs SKIPPING SEG: $offset $etext $skipped_chars $totlen_sofar at ',__LINE__ if $debug;
        next SEG;
      }
      push @seginfo, { obj    => $e,
                       vtoff  => length($vtext),
                       seglen => length($etext),
                       ix     => scalar(@seginfo),
                     };
      $vtext .= $etext;
      my $starting_offset = $totlen_sofar - length($vtext);
      if ($starting_offset <= $offset) {
        # we are in the paragraph $offset points into
        $vtext_pos = $offset - $starting_offset;
      } else {
        $vtext_pos = 0;
      }
say ivis '##Hs SEG $seginfo[-1]',dvis '\n     $vtext $vtext_pos $offset $starting_offset $skipped_chars $totlen_sofar  at ',__LINE__ if $debug;
      oops if $vtext_pos < 0;
      oops if $vtext_pos >= length($vtext);
      next SEG
        if @seginfo==0;  # skipped all segments so far
      CURR_SEG: {
        pos($vtext) = $vtext_pos;
        if ($vtext =~ /\G.*?(${regex})/s
             && ($+[1] < length($vtext) || @input_segs==0)) {
          # MATCHED
          $vtoffset = $-[1];
          $vtend    = $+[1];
          my ($fsi, $lsi);
          for(my $ix = 0; !defined($fsi) or !defined($lsi); ++$ix) {
            oops(dvis '$fsi $lsi $vtoffset $vtend $vtext_pos $vtext $regex @seginfo\n', fmt_tree($para)) if $ix > $#seginfo;
            my $s = $seginfo[$ix];
            $fsi = $ix if !defined($fsi)
                          && $s->{vtoff} <= $vtoffset
                          && $s->{vtoff}+$s->{seglen} > $vtoffset;
            $lsi = $ix, if !(defined $lsi)
                          && ($s->{vtoff} < $vtend || $vtend==0)
                          && $s->{vtoff}+$s->{seglen} >= $vtend;
          }
          push @matches,
            {
               match           => substr($vtext, $vtoffset, $vtend-$vtoffset),
               segments        => [ map{$_->{obj}} @seginfo[$fsi..$lsi] ],
               offset          => $vtoffset - $seginfo[$fsi]->{vtoff},
               end             => $vtend - $seginfo[$lsi]->{vtoff},
               voffset         => $starting_offset + $vtoffset,
               vend            => $starting_offset + $vtend,
               paragraph       => $para,
            };
    say '##Hs match #',$#matches,dvis ' $skipped_chars $offset $totlen_sofar $vtext_pos $vtoffset $vtend at ',__LINE__,"\n",fmt_match($matches[-1]) if $debug;
          last PARA unless $multi;
          $offset = $totlen_sofar - (length($vtext) - $vtend);
          if ($vtend < length($vtext)) {
            $vtext_pos = $vtend;
            redo CURR_SEG
          } # else start on next seg
        }#matched
        else { 
          say dvis '[nomatch] $vtext_pos $regex $vtext' if $debug; 
        }
      }
      say '##Hs advancing to next SEG.  at ',__LINE__ if $debug;
    }#while input_segs remain in current paragraph
  }#PARA
  say "##Hs FINISHED, found ",scalar(@matches)," matches  at ",__LINE__ if $debug;

  return @matches
    if wantarray;
  confess "void context, result would be discarded"
    unless defined wantarray;
  # scalar context
  croak "'$expr' matched ",scalar(@matches)," times\n" if @matches > 1;
  return @matches > 0 ? $matches[0] : undef;
}#Hsearch

=head2 ??? lpod_helper->Hreplace_match([content], $match, [OPTIONS])

=head2 Hreplace_match([content], $match, [OPTIONS])

**** REWORK THIS ****

Replace previously-matched text,
preserving existing spans (and thus styles which apply to them).

C<$match> is a hashref returned by C<Hsearch>.

Important: Never replace more than one match returned by the same C<Hsearch>
call unless you know they refer to different paragraphs (the first
replacement may invalidate references in all matches pointing
to that paragraph).  Use C<Hsubstitute> to replace
multiple occurrences which might lie in the same paragraph.

Note that C<Hreplace_match> is a FUNCTION, not a method, because
the context is implied by C<$match>.

OPTIONS may be:

  Chomp => TRUE     # Remove any final \n from content

C<[content]> is a (ref to a) list of items which describe the new content,
possibly including format specifiers.

Each member of C<content> is either

=over

=item * A text string which may include spaces, tabs and newlines.

=item * [format PROPs]

=back

Each [format PROPs] list specifies a I<character style>
which will be applied only to the immediately-following text string(s).

Each PROP in a list is itself either a [key => value] sublist,
or a string holding one of the following abbreviations:

  "center"      means  [align => "center"]
  "left"        means  [align => "left"]
  "right"       means  [align => "right"]
  "bold"        means  [weight => "bold"]
  "italic"      means  [style => "italic"]
  "oblique"     means  [style => "oblique"]
  "normal"      means  [style => "normal", weight => "normal"]
  "roman"       means  [style => "normal"]
  "small-caps"  means  [variant => "small-caps"]
  "normal-caps" means  [variant => "normal"], #??

  <NUM>         means  [size => "<NUM>pt],   # bare number means point size
  "<NUM>pt"     means  [size => "<NUM>pt],
  "<NUM>%"      means  [size => "<NUM>%],

Internally, an ODF "automatic" Style is created for
each unique combination of PROPs, re-using styles when possible.
Fonts are automatically registered.

An ODF Style which already exists (or will be created) may be specified by

  [style-name => STYLENAME]

which must be the only PROP in the list.

=cut

sub Hreplace_match($$@) {
  my $context = shift;
  my $match   = shift;
  my %opts    = @{ &__canon_options_arg };
  die "unimp";
}

###############################################################

=head2 $context->Hsubstitute($expr, [content], [OPTIONS])

***??? rename Hsubstitute -> Hreplace to be analog of para->replace:e

Replace I<all> occurrences of C<$expr> in the virtual text of C<$context>
with C<[content]>.

C<$expr> is as described for C<Hsearch>.

C<[content]> is as described for C<Hreplace_match>.

OPTIONS may contain:

  Chomp => TRUE           # Remove any final \n from content

  Starting_pos => offset  # As in Hsearch

RETURNS: Match hashref(s) similar C<Hsearch>.

Note: C<Hsubstitute> is conceptually like a
combination of ODF::lpOD's C<search()>
and C<replace()> or C<set_text()>, and C<set_span()>
but the search can match segmented text including spaces/tabs/newlines
and may be invoked directly on a leaf node.

=cut

sub ODF::lpOD::Element::Hsubstitute {
  my $context     = shift;
  my $expr        = shift;
  my $content_arg = shift;
  my %opts    = @{ &__canon_options_arg };
  my @content = @$content_arg;
  my @results;
  my $para;
  SEARCH:
  {
    my @matches = $context->Hsearch($expr, %opts);
    foreach my $match(@matches) {
      if ($para && $match->{paragraph} == $para) {
        # A second match in the same paragraph: Segments were replaced, so
        # so other matchrefs pointing to the paragraph are now invalid!
        # Re-run the search with Starting_offset pointing immediately
        # after the previous replacement.
        my $repl_length = 0;
        foreach(@content) { $repl_length += length($_) unless ref }
        $opts{Starting_off} = $results[-1]->{offset} + $repl_length;
        redo SEARCH;
      }
      $para = $match->{paragraph};
      my %result;
      $result{offset} = $match->{offset}; # posn in entire virtual text
      $result{match}  = $match->{match};  # matched subtring
      my $before = substr( __element2vtext($match->{segments}->[0]),
                           0,
                           $match->{fseg_offset} );
      my $after  = substr( __element2vtext($match->{segments}->[-1]),
                           $match->{lseg_offset} );
      $result{segments} = [
        $match->{segments}->[0]->Hinsert_multi(
                                   [$before, @content, $after],
                                   [position => NEXT_SIBLING] )
      ];
      for my $seg (reverse @{$match->{segments}}) {
        $seg->{obj}->delete;
      }
      $result{fseg_offset} = strlen($before);
      if (@{$result{segments}} > 0) {
        ### This seems inefficient; can Hinsert_multi provide more info? FIXME
        $result{lseg_end_offset} =
          length(__element2vtext(${ $result{segments} }[-1]))
          - strlen($after);
      } else {
        # No content - the node was deleted with no replacement.
        oops if $before || $after;
        $result{fseg_offset} = $result{lseg_end_offset} = 0;

        # Return the containing paragraph so the caller can use it to
        # find the context, e.g. a containing table cell.
        #
        # However if the paragraph is now completely empty, delete it
        # too and return *its* parent.
        if (($para->get_text//"") ne "") {
          @{$result{segments}} = ( $para // oops );
        } else {
          @{$result{segments}} = ( $para->parent // oops );
          $para->delete;
        }
      }
      push @results, \%result;
    }
  }
  @results
}

###############################################################

=head2 $context->Hinsert_multi([content...], [OPTIONS])

Insert the I<content> at the location relative to C<$context> given by

  position => <location>

in I<OPTIONS>.

C<< <location> >> is
B<FIRST_CHILD>, or B<NEXT_SIBLING>, etc. see "Child element creation methods"
in C<ODF::LpOD::Element>.

The I<content> is specified as a list of character strings
and [PROP...] styles as described for C<Hreplace_match()>.

Nothing is inserted if all text is "".

RETURNS: List of node(s) inserted

=cut

# $context->Hinsert_multi([content...], [options...])
sub ODF::lpOD::Element::Hinsert_multi($$) {
  my $context     = shift;
  my $content_arg = shift;
  my %opts = (
    position => FIRST_CHILD,  # the default
    @{ &__canon_options_arg },
  );
  my @content = @$content_arg;

  my $position = $opts{position};

  my $root = $context->get_root;

  # Ignore extraneous initial ""s
  while (@content and ! ref $content[0] and $content[0] eq "") {
    shift @content
  }

  my @nodes;
  while (@content) {
    local $_ = shift @content;
    if (ref) {
      my $tprops = $_;
      my $text = shift @content
        // croak "[text style] not followed by a string\n";
      ! ref($text)
        // croak "consecutive [text style]s (no strings inbetween)\n";

      my $stylename;
      if (@$tprops == 2 && $tprops->[0] =~ /^style[-_ ]name$/) {
        $stylename = $tprops->[1];
      } else {
        my $ts = automatic_style($root, 'text', @$tprops) // oops;
        $stylename = $ts->get_name;
      }

      my $node = ODF::lpOD::Element->create('text:span')->set_class;
      $node->set_attributes({style => $stylename});
      $node->set_text($text);
      $context->insert_element($node, position => $position);
      $context = $node;
      $position = NEXT_SIBLING;
      push @nodes, $node;
    } else {
      while (@content && ! ref $content[0]) {
        $_ .= shift @content;  # concatenate adjacent texts
      }
      while ($_ ne "") {
        my $node;
        if (s/^(  +)//) {
          $node = ODF::lpOD::Element->create('text:s')->set_class;
          $node->set_attribute('a',length($1));
        }
        elsif (s/^\t//) {
          $node = ODF::lpOD::Element->create('text:tab')->set_class;
        }
        elsif (s/^\n//s) {
          $node = ODF::lpOD::Element->create('text:line-break')->set_class;
        }
        elsif (s/^((?:[^\t\n ]|(?<! ) (?! ))+)//s) {
          $node = ODF::lpOD::Element->create('#PCDATA')->set_class;
          $node->set_text($1);
        }
        else { oops }
        oops unless $node;
        $context->insert_element($node, position => $position);
        $context = $node;
        $position = NEXT_SIBLING;
        push @nodes, $node;
      }
    }
  }

  return @nodes;
}

=head1 FUNCTIONS (not methods)

=cut

####################################################
#
#=head2 $context->Hget_vtext()
#
#Returns the "virtual text" contained by C<$context>, with
#possibly-multiple spaces, tab, and newline characters corresponding to
#the special objects which represent those things
#(i.e. tags I<text:s>, I<text:tab>, I<text::line-break>, and I<text:s>).
#
#In comparison, the regular C<get_text> those special objects are represented
#as a single space.
#
#sub ODF::lpOD::Element::Hget_vtext() {
#  my $context = shift;
#  confess "Hget_vtext takes no arguments" if @_ > 0;
#
#  my @elements = $context->descendants_or_self(
#                              qr/^(#PCDATA|text:tab|text:line-break|text:s)$/);
#
#  join "", map{ __element2vtext($_) } @elements
#}


###################################################

=head2 self_or_parent($node, $tag)

**FIXME: Can this be made a method of ODF::lpOD::Element ??

Returns $node or it's nearest ancestor which matches a gi

Currently this throws an exception if neither $node or an ancestor
matches $tag.

=cut

sub self_or_parent($$) {
  my ($node, $tag) = @_;
  my $e = $node->passes($tag) ? $node : $node->parent($tag);
  # Should we return undef instead of croaking??
  croak "Neither node nor ancestors match ",vis($tag),"\n" unless $e;
  return $e;
}

###################################################

=head2 automatic_style($context, $family, PROP...)

Find or create an 'automatic' (i.e. functionally anonymous) style and
return the object.  Styles are re-used when possible, so a style should
not be modified because it might be shared.

PROPs are as described for C<Hsubstitute>.

**FIXME: Can this be made a method of ODF::lpOD::Document or ???

=head2 common_style($context, $family, PROP...)

Create a 'common' (i.e. named by the user) style.

The name must be given by [name => "STYLENAME"] somewhere in PROPs.

=cut

sub automatic_style($$@);
sub automatic_style($$@) {
  my ($context, $family, @input_props) = @_;
  my $doc = $context->get_document // oops;
  my %props = @{ __unabbrev_props(\@input_props) };

  my $sh = get_perdoc_hash($doc);
  my $style_caches = ($sh->{style_caches} //= {});
  my $counters     = ($sh->{counters}     //= {});

  my $cache = ($style_caches->{$family} //= {});
  my $cache_key = hashtostring(\%props);
  my $stylename = $$cache->{$cache_key};
  if (! defined $stylename) {
    for (;;) {
      $stylename = $auto_pfx.uc(substr($family,0,1)).(++$counters->{$family});
      last
        unless defined (my $s=$doc->get_style($family, $stylename));
      my $existing_key = hashtostring(scalar $s->get_properties);
      $$cache{$existing_key} //= $stylename;
    }
    $$cache{$cache_key} = $stylename;
    my $object = __disconnected_style($context,$family, %props, name=>$stylename);
    return $doc->insert_style($object, automatic => TRUE);
  } else {
    return $doc->get_style($family, $stylename);
  }
}

sub common_style($$@) {
  my ($context, $family, @input_props) = @_;
  my %props = @{ __unabbrev_props(\@input_props) };
  croak "common_style must specify 'name'\n" unless $props{name};
  my $object = __disconnected_style($context, $family, %props);
  return $context->get_document->insert_style($object, automatic => FALSE);
}

###################################################

=head2 gen_table_name($context)

**FIXME: Should be a method of ODF::lpOD::Document (or call $self->get_document)

Generate a table name not currently used of the form "Table<NUM>".

=cut

sub gen_table_name($) {
  my $context = shift;
  state $table_names = { map{ ($_->get_name() => 1) }
                            $context->get_document->get_body->get_tables };
  state $seq = 1;
  my $name;
   do { $name=$auto_pfx."Table".$seq++ } while exists $table_names->{$name};
  $table_names->{$name} = 1;
  return $name;
}

###################################################

=head2 hashtostring($hashref)

Returns a single string representing the keys and values of a hash

=cut

sub hashtostring($) {
  my $href = shift;
  return join("!", map{ "$_=>$href->{$_}" } sort keys %$href);
}

###################################################

=head2 fmt_node($node)

Format a single node for debug messages, without a final newline.

=head2 fmt_tree($top)

Format a node and all of it's children (sans final newline).

=head2 fmt_match($matchhash)

Format a match hashreffor debug messages (sans final newline).

=cut

sub fmt_node(_;$) {  # sans final newline
  my ($node, $tailtextonly) = @_;
  if (! ref $node) {
    return "(invalid node: ".vis($node).")";
  }
  my $text = eval{ $node->get_text }; # (resursivly gets full virtual text)
  if (defined($text)) {               
    $text = ODF::lpOD::Common::input_conversion($text);#undo implicit encode
    $text .= " (len=".length($text).")"                #(except in :chars mode)
  }
  my $tag  = eval{ $node->tag };
  my $att  = eval{ $node->get_attributes };
  my $s = "$node";
  $s =~ s/ODF::lpOD:://;
  $s =~ s/=HASH//;   # ref($node)
  $s .=  " $tag" if defined $tag;
  $s .= " ".(%$att && $tag =~ /^(table-cell|sequence)/ ? "{...}" : vis($att))
    if defined $att;

  $s .= " ".vis($text)
    if defined($text) && (!$tailtextonly
                            || $tag !~ /text:(box|span|text|p)|office:/);
  return $s;
}
sub _fmt_tree($$$);
sub _fmt_tree($$$) {
  my ($obj,$indent,$sref) = @_;
  $indent //= 0;
  $$sref .= " "x$indent.fmt_node($obj,1)."\n";
  return unless ref $obj;
  foreach my $e ($obj->children) {
    my $oldtext = $e->get_text;
    _fmt_tree($e,$indent+1,$sref);
  }
}
sub fmt_tree(_;@) { # sans final newline
  my $top = shift;
  my %opts = (indent => 0, @_);
  my $indent = $opts{indent};
  my $string = "";
  if ($opts{ancestors} and ref $top) {
    $opts{indent} ||= 1;
    my @a = reverse $top->ancestors;
    shift @a; # don't show the document container
    foreach my $e (@a) {
      $string .= "<"x$indent.fmt_node($e,1)."\n";
      $indent++;
    }
  }
  _fmt_tree($top, $indent, \$string);
  return "------------\n".$string."------------";
}

sub fmt_match(_) { # sans final newline
  my $href = shift;
  my %h = %$href;
  my @segments = map { 
                  my $t = __element2vtext($_);
                  "$_ ".vis("$t")." (len=".length($t).")"
                 } @{ delete $h{segments} }; 
  my $match_str = delete $h{match};
  my $s = "{";
  $s .= "\n  match=".vis($match_str)." (len=".length($match_str).")";
  foreach my $k ('match', 'paragraph', [qw/voffset vend/],
                 [qw/offset end/], 'segments') {
    my $ss = "";
    if (ref $k) {
      foreach my $key (@$k) {
        $ss .= " $key=".vis( delete($h{$key}) ) if exists $h{$key};
      }
    } else {
      $ss .= " $k=".vis( delete($h{$k}) ) if exists $h{$k};
    }
    $s .= "\n ".$ss if $ss;
  }
  for my $key (keys %h) {
    $s .= "\n  $key=".vis(delete $h{$key}) if exists $h{$key};
  }
  $s .= "\n  segments => [\n    ".join("\n    ",@segments)."\n  ]";
  return $s."\n}";
}

###################################################

=head1 UNICODE ISSUES

The usual Perl paradigm is to *decode* character data immediately upon
receipt from an external source, process the data as Perl character
strings without concern for encoding,
and finally *encode* results just before sending them out.
Often this can be done automatically by calling
C<open> or C<binmode> with an ":encoding()" specification.

For historical reasons
ODF::lpOD is incompatible with the above paradigm by default
because it's methods always encode result strings (e.g. into UTF-8)
before returning them to you, and attempts to decode strings you pass in before
using them.  Therefore, by default,
you must work with binary rather than character strings;
regex match, substr(), length(), etc. will not do what you want
with non-ASCII characters
(ASCII slides by because C<encode> and C<decode> are essentially no-ops
for those characters).

B<< use ODF::lpOD_Helper ':chars'; >>
disables ODF::lpOD's internal encoding and decoding,
so that methods speak and listen in characters, not octets.

It is possible to toggle between the old behavior and character-string
mode:
I<< set_input/output_charset() >> (see C<ODF::lpOD::Common>)
will re-enable implicit decoding/encoding of method arguments
if the B<:chars> tag was imported.
And I<< lpod->Huse_character_strings() >> will disable the old behavior
and restore B<:chars> mode.

If the above discussion is bewildering, you are not alone; the official Perl
docs to read are 'man perlunicode' and it's references, but they can be daunting
on first read.   Here is yet another overview of the subject:

=over

A I<character> is an abstract thing defined only by it's Unicode code point.
Obviously Perl must represent characters somehow in memory, but you 
do not care how because Perl *character* strings behave as a sequence
of those abstract entities called characters, each of length 1, 
regardless of their internal representation.  Internally, Perl represents
some characters as a single byte and others as multiple bytes, but
this is invisible to Perl code.

The ONLY time your program must know how characters are stored
is when communicating with the world outside of Perl.
When reading input, your program uses C<decode> to convert 
from the external representation (which you specify) to Perl's internal
representation (which you don't know); and you use C<encode> when
writing to disk or network to convert from Perl's internal rep to
the appropriate external representation (which you specify).  The
C<encoding(...)> option to C<open> and C<binmode> make Perl's I/O system
do the encode or decode for you. 

Encoded data (the "external representation") is stored by Perl in "binary"
strings, which are strings internally distinguished by Perl to work as
a sequence of arbitrary octets rather than as containers for characters;
operations like C<length()> and C<substr()> treat the stored octets individually,
just as you would want when working with binary data.
The difference is that with *character* strings, the octets actually stored 
are determined by Perl according to it's scheme for representing Unicode 
characters; the individual octets are not accessible except via back doors.

Before Perl implemented Unicode all strings were "binary", which was okay
because all characters were represented as single bytes.
Nowadays there are two species of strings, and they must be kept separate.
Inter-species marriage (for example concatenation) will yield wrong results.

By the way, encode and decode are very fast when the UTF-8 encoding is used
because Perl often (but not always) uses UTF-8 as it's internal representation 
and in that case there's nothing to do.  
However you still must perform the encode & decode operations so 
that Perl will know which strings represent abstract characters
and which should give direct access to the stored octets.

=back

=head1 HISTORY

The original ODF::lpOD_Helper was written in 2012.  The code was reworked
and this manual written in 2023.

As of Feb 2023,
ODF::lpOD is not actively maintained (last updated in 2014, v1.126),
and is now unusable as-is because of the warning mentioned above.
With ODF::lpOD_Helper, ODF::lpOD is once again an
extremely useful tool.

=head1 AUTHOR

Jim Avera  (jim.avera AT gmail dot com)

=head1 LICENSE

ODF::lpOD (v1.126) may be used under the GPL 3 or Apache 2.0 license.

ODF::lpOD_Helper is in the Public Domain (or CC0 license), but
requires ODF::lpOD to function so as a practical matter
use must comply with ODF::lpOD's license.

=for Pod::Coverage oops

=cut

1;
