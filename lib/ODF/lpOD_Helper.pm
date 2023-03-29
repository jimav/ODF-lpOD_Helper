# License: Public Domain or CC0
# See https://creativecommons.org/publicdomain/zero/1.0/
# The author, Jim Avera (jim.avera at gmail) has waived all copyright and
# related or neighboring rights to the content of this file.
# Attribution is requested but is not required.
# -----------------------------------------------------------------------------
# Please note that ODF::lpOD, as of v1.126, has a more restrictive license
# (your choice of GPL 3 or Apache 2.0).
# -----------------------------------------------------------------------------

use strict; use warnings; use feature qw(switch state say current_sub);

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
    Hinsert_content
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
will store \t, \n, or consecutive spaces
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
  REPL_CONTINUE REPL_SUBST_CONTINUE REPL_SUBST_STOP REPL_STOP
);
our @EXPORT_OK = qw(
  hashtostring
  $auto_pfx
);

use constant {
  REPL_CONTINUE => "REPL_CONTINUE",
  REPL_SUBST_CONTINUE => "REPL_SUSBST_CONTINUE",
  REPL_SUBST_STOP => "REPL_SUBST_STOP",
  REPL_STOP => "REPL_STOP",
};

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
#sub btw(@) { local $_=join("",@_); s/\n\z//s; say "$_  \@ ".(caller(0))[2]; }
sub btw(@) { local $_=join("",@_); s/\n\z//s; say "[".(caller(0))[2]."]$_"; }
use Data::Dumper::Interp qw/visnew ivis ivisq vis visq addrvis refvis dvis u/;
use Scalar::Util qw/refaddr blessed reftype weaken isweak/;
use List::Util qw/min max first any all none reduce max sum0/;

# State information for generating & reusing styles is stored per-document.
# A weakened ref to the doc object is saved; it will become undef automatically
# when the doc object is DESTROYed; this is how we know to forget previous
# state if the same memory address has been reused for a new Document object.
#
our %perdoc_state;  # "address" => [ { statehash }, $doc_weakened ]
my $auto_pfx = "auto";
sub _get_per_doc_hash($) {
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
  my $tag = $node->get_tag;
  if ($tag eq 'text:tab')
          {
          $text = $ODF::lpOD::Common::TAB_STOP;
          }
  elsif ($tag eq 'text:line-break')
          {
          $text = $ODF::lpOD::Common::LINE_BREAK;
          }
  elsif ($tag eq 'text:s')
          {
          $text = "";
          my $c = $node->get_attribute('c') // 1;
          $text .= " " while $c-- > 0;
          }
  elsif ($tag eq '#PCDATA')
          {
          $text = $node->get_text();
          }
  else    {
          confess "not a leaf: $tag";
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

# TODO: Can we make :chars a pragmata which only affects it's scope?
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

sub ODF::lpOD::Common::Huse_character_strings() {
  $ODF::lpOD::Common::INPUT_CHARSET = undef;
  $ODF::lpOD::Common::OUTPUT_CHARSET = undef;
  # It would be nicer if lpod->set_input_charset(undef) worked...
}
sub ODF::lpOD::Common::Huse_octet_strings() {
  lpod->set_input_charset("UTF-8");
  lpod->set_output_charset("UTF-8");
}


###############################################################

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
   match    => The matched virtual text
   segments => [ leaf nodes containing the match ]
   offset   => Offset of match in the first segment's virtual text
   end      => Offset+1 of end of match in the last segment's v.t.
   voffset  => Offset of match in the combined virtual texts
   vend     => Offset+1 of match-end in the combined virtual texts
 }

        Para.#1 ║ Paragraph #2 containing a match  │
        (ignord)║  spread over several segments    │
                ║                                  │
                ║                                  │
        ------------match voffset---►┊             │
        --------match vend---------------------►┊  │
                ║                    ┊          ┊  │
                ║              match ┊   match  ┊  │
                ║             ║-off-►┊ ║--end--►┊  │
        ╓──╥────╥──╥────╥─────╥──────┬─╥────────┬──╖
        ║xx║xxxx║xx║xxxx║xx...║......**║*MATCH**...║
        ║xx║xxxx║xx║xxxx║xxSEA║RCHED VI║IRTUAL TEXT║
        ╙──╨────╨──╨────╨──┼──╨────────╨───────────╜
        ┊─OPTION 'offset'─►┊

If the last segment is a C<text:tab> or C<text:newline> object 
then 'end' will be 1.
If the last segment is a C<text:s> (which can represents several
consecutive spaces), 'end' will be the number of spaces included in the match.

RETURNS:

In array context, zero or more hashrefs.

In scalar context, a hashref or undef if there was no match,
and croaks if there were multiple matches.

=cut

sub ODF::lpOD::Element::Hsearch {
  my $context = shift;
  my $expr    = shift;
  my %opts    = @{ &__canon_options_arg };

  my @matches;
  $context->Hreplace($expr, 
                     sub{ push @matches, $_[0]; return(REPL_CONTINUE) }, 
                     %opts);
  return @matches
    if wantarray;
  confess "void context, result would be discarded"
    unless defined wantarray;
  # scalar context
  croak "'$expr' matched ",scalar(@matches)," times\n" if @matches > 1;
  return @matches > 0 ? $matches[0] : undef;
}

=head2 $context->Hreplace($expr, [content], OPTIONS)

=head2 $context->Hreplace($expr, sub{...},  OPTIONS)

Search and replace. C<$expr> is a string or qr/regex/s as with C<Hsearch>.

In the first form, each matched substring in the virtual text is
replaced with C<[content]>.

In the second form, the specified sub is called for each match, passing
a I<match hashref> (see C<Hsearch>) as the only argument.

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

=head3 B<Content Specification>

The C<[content]> argument is a list of zero or more elements, 
each of which is either

=over

=item * A text string which may include spaces, tabs and newlines, or

=item * A reference to [list of format PROPs] 

=back

Each [list of format PROPs] describes a I<character style>
which will be applied only to the immediately-following text string.

Each PROP is itself either a [key => value] sublist,
or a shorcut string:

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

  [ [style-name => "name of style"] ]

=cut

sub ODF::lpOD::Element::Hreplace {
  my $context = shift;
  my $expr    = shift;
  my $repl    = shift;
  my %opts    = @{ &__canon_options_arg };

  my $offset = delete $opts{offset} // 0;
  my $multi  = delete $opts{multi};
  my $debug  = delete $opts{debug};
  croak "Invalid option ",avis(keys %opts) if %opts;

  my $regex = ref($expr) eq 'Regexp' ? $expr : qr/\Q${expr}\E/s;

  if (ref($repl) ne "CODE") {
    my $content = $repl;
    $repl = sub{ return($multi ? REPL_SUBST_CONTINUE : REPL_SUBST_STOP, 
                        $content); };
  }
                  
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
  my %seen;
  my $totlen_sofar = 0;
  PARA:
  foreach my $para ($context->descendants_or_self(qr/text:(p|h)/)) {
    btw dvis('##Hr START PARA $offset $totlen_sofar $regex $para')  if $debug;
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
        btw dvis '##Hr SKIP SEG: $offset $etext $skipped_chars $totlen_sofar' if $debug;
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
      btw ivis '##Hr SEG $seginfo[-1]', dvis '\n     $vtext $vtext_pos $offset $starting_offset $skipped_chars $totlen_sofar' if $debug;
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
          my $m = {
            match      => substr($vtext, $vtoffset, $vtend-$vtoffset),
            segments   => [ map{$_->{obj}} @seginfo[$fsi..$lsi] ],
            offset     => $vtoffset - $seginfo[$fsi]->{vtoff},
            end        => $vtend - $seginfo[$lsi]->{vtoff},
            voffset    => $starting_offset + $vtoffset,
            vend       => $starting_offset + $vtend,
            paragraph  => $para,
          };
          btw dvis '##Hr MATCH $skipped_chars $offset $totlen_sofar $vtext_pos $vtoffset $vtend\n',fmt_match($m) if $debug;
          my ($opcode, @args) = $repl->($m);
          btw dvis '##Hr $opcode @args' if $debug;
          confess "Invalid opcode returned from match sub:",vis($opcode)
            unless $opcode =~ /^REPL_(SUBST_)?(CONTINUE|STOP)$/;
          my ($dosubst, $continue_or_stop) = ($1,$2);
          if ($dosubst) {
            my $content = shift @args // confess "No [content] after $opcode";
            #############################################################
            my ($next_seg, $next_offset) 
              = Hreplace_match($m, $content, debug => $debug);
            #############################################################
            btw dvis '##Hr Hreplace_match -> $next_seg $next_offset' if $debug;
            if ($next_seg && (@input_segs==0 || $next_seg != $input_segs[0])) {
              oops if grep {$next_seg == $_} @input_segs; 
              push @input_segs, $next_seg;  # continue here
            }
            my $inserted_vlen = sum0(map{ref($_) ? 0 : length($_)} @$content);
            # $vtext still contains the old text; will continue if < theend.
            #my $delta = $inserted_vlen - ($m->{vend} - $m->{voffset});
            #$totlen_sofar += $delta;
            #$vtend += $delta;
            oops(dvis '$delta $totlen_sofar $vtend')  
              unless $vtend==0 || substr($vtext,$vtend-1,1) eq substr($m->{match},-1);

            while (@args) {
              if ($args[0] eq 'expr') { shift @args; $expr = shift @args; }
              else { confess "unknown arg for $opcode: @args" };
            }
          }
          if ($continue_or_stop eq "STOP") {
            return @args;
          }
          oops unless $continue_or_stop eq "CONTINUE";

          last PARA unless $multi;
          # ? IS THIS NECESSARY?  i.e. won't $offset small enough?
          $offset = $totlen_sofar - (length($vtext) - $vtend);
          if ($vtend < length($vtext)) {
            $vtext_pos = $vtend;
            redo CURR_SEG
          } # else start on next seg
        }#matched
        else { 
          btw dvis '[nomatch] $vtext_pos $regex $vtext' if $debug; 
        }
      }
      btw '##Hr advancing to next SEG.' if $debug;
    }#while input_segs remain in current paragraph
  }#PARA
}#Hreplace

# For the time being, Hreplace_match is not public

=for Pod::Coverage Hreplace_match

=cut

sub Hreplace_match($$@) {
  my $m = shift;
  my $content = shift;
  my %opts    = @{ &__canon_options_arg };

  my @segments = @{ $m->{segments} };
  my $seg0 = $segments[0];
  my $segL = $segments[-1];
  my $num_segs = @segments;
  my $residue_vtext = substr(__element2vtext($segL), $m->{end});
  my $residue_len = length($residue_vtext);

  # Copy any "after" residue from segL into a new successor segment
  my $residue_seg;
  if ($residue_len > 0) {
    if ($segL->is(TEXT_SEGMENT)) {
      $residue_seg = odf_create_element(TEXT_SEGMENT);
      $residue_seg->set_text($residue_vtext); # guaranteed flat
    }
    elsif ($segL->is("text:s")) {
      my $c = $segL->get_attribute('c') // 1;
      oops unless $c > $residue_len && $residue_vtext =~ /^\s+$/;
      $residue_seg = odf_create_element("text:s");
      $residue_seg->set_attribute('c', $residue_len);
    }
    else { oops }
    $residue_seg->paste_after($segL);
  }
  if ($m->{offset} > 0) {
    # seg0 contains "before" part to be saved: elide the rest of seg0's text
    if ($seg0->is(TEXT_SEGMENT)) {
      $seg0->set_text( substr(__element2vtext($seg0),0,$m->{offset}) );
    }
    elsif ($seg0->is("text:s")) {
      oops unless $seg0->get_attribute('c') > $m->{offset};
      $seg0->set_attribute('c', $m->{offset});
    }
    else { oops }
    shift @segments; # don't delete seg0
  }
  
  # N.B. This might merge whatever follows $segL out of existence, 
  # but AFAIK will not merge "backwards", so $segL will remain in existence
  $segL->Hinsert_content($content, 
                         position => NEXT_SIBLING, debug => $opts{debug});

  $_->delete() foreach (@segments);

  # If there was "after" residue, return a pointer to that
  ( $residue_seg ? ($residue_seg,0) : () )
}

#  # Possible situations:
#  #     First Seg                           Last Seg
#  # ┌─────┬──────┐    ┌────────────┐    ┌──────┬─────┐
#  # │  A  │xxxxxx│    │xxxxxxxxxxxx│    │xxxxxx│  Z  │
#  # └─────┴──────┘    └────────────┘    └──────┴─────┘
#  #
#  #     First Seg                           Last Seg
#  # ┌────────────┐    ┌────────────┐    ┌────────────┐
#  # │xxxxxxxxxxxx│    │xxxxxxxxxxxx│    │xxxxxxxxxxxx│
#  # └────────────┘    └────────────┘    └────────────┘
#  #
#  #                     Only Seg
#  #                 ┌─────┬──────┬─────┐
#  #                 │  A  │xxxxxx│  Z  │
#  #                 └─────┴──────┴─────┘


=head2 $context->Hinsert_content([content], OPTIONS)

This works like C<ODF::lpOD::Element::insert_element()> except 
the possibly-multiple segments to be inserted are described by 
a high-level C<[content]> specification (as described for C<Hreplace>).

The segment(s) actually inserted will include spans and the special 
ODF objects representing tabs, spaces and newlines as implied by
the characters in C<[content]>.

OPTIONS may contain:
  
  position => ...  # default is FIRST_CHILD

The new content is inserted at the indicated position relative to
C<$context>.  

If multiple segments are inserted, the first one
is will be at the indicated position and the others will be
immediately-following siblings of the first.

Returns nothing.

Note: No new segments are inserted if C<position> is C<WITHIN> 
and the content was entirely inside that segment.

=cut

##=head3 B<Spans>
##
##In ODF, local character styles are applied to text segments by storing the
##text segments as children of a B<span> object which specifies the style.
##
##C<Hinsert_content> will 'divide' existing spans into multiple copies
##if needed to allow newly inserted spans to exist at the top level.  
##
##For example, consider a paragraph which initially contains 4 text nodes, 
##with the middle two under a span:
##                        
##      |--Text1  
##      |         |-Text2
##  Para|----SPAN1|
##      |         |-Text3
##      |--Text4
##
##Now C<Hinsert_content> is called to insert after Text2, with
##a [content] parameter C<[ "Text2.3", ["bold"], "Text2.7" ]>.
##
##This means "Text2.3" will have the same formatting as it's predecessor
##("Text2"), but "Text2.7" will by covered by a new span which specifies
##a bold Style.  Therefore the existing SPAN1 is divided to make room
##for the new span at the top level:
##
##      |--Text1  
##      |        |--Text2
##      |--SPAN1a|--Text2.3
##      |
##  Para|--NEWSPAN--Text2.7
##      |
##      |--SPAN1b--Text3
##      |
##      |--Text4

=pod

Empty elements are deleted (or not inserted).

=cut

sub ODF::lpOD::Element::Hinsert_content($$) {
  my $context     = shift;
  my $content_arg = shift;
  confess "[content] must be an array ref" unless ref($content_arg) eq "ARRAY";
  my %opts = (
    position => FIRST_CHILD, 
    @{ &__canon_options_arg },
  );
  my @content = @$content_arg;
  my $debug = $opts{debug};

  my $show_cp = $opts{position} =~ /SIBLING/ ? $context->{parent} : undef;
  my sub show_context($) {
    my $msg = shift;
    if ($show_cp) {
      # The original context might have been merged out of existence by now
      $msg .= " context=".addrvis($context)." ORIG context->{parent}:\n"
                         .fmt_tree($show_cp);
    } else {
      $msg .= " context:\n". fmt_tree($context);
    }
    @_ = ($msg);
    goto &btw;  # show caller's line number
  }

show_context(dvis '##Hi TOP %opts\n     @content\n    ') if $debug;
  return if @content==0; # nothing to insert?

  my $root = $context->get_root;
  my $ins_context = $context;

  # The node first goes at the position specified by the user,
  # which might be WITHIN, i.e. splitting an existing text segment.
  # Subsequent nodes are inserted immediately after the first node.
  # After everyting is put in, Hnormalize_spans() will "promote" any
  # 2nd-level spans to the top level.
  my @tmp_paras; 
  while (@content) {
    local $_ = shift @content;
    my ($text_context, $text_opts);
    if (ref) {
      my $tprops = $_;
      my $stylename;
      if (@$tprops == 2 && $tprops->[0] =~ /^style[-_ ]name$/) {
        $stylename = $tprops->[1];
      } else {
        my $ts = automatic_style($root, 'text', @$tprops) // oops;
        $stylename = $ts->get_name;
      }
      my $vtext = shift(@content) 
        // croak "[style spec] not followed by anything";
      if (ref($vtext)) { croak "[style spec] not followed by plain text" }

      my $span = $ins_context->insert_element('text:span', %opts);
      $span->set_attribute('style-name', $stylename);
      # ODF::lpOD::TextElement::set_text replaces all children of a container
      # with PCDATA and tab, etc. nodes which in this case is what we want.
      $span->ODF::lpOD::TextElement::set_text($vtext);
show_context("##Hi BBB span=".addrvis($span)) if $debug;
      $ins_context = $span;
    } else {
      my $vtext = $_;
      # Put the new content into a dummy paragraph inserted exactly where
      # the content should end up (likely forming an invalid structure e.g.
      # paragraphs). Later the new children will be moved out and the dummy
      # paragraphs deleted, but that is deferred to avoid dealing with merged
      # text nodes in the middle of processing.
      my $tmp_para = $ins_context->insert_element('text:p', %opts);
      $tmp_para->ODF::lpOD::TextElement::set_text($vtext);
      push @tmp_paras, $tmp_para;
show_context("##Hi CCC tmp_para=".addrvis($tmp_para)) if $debug;
      $ins_context = $tmp_para;
    }
    $opts{position} = NEXT_SIBLING;
    delete @opts{qw/after before offset/};
  }
  # Move children out of the temporary paragraphs and delete the temp paras.
  # NOTE this merges adjacent #PCDATA nodes, possibly re-combining an
  # originally-split ancestor.  WE DONT KNOW WHERE THE NEW DATA WILL END UP.
  while (my $tmp_para = shift @tmp_paras) {
    foreach my $node ( $tmp_para->cut_children() ) {
      $node->paste('before', $tmp_para);
    }
    $tmp_para->delete();
  }

  #warn "TODO: normalize_spans\n";
  #  ACTUALLY maybe nested spans are okay, if styles can "not specify"
  #  some attributes, i.e. to inherit part of an enclosing span's style.
  #...maybe should do this before removing temp paragraphs?
  #...we would need to look inside the temp paras to see sub-spans

show_context("##Hi FINAL") if $debug;

  confess "Hinsert_content does not return a value"
    if defined(wantarray);
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

C<$family> is "text" or another style family name (TODO: specify)

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

  my $sh = _get_per_doc_hash($doc);
  my $style_caches = ($sh->{style_caches} //= {});
  my $counters     = ($sh->{counters}     //= {});

  my $cache = ($style_caches->{$family} //= {});
  my $cache_key = hashtostring(\%props);
  my $stylename = $$cache{$cache_key};
  if (! defined $stylename) {
    for (;;) {
      $stylename = $auto_pfx.uc(substr($family,0,1)).(++$counters->{$family});
      # Append something to remind us what style this is while debugging
      # (the counter guarantees unique results)
      foreach my $key (qw/align weight style variant size/) {
        $stylename .= "_".$props{$key} if $props{$key}
      }
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
  my ($node, $leaftextonly) = @_;
  if (! ref $node) {
    return "(invalid node: ".vis($node).")";
  }
  my $text = eval{ $node->get_text }; # (resursivly gets full virtual text)
  if (defined($text)) {               
    $text = ODF::lpOD::Common::input_conversion($text);#undo implicit encode
  }
  my $tag  = eval{ $node->tag };
  my $att  = eval{ $node->get_attributes };
  ref($node) =~ /ODF::lpOD::(\w+)/;
  my $class = $1 // ref($node) || confess("not a ref");
  my $s = "$class<".addrvis(refaddr $node).">";
  $s .=  " $tag" if defined $tag;
  $s .= " ".(%$att && $tag =~ /^(table-cell|sequence)/ ? "{...}" : vis($att))
    if keys %$att;

  $s .= " ".vis($text)."[len=".length($text)."]"
    if defined($text) && (!$leaftextonly  # ?? what is text:box (?a mistake?)
                            || $tag !~ /^text:(box|span|text|p|h)|^office:/);

  foreach my $k (sort keys %$node) { # any private members e.g. from XML::Twig?
    next if any{ $k eq $_ } qw/pcdata att gi parent first_child last_child 
                               prev_sibling next_sibling/;
    $s .= " $k=".visq($node->{$k});
  }
  return $s;
}
sub _fmt_tree($$$);
sub _fmt_tree($$$) {
  my ($obj,$indent,$sref) = @_;
  $indent //= 0;
  $$sref .= " "x$indent.fmt_node($obj,1)."\n";
  return unless ref $obj;
  foreach my $e ($obj->children) {
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
  return "undef" unless defined $href;
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
I<< lpod->Huse_octet_strings() >> or
I<< lpod->set_input/output_charset() >> (see C<ODF::lpOD::Common>)
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

=for Pod::Coverage oops btw 

=cut

1;
