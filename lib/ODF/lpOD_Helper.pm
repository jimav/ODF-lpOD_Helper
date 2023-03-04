# $Id: lpOD_Helper.pm,v 1.13 2022/07/15 20:05:51 jima Exp jima $
# -----------------------------------------------------------------------------
# Copyright © Jim Avera 2012-2023
# License: Public Domain or CC0
# See https://creativecommons.org/publicdomain/zero/1.0/
# The author, Jim Avera (jim.avera at gmail) has waived all copyright and
# related or neighboring rights.  Attribution is requested but is not required.
# -----------------------------------------------------------------------------
use strict; use warnings FATAL => 'all'; use feature qw(switch state say);

=encoding utf8

=head1 NAME

ODF::lpOD_Helper - ease-of-use wrapper for ODF::lpOD

=head1 SYNOPSIS

  use ODF::LpOD;
  use ODF::LpOD_Helper;

  Sorry, no examples yet... TODO TODO FIXME

  The following APIs are exported by default:

    my_search -- find a possibly-segmented string
    subst_content -- find and replace strings
    fmt_match fmt_node fmt_tree -- debug utilities for "match" data structures
    self_or_parent
    gen_table_name
    automatic_style
    common_style

=head1 DESCRIPTION

ODF::lpOD_Helper provides easier text search and replace
(or insert) while intelligently handling strings which may span multiple
segments, may contain newlines, tabs, or multiple spaces, and may contain
non-ASCII characters.

It turns out that ODF::lpOD by itself is not convenient because:

=over

=item 1.

C<ODF::lpOD> requires text to be passed as encoded binary octets,
rather than as Perl characters.

=item 2.

I<Search> can not match strings which span multiple segments, such as
those created automatically by LibreOffice to support
it's "record changes" function.

=item 3.

I<Search> can not match strings containing newlines, tabs, or repeated spaces,
nor can strings containing those things be easily inserted.

=back

C<ODF::lpOD_Helper>
also works around a bug which causes
"Unknown method DESTROY" warnings
(see https://rt.cpan.org/Public/Bug/Display.html?id=97977)

=head1 PRIMARY FUNCTIONS

Note: This documentation was written several years after the code;
a few uncertain details are denoted by "??" in the description.


=head2 my_search($context, $expr)

Locates all occurences of C<$expr> in the specified C<$context>, returning
a match hash for each.

Somewhat like C<ODF::lpOD::Element::search> but each match may span
multiple segments and may contain consecutive spaces, tabs, and/or newlines.
C<< $expr >> is a string containing charcters, not octets.

RETURNS: In list context, 0 or more match hashrefs;
Otherwise, 1 match hashref or dies if there was not exactly one match

Each match hash contains:

  {
    match           => The matched text
    offset          => Offset in the virtual string (including \t etc.)
    segments        => [ list of text nodes containing the match ]
    fseg_offset     => offset into the first node of the start of match
    lseg_end_offset => offset of end+1 of the match in the last node
  }

=head2 subst_content($context, [OPTIONS], @content)

Change some or all of the text in an object, optionally inserting new
character style spans.  Elements which end up with empty text are removed.

The 1st arg is the context of the text, either a leaf (#PCDATA) or a
container or an ancestor (paragraph, table cell, or even the document body).
Leaves are always deleted and possibly replaced with other nodes.

The 2nd arg is an array ref [list of options] which may contain:

   Chomp => TRUE
     Remove any trailing newline from the last new text string

   Search => string or qr/regex/
     Find an existing string and replace it, preserving existing spans.
     The string must be contained within one paragraph but may be segmented.

   Without 'Search' the entire text content is replaced (??).

The remaining arguments specify the new content, as a list of
text strings possibly interspersed with [PROP,...] lists.  Please see the
SYNOPSIS for example.

Each string may include consecutive spaces, tabs, and newlines.
Each [PROP,...] list defines a new "automatic" character style which
will be applied only to the immediately-following text string.

A PROP is either a [key => value] sublist,
for example [color => "red"], or one of the following
single-word abbreviations:

  "center"      means  [align => "center"]
  "left"        means  [align => "left"]
  "right"       means  [align => "right"]
  "bold"        means  [weight => "bold"]
  "italic"      means  [style => "italic"]
  "oblique"     means  [style => "oblique"]
  "normal"      means  [style => "normal", weight => "normal"]
  "roman"       means  [style => "normal"]
  "small-caps"  means  [variant => "small-caps"]
  "normal-caps" means  [variant => "normal"], #?

  <NUM>         means  [size => "<NUM>pt],   # bare number means point size
  "<NUM>pt"     means  [size => "<NUM>pt],
  "<NUM>%"      means  [size => "<NUM>%],

Fonts are automatically registered.

Note: Strings contain charcters, not octets.

RETURNS:
  In list context:   A list of 0 or more match hashrefs (see my_search)
  In scalar context: A match hashref; dies if there was not exactly one match
  In null context:   Nothing, but dies if there was not exactly one match

Note: This is conceptually like a combination of C<ODF::lpOD's search()>
and C<replace()> or C<set_text()>, and C<set_span()>,
except that those APIs do not support segmented text and/or consecutive
spaces, tabs or newlines when searching or inserting character styles,
use encoded octet streams in their APIs instead of characters,
and can can not generally be invoked directly on a leaf node.

=head2 insert_content($parent, $prev_sibling, @content)

Insert new content using multiple nodes.  The new node(s) are inserted
after C<$prev_sibling> if it is defined, otherwise as
new first children of C<$parent>.

@content is a list of strings and [PROP...] styles as described
for C<subst_content()>.

Nothing is inserted if all text is "".

RETURNS: List of node(s) inserted


=head1 MISC. UTILITIES

=head2 self_or_parent($node, $tag)

Return $node or it's nearest ancestor which matches a gi

Currently this throws an exception if neither $node or an ancestor
matches $tag.

=head2 automatic_style($context, $family, PROP...)

Find or create an 'automatic' (i.e. functionally anonymous) style and
return the object.  Styles are re-used when possible, so a style should
not be modified because it might be shared.

PROPs are as described for C<subst_content>.

=head2 common_style($context, $family, PROP...)

Create a 'common' (i.e. named by the user) style.

The name must be given by [name => "STYLENAME"] somewhere in PROPs.

=head2 gen_table_name($context)

Generate a new (not currently used) table name of the form "Table<NUM>".

=head2 fmt_node($node)

Format a single node for debug messages.
Returns a string without a final newline.

=head2 fmt_tree($top)

Format a node and all of it's children, for debug messages.

=head2 fmt_match($matchhash)

Format a match hash (ref) for debug messages.

=head1 BUGS

Only one document can ever be processed (per run) because of the use
of global state, namely the cache of automatic style objects
and information used to construct unique names.

This may someday be fixed by keeping separate state for each
unique value of C<< $context->get_document() >>.


=head1 HISTORY

ODF::lpOD_Helper was first written in 2012 when ODF::lpOD was most likely
at v1.118.  At that time the author of ODF::lpOD was not available or
could not substantively respond to the problem of generalized
text search over segmented strings and arbitrary white-space.

As of this writing (Feb 2023), the latest release of ODF::lpOD 
is v1.126, released in 2014.

=head1 AUTHOR / LICENSE

 Jim Avera  (jim.avera AT gmail dot com)

 License for ODF::lpOD_Helper : Public Domain or CC0
 See https://creativecommons.org/publicdomain/zero/1.0/

=cut


package ODF::lpOD_Helper;

# VERSION
# DATE

use Exporter 'import';
our @EXPORT = qw(
  my_search
  subst_content
  disconnected_style
  automatic_style common_style
  self_or_parent
  fmt_match fmt_node fmt_tree
  gen_table_name
);
our @EXPORT_OK = qw(
  my_get_text_func
  insert_content
  unabbrev_props
  hashtostring
  $auto_pfx
);

our $auto_pfx = "auto";  # used for style & table names

use Carp;
sub oops(@) { unshift @_, "oops! "; goto &Carp::confess; }
use ODF::lpOD;
BEGIN {
  # https://rt.cpan.org/Public/Bug/Display.html?id=97977
  no warnings 'once';
  no strict 'refs';
  *{"ODF::lpOD::Element::DESTROY"} = sub {}
    unless defined &ODF::lpOD::Element::DESTROY
}
use Data::Dumper::Interp;

sub fmt_node($;$) {  # for debug prints
  my ($node,$tailtextonly) = @_;
  if (! ref $node) {
    return "(invalid node: ".vis($node).")";
  }
  my $text = eval{ $node->get_text };
  my $tag  = eval{ $node->tag };
  my $att  = eval{ $node->get_attributes };
  my $s = "$node";
  $s =~ s/ODF::lpOD:://;
  $s =~ s/=HASH//;   # ref($node)
  $s .=  " $tag" if defined $tag;
  $s .= " ".(%$att && $tag =~ /^(table-cell|sequence)/ ? "{...}" : vis($att))
    if defined $att;
  $s .= " text=".vis($text)
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
sub fmt_tree($@) {
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
  return "------------\n".$string."------------\n";
}

# Result is the node or it's nearest ancestor which matches a gi
sub self_or_parent($$) {
  my ($node, $tag) = @_;
  my $e = $node->passes($tag) ? $node : $node->parent($tag);
  # Should we return undef instead of croaking??
  croak "Neither node nor ancestors match ",vis($tag),"\n" unless $e;
  return $e;
}

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
sub unabbrev_props($) {
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
sub disconnected_style($$@) {
  my ($context, $family, @input_props) = @_;
  my %props = @{ unabbrev_props(\@input_props) };

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

sub hashtostring($) {
  my $href = shift;
  return join("!", map{ "$_=>$href->{$_}" } sort keys %$href);
}

# Create an automatic style and return the object, re-using styles when
# possible (the style should not be modified because it might be shared).
# Many single-word abbreviations are accepted for paragraph and text styles.
sub automatic_style($$@);
sub automatic_style($$@) {
  my ($context, $family, @input_props) = @_;
  my $doc = $context->get_document // oops;
  my %props = @{ unabbrev_props(\@input_props) };

  state %style_caches;  # family => { digest_of_props => stylename }
  state %counters;
  my $cache = ($style_caches{$family} //= {});
  my $cache_key = hashtostring(\%props);
  my $stylename = $$cache{$cache_key};
  if (! defined $stylename) {
    for (;;) {
      $stylename = $auto_pfx.uc(substr($family,0,1)).(++$counters{$family});
      last
        unless defined (my $s=$doc->get_style($family, $stylename));
      my $existing_key = hashtostring(scalar $s->get_properties);
      $$cache{$existing_key} //= $stylename;
    }
    $$cache{$cache_key} = $stylename;
    my $object = disconnected_style($context,$family, %props, name=>$stylename);
    return $doc->insert_style($object, automatic => TRUE);
  } else {
    return $doc->get_style($family, $stylename);
  }
}

sub common_style($$@) {
  my ($context, $family, @input_props) = @_;
  my %props = @{ unabbrev_props(\@input_props) };
  croak "common_style must specify 'name'\n" unless $props{name};
  my $object = disconnected_style($context, $family, %props);
  return $context->get_document->insert_style($object, automatic => FALSE);
}

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

# Like ODF::lpOD get_text methods except:
#   Tab, line break and space objects are translated
#   The result is a normal Perl string, not octets.
# (this is a function, not a method)

# FIXME: The main purpose of this is return the text as Perl characters
# rather than octets, but it behaves differently
# than e.g. ODF::lpOD::TextElement::get_text in another way: It looks only
# at the top node, not it's children.
# Why can't this just be
#    return ODF::lpOD::Common::input_conversion( $node->get_text() );
# ???

sub my_get_text_func($) {
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
  else
          {
          $text = ODF::lpOD::Common::input_conversion( $node->get_text() );
          }
  return $text;
}

# Insert new content using multiple nodes.  The new node(s) are inserted
# after $prev_sibling if it is provided and not undef, otherwise as
# new first children of $parent.  Nothing is inserted if all text is "".
# Strings to be stored are normal Perl character strings, not octets.
# RETURNS: List of node(s) inserted
sub insert_content($$@) {
  my ($parent, $prev_sib) = (shift, shift);
  my $root = ($parent//$prev_sib)->get_root;

  # Ignore extraneous initial ""
  while (@_ and ! ref $_[0] and $_[0] eq "") { shift }

  my @nodes;
  while (@_) {
    local $_ = shift;
    if (ref) {
      my $tprops = $_;
      my $text = shift
        // croak "[text style] not followed by a string\n";
      ! ref($text)
        // croak "consecutive [text style]s (no string inbetween)\n";

      my $tsname;
      if (@$tprops == 2 && $tprops->[0] =~ /^style[-_ ]name$/) {
        $tsname = $tprops->[1];
      } else {
        my $ts = automatic_style($root, 'text', @$tprops) // oops;
        $tsname = $ts->get_name;
      }

      my $node = ODF::lpOD::Element->create('text:span')->set_class;
      $node->set_attributes({style => $tsname});
      $node->set_text( ODF::lpOD::Common::output_conversion($text) );
      if (defined $prev_sib) {
        $prev_sib->insert_element($node, position => NEXT_SIBLING);
      } else {
        $parent->insert_element($node, position => FIRST_CHILD);
      }
      $prev_sib = $node // oops;
      push @nodes, $node;
    } else {
      while (@_ && ! ref $_[0]) {
        $_ .= shift;  # combine adjacent texts
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
          $node->set_text( ODF::lpOD::Common::output_conversion($1) );
        }
        else { oops }
        if (defined $prev_sib) {
          $prev_sib->insert_element($node, position => NEXT_SIBLING);
        } else {
          $parent->insert_element($node, position => FIRST_CHILD);
        }
        oops unless $node;
        $prev_sib = $node;
        push @nodes, $node;
      }
    }
  }

  return @nodes;
}

sub fmt_match($) { # for debugging
  my $href = shift;
  my %r = %$href;
  my @segments = map { "$_ with text ".vis(my_get_text_func($_)) } @{$r{segments}};
  delete $r{segments};
  local $_ = ivis('%r'); s/\)$// or oops;
  return $_.", segments => [".join("\n   ",@segments)."])";
}

# Change some or all of the text in an object, optionally inserting new
# character style spans.  Elements which end up with empty text are removed.
#
# Strings are normal Perl possibly-wide characters, not octets.
# (This is unlike ODF::lpOD which gets/expects encoded octet streams
# using a particular encoding)
#
# The 1st arg is the context of the text, either a leaf (#PCDATA) or a
# container or an ancestor (paragraph, table cell, or even the document body).
# Leaves are always deleted and possibly replaced with other nodes.
#
# The 2nd arg is an array ref [list of options] which may contain:
#    Chomp => TRUE
#      Remove any trailing newline from the last new text string
#    Search => string or qr/regex/
#      Find an existing string and replace it, preserving existing spans.
#      The string must be contained within one paragraph but may be segmented.
#    Search_only => TRUE
#      Used by my_search()
#
# Remaining arguments define the new content with a combination of
# [span prop list]s and text strings.  Each [span prop list] defines an
# overriding character style which applies only to the immediately-following
# text string.  The strings may include consecutive spaces, tabs, and newlines.
#
# RETURNS:
#   In list context:   A list of 0 or more match hashrefs (see my_search)
#   In scalar context: A match hashref; dies if there was not exactly one match
#   In null context:   Nothing, but dies if there was not exactly one match
#
# Note: This is conceptually like a combination of ODF::lpOD's search()
# and replace() or set_text(), and set_span(), except that those APIs do not
# support segmented text and/or consecutive spaces, tabs or newlines
# when searching or inserting character styles, and use encoded octet
# streams in their APIs instead of normal Perl character strings,
# and can can not generally be invoked directly on a leaf node.
#
sub subst_content($$@) {
  my ($context, $opts_aref, @content) = @_;
  my %opts    = @$opts_aref;
  oops unless ref($context);
  while (@content and !ref($content[$#content]) and $content[$#content] eq "") {
    pop @content;
  }
  if ($opts{Chomp} and @content and !ref $content[$#content]) {
    $content[$#content] =~ s/\n\z//s;
  }

  my $Chomp        = delete $opts{Chomp};
  my $Search_only  = delete $opts{Search_only};
  my $Search       = delete $opts{Search} // $Search_only // qr/.+/s;
  my $Starting_pos = delete $opts{Starting_pos};
  my $debug        = delete $opts{debug};
  # my $Onceonly    = delete $opts{Onceonly}; # future
  croak "Invalid opt ",avis(keys %opts) if %opts;

  $Search = qr/\Q${Search}\E/s unless ref($Search) eq 'Regexp';

my $show = $debug || 0; #("@content" =~ /NOTES|OTHER.*INFO/);

  my @rlist;
  # N.B. Paragraphs can be indirectly nested (via frames), so we
  # need to avoid visiting the same leaf text node more than once.
  # TODO: Re-write this to explicitly traverse the tree using XML::Twig
  #   and directly visit text nodes only once.
  my %seen_text_nodes;
  PARA:
  foreach my $para ($context->descendants_or_self(qr/text:(p|h)/)) {
    my @segments;
    my $oldtext = "";
    my @elements
      = $para->descendants_or_self(qr/^(#PCDATA|text:tab|text:line-break|text:s)$/);
    foreach my $e (@elements) {
      next if $seen_text_nodes{$e}; # incremented below after possible re-visit
      my $etext = my_get_text_func($e);
      push @segments, { obj => $e,
                        offset => length($oldtext),
                        length => length($etext),
                        ix     => scalar(@segments),
                      };
      $oldtext .= $etext;
    }
    next unless @segments;

    my $prev_repl;

    if (defined $Starting_pos) {
      oops if $Starting_pos > length($oldtext);
      pos($oldtext) = $Starting_pos;
    }
    while ($oldtext =~ /\G.*?(${Search})/mgsc) {
      my $start_off = $-[1];
      my $end_off   = $+[1];
      my ($fsi, $lsi);
      foreach (@segments) {
        $fsi = $_->{ix} if (! defined $fsi) &&
          $_->{offset} <= $start_off && $start_off < $_->{offset}+$_->{length};
        $lsi = $_->{ix} if (! defined $lsi) &&
          $_->{offset} < $end_off && $end_off <= $_->{offset}+$_->{length};
      }
      oops unless defined $fsi and defined $lsi;
      my %r = (
               match           => $1,
               offset          => $start_off,
               segments        => [ map{$_->{obj}} @segments[$fsi..$lsi] ],
               fseg_offset     => $start_off - $segments[$fsi]->{offset},
               lseg_end_offset => $end_off - $segments[$lsi]->{offset},
              );
      unless ($Search_only) {
        if ($prev_repl) {
          # This is the second match within a paragraph...
          # @segments may have become invalid when we inserted replacement
          # content for the previous match; so re-get the text anew and
          # search again, starting immediately after the replaced content
          # from the previous match.
          my $repl_length = 0;
          foreach(@content) { $repl_length += length($_) if ! ref }
          $Starting_pos = $rlist[$#rlist]->{offset} + $repl_length;
          redo PARA;
        }
        # Insert all the new content (possibly multiple nodes) in place of
        # the segment containing the start of the match, and delete the
        # other segments as well.
        # Anything before or after the match in the deleted segments is
        # saved and put back with the new content.
        my $before = substr($oldtext,
                            $segments[$fsi]->{offset},
                            $r{fseg_offset});
        my $after  = substr($oldtext,
                            $end_off,
                            $segments[$lsi]->{length} - $r{lseg_end_offset});

if ($show) {
  warn dvis('### BEFORE: $Search @content $fsi $lsi $start_off $end_off $before $after $oldtext\n');
  for my $i (0..$#segments) {
    my %h = %{ $segments[$i] };
    $h{obj} = fmt_node($h{obj});
    warn ivisq "     segments[$i] = \%h\n";
  }
}
        $r{segments} = [
            insert_content(undef, $segments[$fsi]->{obj},
                           $before,
                           @content,
                           $after
                          )
        ];
        for my $i (reverse $fsi .. $lsi) {
          $segments[$i]->{obj}->delete;
        }
        if (@{$r{segments}} == 0) {
          # No content - the node was deleted with no replacement.
          # Return the containing paragraph so the caller can use it to
          # find the context, e.g. a containing table cell.
          #
          # However if the paragraph is now completely empty, delete it
          # too and return *its* parent.
          if (($para->get_text//"") eq "") {
            @{$r{segments}} = ( $para->parent // oops );
            $para->delete;
          } else {
            @{$r{segments}} = ( $para // oops );
          }
        }
if ($show) {
  warn "### AFTER :",
  map{ " segment: ".fmt_node($_)."\n" } @{ $r{segments} };
}
        $prev_repl = 1;
      }
      push @rlist, \%r;
    }
    foreach my $e (@elements) { ++$seen_text_nodes{$e} }
  }

#  foreach (@rlist) {
#    my %r = %$_; delete $r{segments};
#    warn ivis('  RET=%r @content\n'),
#         map{ "      segment: ".fmt_node($_)."\n" } @{$rlist[0]->{segments}};
#  }

  return @rlist if wantarray;
  croak "'$Search' matched ",scalar(@rlist)," times\n" unless @rlist==1;

  return $rlist[0];
}

# Similar to ODF::lpOD::Element::search but works with texts which span
# segments and may contain consecutive spaces, tabs, and/or newlines.
# In list context, returns a list of 0 or more match hashrefs;
# otherwise returns 1 match hashref or dies if not exactly one match
#
# Each match hash contains:
# {
#   match           => The matched text
#   offset          => Offset in the virtual string (including \t etc.)
#   segments        => [ list of text nodes containing the match ]
#   fseg_offset     => offset into the first node of the start of match
#   lseg_end_offset => offset of end+1 of the match in the last node
# }
sub my_search($$;@) {
  my ($context, $expr, @opts) = @_;
  return subst_content($context, [ @opts, Search_only=>$expr ]);
}

1;
