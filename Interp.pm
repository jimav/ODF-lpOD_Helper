# Copyright © Jim Avera 2012-2022.  This document contains code snippets
# from perl5db.pl and Data::Dumper as noted in adjacent comments, and
# those extracts remain subject to the licenses of their respective sources.
# Excepting those portions, this file has been dedicated to the Public Domain 
# per Creative Commons CC0 (http://creativecommons.org/publicdomain/zero/1.0/)
use strict; use warnings FATAL => 'all'; use utf8; use 5.012;
use feature qw(switch state);

package DB;
sub DB_Vis_Evalwrapper { # Must appear before any variables are declared
  eval $Vis::string_to_eval;
}

package Vis;
# POD documentation follows __END__

use version 0.77; our $VERSION = version->declare(sprintf "v%s", q$Revision: 2.7 $ =~ /(\d[.\d]+)/);

require Data::Dumper;
use Carp;
use POSIX qw(INT_MAX);
use Encode ();
use Scalar::Util qw(blessed reftype refaddr looks_like_number);
use List::Util qw(min max first any);
use Regexp::Common qw/RE_balanced/;
use Term::ReadKey qw(GetTerminalSize);
use overload ();

sub debugvis(_) {  # for our internal debugging messages
  my $s = Data::Dumper->new([shift])->Useqq(1)->Terse(1)->Indent(0)->Dump;
  chomp $s;
  $s
}
sub debugavis(@) { "(" . join(", ", map{debugvis} @_) . ")" }
sub oops(@) { @_ = ("\noops:",@_,"\n  "); goto &Carp::confess }

use Exporter 'import';
our @EXPORT    = qw(vis  avis  lvis  svis  dvis  hvis  hlvis
                    visq avisq lvisq svisq dvisq hvisq hlvisq
                    u qsh forceqsh qshpath);

our @EXPORT_OK = qw($Maxwidth $MaxStringwidth $Truncsuffix $Debug
                    $Stringify
                    $Useqq $Quotekeys $Sortkeys $Terse $Indent $Sparseseen);

our @ISA       = ('Data::Dumper'); # see comments at new()

our ($Debug, $MaxStringwidth, $Truncsuffix, $Stringify,
     $Maxwidth, $Maxwidth1,
     $Useqq, $Quotekeys, $Sortkeys, $Terse, $Indent, $Sparseseen);

$Debug          = 0            unless defined $Debug;
$MaxStringwidth = 0            unless defined $MaxStringwidth;
$Truncsuffix    = "..."        unless defined $Truncsuffix;
$Stringify      = 1            unless defined $Stringify;
$Maxwidth       = undef        unless defined $Maxwidth;  # undef to auto-detect
$Maxwidth1      = undef        unless defined $Maxwidth1; # override for 1st

# The following Vis defaults override Data::Dumper defaults
$Useqq          = 1            unless defined $Useqq;
$Quotekeys      = 0            unless defined $Quotekeys;
$Sortkeys       = \&__sortkeys unless defined $Sortkeys;
$Terse          = 1            unless defined $Terse;
$Indent         = 1            unless defined $Indent;
$Sparseseen     = 1            unless defined $Sparseseen;

sub Debug {
  my($s, $v) = @_;
  @_ == 2 ? (($s->{VisDebug} = $v), return $s) : $s->{VisDebug};
}
sub MaxStringwidth {
  my($s, $v) = @_;
  @_ == 2 ? (($s->{MaxStringwidth} = $v), return $s) : $s->{MaxStringwidth};
}
sub Maxwidth {
  my($s, $v, $v1) = @_;
  return(wantarray ? ($s->{Maxwidth}, $s->{Maxwidth1}) : $s->{Maxwidth})
    if @_ == 1;
  $s->{Maxwidth} = $v; 
  $s->{Maxwidth1} = $v1 if @_==3;
  $s
}
sub Maxwidth1 {  # experimental
  my($s, $v) = @_;
  @_ == 2 ? (($s->{Maxwidth1} = $v), return $s) : $s->{Maxwidth1};
}
sub Truncsuffix {
  my($s, $v) = @_;
  @_ == 2 ? (($s->{Truncsuffix} = $v), return $s) : $s->{Truncsuffix};
}
sub Stringify {
  my($s, $v) = @_;
  @_ == 2 ? (($s->{Stringify} = $v), return $s) : $s->{Stringify};
}
sub VisType { # NOT currently documented for direct use by users
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{VisType} = $v), return $s) : $s->{VisType};
}

############### Functional (non-oo) APIs #################

sub u(_) { $_[0] // "undef" }
sub forceqsh(_) {
  # Unlike Perl, /bin/sh does not recognize any backslash escapes in '...'
  local $_ = shift;
  return "undef" if !defined;
  $_ = vis($_) if ref; 
  # Prefer "double quoted" if no shell escapes would be needed
  if (/["\$`!\\\x{00}-\x{1F}\x{7F}]/) {
    s/'/'\\''/g; # foo'bar => foo'\''bar
    return "'${_}'";
  } else {
    return "\"${_}\"";
  }
}
sub qsh(_) {
  local $_ = shift;
  defined && !/[^-=\w_\/:\.,]/ && $_ ne "" && !ref ? $_ : forceqsh
}
sub qshpath(_) {  # like qsh but does not quote initial ~ or ~username
  local $_ = shift;
  return qsh if !defined or ref;
  my ($tilde_prefix, $rest) = /^( (?:\~[^\/\\]*[\/\\]?+)? )(.*)/xs or die;
  $rest eq "" ? $tilde_prefix : $tilde_prefix.qsh($rest)
}

########### Functional/OO APIs #############

sub __getobj {
  (blessed($_[0]) && $_[0]->isa(__PACKAGE__) ? shift : __PACKAGE__->new())
}
sub __getobj_s { &__getobj->Values([$_[0]]) }
sub __getobj_a { &__getobj->Values([\@_])   } #->Values([[@_]])
sub __getobj_h {
  my $o = &__getobj;
  (scalar(@_) % 2)==0 or croak "Uneven number args for hash key => val pairs";
  $o ->Values([{@_}])
}

# These can be called as *FUNCTIONS* or as *METHODS* of a Vis object
sub vis(_)    { &__getobj_s ->VisType('s' )->Dump; }
sub visq(_)   { &__getobj_s ->VisType('s' )->Useqq(0)->Dump; }
sub avis(@)   { &__getobj_a ->VisType('a' )->Dump; }
sub avisq(@)  { &__getobj_a ->VisType('a' )->Useqq(0)->Dump; }
sub lvis(@)   { &__getobj_a ->VisType('l' )->Dump; }
sub lvisq(@)  { &__getobj_a ->VisType('l' )->Useqq(0)->Dump; }
sub hvis(@)   { &__getobj_h ->VisType('h' )->Dump; }
sub hvisq(@)  { &__getobj_h ->VisType('h' )->Useqq(0)->Dump; }
sub hlvis(@)  { &__getobj_h ->VisType('hl')->Dump; }
sub hlvisq(@) { &__getobj_h ->VisType('hl')->Useqq(0)->Dump; }

# Trampolines which replace the call frame with a call directly to the
# interpolation code which uses package DB to access the user's context.
sub svis(_) { @_=(&__getobj,          shift,'s');goto &DB::DB_Vis_Interpolate }
sub svisq(_){ @_=(&__getobj->Useqq(0),shift,'s');goto &DB::DB_Vis_Interpolate }
sub dvis(_) { @_=(&__getobj,          shift,'d');goto &DB::DB_Vis_Interpolate }
sub dvisq(_){ @_=(&__getobj->Useqq(0),shift,'d');goto &DB::DB_Vis_Interpolate }

# Our new() takes no parameters and returns a default-initialized object,
# on which option-setting methods may be called and finally "vis", "avis", etc.
# as a method to produce the output (those routines can also be called as
# functions, in which case they create a new object internally).
#
# An earlier version of this package was a true drop-in replacement for 
# Data::Dumper and supported all of the same APIs (mostly by inheritance) 
# including Data::Dumper's new([values],[names]) constructor.
# Vis extensions were accessed via differently-named alternative constructors.
#
# Now Vis is no longer API compatible with Data::Dumper, but uses the same
# option-setting paradigm where methods like Maxwidth() modify the object
# if called with arguments while returning the object to allow method chaining.
#
# However there remains a wart: Vis still derives from Data::Dumper and
# a few Data::Dumper options may reasonably be called by users (Quotekeys, 
# Sortkeys, Useqq, and maybe Sparseseen).  This is fine as far as method
# calls are concerned, but the default values for those options come from
# globals in package Data::Dumper, not Vis.
#
# To fix this, we should implement all user-settable options ourself 
# (with defaults coming from globals in package Vis).  In that case
# a Data::Dumper object can be stored internally ("has a" relationship) 
# rather than deriving.  But that is a project for another day.
sub new {
  croak "No args allowed for Vis::new" if @_ > 1;
  my ($class) = @_;
  (bless $class->SUPER::new([],[]), $class)->_config_defaults()
}

############# only internals follow ############
sub _config_defaults {
  my $self = shift;

  if (! defined $Maxwidth) {
    if (u($ENV{COLUMNS}) =~ /^[1-9]\d*$/) {
      $Maxwidth = $ENV{COLUMNS}; # overrides actual terminal width
      say "Default Maxwidth=$Maxwidth from ENV{COLUMNS}" if $Debug;
    } else {
      local *_; # Try to avoid clobbering special filehandle "_"
      # Does not yet work, see https://github.com/Perl/perl5/issues/19142
      my ($width, $height) = GetTerminalSize(
        -t STDERR ? *STDERR : -t STDOUT ? *STDOUT 
        : do{my $fh; for("/dev/tty",'CONOUT$') { last if open $fh, $_ } $fh}
      );
      if (($Maxwidth = $width)) {
        say "Default Maxwidth=$Maxwidth from Term::ReadKey" if $Debug;
      } else {
        $Maxwidth = 80;
        say "Maxwidth=$Maxwidth from hard-coded backup default" if $Debug;
      }
    }
  }

  $self
    ->Debug($Debug)
    ->MaxStringwidth($MaxStringwidth)
    ->Maxwidth($Maxwidth, $Maxwidth1)
    ->Stringify($Stringify)
    ->Truncsuffix($Truncsuffix)
    # The following are Data::Dumper methods callable by Vis users
    ->Quotekeys($Quotekeys)
    ->Sortkeys($Sortkeys)
    ->Sparseseen($Sparseseen)
    ->Useqq($Useqq)
    # The following Data::Dumper options should never by changed by users 
    ->Terse($Terse)
    ->Indent($Indent)
}

my $unique = refaddr \&new;
my $magic_num_prefix    = "<NUMMagic$unique>";
my $magic_numstr_prefix = "<NUMSTRMagic$unique>";

sub __walk_worker($$$$$) {
  my (undef, $detection_pass, $stringify, $maxstringwidth, $truncsuf) = @_;
  return 1
    unless defined $_[0];
  # Truncate over-length strings
  if ($maxstringwidth) {
    if (ref($_[0]) eq "") { # a scalar
      my $maxwid = $maxstringwidth + length($truncsuf);
      if (!show_as_number($_[0])
          && length($_[0]) > $maxstringwidth + length($truncsuf)) {
        return \undef if $detection_pass;
        $_[0] = substr($_[0],0,$maxstringwidth).$truncsuf;
      }
    }
  }
  if (my $class = blessed($_[0])) {
    # Strinify objects which have the stringification operator
    if (overload::Method($class,'""')) { # implements operator stringify
      if (any { ref() eq "Regexp" ? $class =~ /$_/ 
                                  : ($_ eq "1" || $_ eq $class) } @$stringify) 
      {
        return \undef if $detection_pass;  # halt immediately
        # Make the change.  We are on a 2nd pass on a cloned copy
        my $prefix = show_as_number($_[0]) ? $magic_num_prefix : "";
        $_[0] = "${prefix}($class)".$_[0];  # *calls stringify operator*
      }
    }
  }
  # Prepend a "magic prefix" (later removed) to items which Data::Dumper is 
  # likely to represent wrongly or anyway not how we want:
  #
  #  1. Scalars set to strings like "6" will come out as a number 6 rather
  #     than "6" with Useqq(1) or Useperl(1) (string-ness is preserved
  #     with other options).  IMO this is a Data::Dumper bug which the
  #     maintainers won't fix it because the difference isn't functionally 
  #     relevant to correctly-written Perl code.  However we want to help 
  #     humans debug their software and so want to see the representation
  #     most liklye to have been used by the programmer to store the value.
  #
  #  2. Floating point values come out as "strings" to avoid some
  #     cross-platform problem I don't understand.  For our purposes
  #     we want all numbers to appear as numbers.
  if (!reftype($_[0]) && looks_like_number($_[0])) {
    return \undef if $detection_pass;  # halt immediately
    my $prefix = show_as_number($_[0])
                   ? $magic_num_prefix : $magic_numstr_prefix;
    $_[0] = $prefix.$_[0];
  }
  1
}

sub Dump {
  my $self = $_[0];
  local $_;
  &SaveAndResetPunct;
  if (! ref $self) { # ala Data::Dumper
    $self = $self->new(@_[1..$#_]);
  } else {
    croak "extraneous args" if @_ != 1;
  }

  my ($debug, $maxstringwidth, $stringify)
    = @$self{qw/VisDebug MaxStringwidth Stringify/};

  # Do desired substitutions in the data (cloning first)
  if ($stringify || $maxstringwidth) {
    $stringify = [ $stringify ] unless ref($stringify) eq 'ARRAY';
    $maxstringwidth //= 0;
    my $truncsuf = $self->{Truncsuffix};
    my $r = $self->_Visit_Values(
      sub{ __walk_worker(shift,1,$stringify,$maxstringwidth,$truncsuf) } );
    if (ref $r) {  # something needs changing
      $self->_Modify_Values(
        sub{ __walk_worker(shift,0,$stringify,$maxstringwidth,$truncsuf) } );
    }
  }

  my @values = $self->Values;
  if (@values != 1) {
    croak(@values==0 ? "No Values set" : "Only a single scalar value allowed")
  }

  # We always call Data::Dumper with Indent(0) and Pad("") to get a single
  # maximally-compact string, and then manually fold the result to Maxwidth,
  # and insert the user's Pad before each line.
  my $pad = $self->Pad();
  $self->Indent(0)->Pad("");
  #say "##Vis b4 SUPER: Indent=",$self->Indent(), " Useqq=",u($self->Useqq), " Pad=",debugvis($self->Pad)," Values=(",join(",", map{u} $self->Values),")" if $debug;
  {
    my ($sAt, $sQ) = ($@, $?); # Data::Dumper corrupts these
    $_ = $self->SUPER::Dump;
    ($@, $?) = ($sAt, $sQ);
  }
  #say "##Vis After SUPER ", Vis::debugvis($_);
  $self->Pad($pad);
  $_ = $self->_postprocess_DD_result($_);

  &RestorePunct;
  $_
}

# Walk an arbitrary structure calling &coderef on each item. stopping
# The sub should return 1 to continue, or any other defined value to
# terminate the traversal early.
# Members of containers are visited after processing the container item itself,
# and containerness is checked after &$coderef returns so that &$coderef
# may transform the item (by reference through $_[0]) e.g. to replace a
# container with a scalar.
# RETURNS: The final $&coderef return val
sub __walk($$;$);
sub __walk($$;$) {  # (coderef, item [, seenhash])
  no warnings 'recursion';
  my $seen = $_[2] // {};
  # Test for recursion both before and after calling the coderef, in case the
  # code unconditionally clones or otherwise replaces the item with new data.
  if (reftype($_[1])) {
    my $refaddr0 = refaddr($_[1]);
    return 1 if $seen->{$refaddr0}; # increment only below
  }
  # Now call the coderef and re-check the item
  my $r = &{ $_[0] }($_[1]);
  return $r unless (my $reftype = reftype($_[1])); # no longer a container?
  my $refaddr1 = refaddr($_[1]);
  return $r if $seen->{$refaddr1}++;
  return $r unless $r eq "1";
  if ($reftype eq 'ARRAY') {
    foreach (@{$_[1]}) {
      my $r = __walk($_[0], $_, $seen);
      return $r unless $r eq "1";
    }
  }
  elsif ($reftype eq 'HASH') {
    #foreach (values %{$_[1]})
    #  return 0 unless __walk($_[0], $_, $seen);
    #}
    # sort to retain same visitation order in cloned copy
    foreach (sort keys %{$_[1]}) {
      my $r = __walk($_[0], $_[1]->{$_}, $seen);
      return $r unless $r eq "1";
    }
  }
  1
}

# __walk() is called with the specified subref on the
# array of Values in the object.  The sub should not modify anything,
# but may return other than "1" to terminate the traversal.
# Returns the last value returned by the visitor sub.
sub _Visit_Values {
  my ($self, $coderef) = @_;
  my @values = $self->Values;
  __walk($coderef, \@values);
}

# Edit Values: __walk() is called with the specified subref on the
# array of Values in the object.  The Values are cloned first to
# avoid corrupting the user's data structure.
# The sub should return only 1, or 0 to terminate the traversal early.
sub _Modify_Values {
  my ($self, $coderef) = @_;
  my @values = $self->Values;
  unless ($self->{VisCloned}++) {
    require Clone;
    @values = map{ Clone::clone($_) } @values;
  }
  my $r = __walk($coderef, \@values);
  confess "bug" unless $r =~ /^[01]$/;
  $self->Values(\@values);
}

sub show_as_number(_) { # Derived from JSON::PP version 4.02
  my $value = shift;
  return unless defined $value;
  no warnings 'numeric';
  # if the utf8 flag is on, it almost certainly started as a string
  return if utf8::is_utf8($value);
  # detect numbers
  # string & "" -> ""
  # number & "" -> 0 (with warning)
  # nan and inf can detect as numbers, so check with * 0
  return unless length((my $dummy = "") & $value);
  return unless 0 + $value eq $value;
  return 1 if $value * 0 == 0;
  return -1; # inf/nan
}

# Split keys into "components" (e.g. 2_16.A has 3 components) and sort 
# components containing only digits numerically.
sub __sortkeys {
  my $hash = shift;
  return [
    sort { my @a = split /([_\W])/,$a;
           my @b = split /([_\W])/,$b;
           for (my $i=0; $i <= $#a; ++$i) {
             return 1 if $i > $#b;  # a is longer
             my $r = ($a[$i] =~ /^\d+$/ && $b[$i] =~ /^\d+$/)
                      ? ($a[$i] <=> $b[$i]) : ($a[$i] cmp $b[$i]) ;
             return $r if $r != 0;
           }
           return -1 if $#a < $#b; # a is shorter
           return 0;
         }
         keys %$hash
  ]
}

# FIXME: These don't take into account quoted strings in the interior!
our $curlies_re = RE_balanced(-parens=>'{}');
our $parens_re = RE_balanced(-parens=>'()');
our $curliesorsquares_re = RE_balanced(-parens=>'{}[]');

my $bareword_re = qr/\b[A-Za-z_][A-Za-z0-9_]*\b/;
my $qquote_re = qr/"(?:[^"\\]++|\\.)*+"/;
my $squote_re = qr/'(?:[^'\\]++|\\.)*+'/;
my $quote_re = qr/${qquote_re}|${squote_re}/;

# These never match spaces (except as part of a quote)
my $nonquote_atom_re 
      = qr/ (?: [^,;\{\}\[\]"'\s]++ | \\["'] )++ | [,;\{\}\[\]] /xs; 
my $atom_re = qr/ $quote_re | $nonquote_atom_re /x;

my $indent_unit = 2;

sub __insert_spaces() { # edits $_ in place
  #FIXME BUG HERE might corrupt interior of quoted strings
  
  ### TEMP? Verify that we can parse everything
  ### (probably redundant with 'unmatched tail' check in __fold)
  /\A(?: ${atom_re} | \s+ )+\z/xs or oops "regex problem($_)";

  s( $quote_re ?+ \K 
     ( \bsub\s*${curlies_re} | (?: $nonquote_atom_re | \s+)*+ )
   )
   ( do {
       local $_ = $1;
       s/\ (?![\$\@])//g;                        # FIXME: is this correct?
       s/^sub\ ?(${curlies_re})/"sub { ".substr($1,1,length($1)-2)." }"/eg;
       s/=>/ => /g;
       s/(=>\s*[-.\w\\]+,)/$1 /g;  #  key => value,<add space here>
       s/([\]\}],)(?!\ )/$1 /g; # after ], or },
       #s/,(?!\ )/, /g;      # space after every comma
       # Insert a space after an opening bracket at start of line, so the
       # first item will line up with the indentation of stuff at that level.
       s/\ +/ /sg;           # collapse muiltiple spaces
       $_
     }
   )exsg;
}#__insert_spaces

my $foldunit_re = qr/${atom_re}(?: , | \s*=>)?+/x;

sub _fold { # edits $_ in place
  my $self = shift;
  my ($debug, $maxwidth, $maxwidth1, $pad) = 
       (@$self{qw/VisDebug Maxwidth Maxwidth1/}, $self->Pad);
  return 
    if $maxwidth == 0;  # no folding
  my $maxwid = $maxwidth1 || $maxwidth;
  #$maxwid = INT_MAX if $maxwid==0;  # no folding, but maybe space adjustments
  $maxwid = max(0, $maxwid - length($pad));
  my $smidgen = max(5, int($maxwid / 6));
  #say "#VisFold WIDTHS: mw=$maxwid sm=$smidgen, iu=$indent_unit maxw=",u($maxwidth), " maxw1=",u($maxwidth1);

  pos = 0;
  my $curr_indent = 0;
  my $next_indent = 0;
  our $nind; local $nind = 0;
  my sub __ind_adjustment(;$) {
    if ($debug) {
      my $len = $curr_indent + pos() - $-[0];
      say "#VisFold: @{_}atom «$^N» len=$len pos=${\pos} \$-[0]=$-[0] c_indent=$curr_indent n_indent=$next_indent nind=$nind mw=$maxwid,$smidgen";
    }
    local $_ = $^N;;
    /^["']/ ? 0 : ( (()=/[\[\{\(]/) - (()=/[\]\}\)]/) )*$indent_unit;
  }
  s(\G
    (?{ say "##Visfold at top: pos=",u(pos)," ->«",substr($_,pos//0),"»"
          if $debug;
        local $nind = $next_indent;  # initialize localized var
    })
    (
      \s*(${foldunit_re})  # at least one even if too wide
      (?{ local $nind = $nind + __ind_adjustment("First ") })
      (?:
          \s*
          (${foldunit_re})
          (?{ local $nind = $nind + __ind_adjustment("Cont  ") })
          (?(?{ my $len = $curr_indent + pos() - $-[0];
                $len <= ($^N eq "[" ? $maxwid-$smidgen : $maxwid)
              })|(*FAIL))
      )*+
    )
    (?{ $next_indent = $nind }) # copy to non-localized storage
    (?<extra>\s*)
   )(do{
       my $len = length($1);
       my $indent = $curr_indent;
       $curr_indent = $next_indent;
       $maxwid = max(0, $maxwidth - length($pad)); # stop using maxwidth1
       say "#VisFold: --folding-- after «$1» pos ${\pos} new mw=$maxwid" 
         if $debug;
       $pad . (" " x $indent) . $1 ."\n"
     }
   )exsg
    or oops "\nnot matched (pad='$pad' maxwid=$maxwid):\n", debugvis($_),"\n";
  s/\n\z//
    or oops "unmatched tail (pad='${pad}') in:\n",debugvis($_);
  #say "## fold RESULT:", debugvis($_);
}#__fold

sub __unescape_printables() {
  # Data::Dumper outputs wide characters as escapes with Useqq(1).
  #say "__un INPUT:$_";

  s( \G (${atom_re}) (?<trailing>\s*)
   )( do{
        local $_ = $1;
        if (/^"/) {  # "double quoted string
          s{ (?: [^\\]++ | \\(?!x) )*+ \K ( \\x\{ (?<hex>[a-fA-F0-9]+) \} )
           }{
              my $c;
              length($+{hex}) <= 6 
                && ($c = chr(hex($+{hex}))) !~ m<\P{XPosixGraph}|[\0-\377]> 
              ? $c : $1
           }xesg;
        }
        $_
      }.$+{trailing}
   )xesg;
}

sub _postprocess_DD_result {
  (my $self, local $_) = @_;

  my ($debug, $vistype, $maxwidth, $maxwidth1)
    = @$self{qw/VisDebug VisType Maxwidth Maxwidth1/};

  croak "invalid VisType ", u($vistype)
    unless ($vistype//0) =~ /^(?:[salh]|hl)$/;

  say "##RAW  :",$_ if $self->{VisDebug};

  s/(['"])\Q$magic_num_prefix\E(.*?)(\1)/$2/sg;
  s/\Q$magic_numstr_prefix\E//sg;

  __unescape_printables;
  __insert_spaces;
  $self->_fold();

  if (($vistype//"s") eq "s") { }
  elsif ($vistype eq "a") {
    s/\A\[/(/ && s/\]\z/)/s or oops;
  }
  elsif ($vistype eq "l") {
    s/\A\[// && s/\]\z//s or oops;
  }
  elsif ($vistype eq "h") {
    s/\A\{/(/ && s/\}\z/)/s or oops;
  }
  elsif ($vistype eq "hl") {
    s/\A\{// && s/\}\z//s or oops;
  }
  else { oops }

  $_
} #_postprocess_DD_result {

my $sane_cW = $^W;
my $sane_cH = $^H;
our @save_stack;
sub SaveAndResetPunct() {
  # Save things which will later be restored, and reset to sane values.
  push @save_stack, [ $@, $!+0, $^E+0, $,, $/, $\, $^W ];
  $,  = "";       # output field separator is null string
  $/  = "\n";     # input record separator is newline
  $\  = "";       # output record separator is null string
  $^W = $sane_cW; # our load-time warnings
  #$^H = $sane_cH; # our load-time strictures etc.
}
sub RestorePunct() {
  ( $@, $!, $^E, $,, $/, $\, $^W ) = @{ pop @save_stack };
}

package DB;
# eval a string in the user's context and return the result.  The nearest
# non-DB frame must be the original user's call; this is accomplished by
# using "goto &DB::DB_Vis_Interpolate" in the entry-point sub.
sub DB_Vis_Eval($$) {
  my ($label_for_errmsg, $evalarg) = @_;
  Carp::confess("Vis bug:empty evalarg") if $evalarg eq "";
  # Many ideas here taken from perl5db.pl

  # Find the closest non-DB caller.  The eval will be done in that package.
  # Find the next caller further up which has arguments (i.e. wasn't doing
  # "&subname;"), and set our local @_ to be those arguments
  my ($distance, $pkg, $fname, $lno);
  for ($distance = 0 ; ; $distance++) {
    ($pkg, $fname, $lno) = caller($distance); 
    last if $pkg ne "DB";
  }
  while() {
    $distance++;
    my ($p, $hasargs) = (caller($distance))[0,4];
    if (! defined $p){
      @DB::args = ('<@_ is not defined in the outer block>');
      last
    }
    last if $hasargs;
  }
  local *_ = [ @DB::args ];  # copy in case of recursion

  # &SaveAndResetPunct was called in DB_Vis_Interpolate
  &Vis::RestorePunct;
  $Vis::user_dollarat = $@; # 'eval' will reset $@
  my @result = do {
    local @Vis::result;
    local $Vis::string_to_eval = 
      "package $pkg; "
     .' $@ = $Vis::user_dollarat; '
     .' @Vis::result = '.$evalarg.';'
     .' $Vis::user_dollarat = $@; '  # possibly changed by a tie handler
     ;
     &DB_Vis_Evalwrapper;
     #say "RAW errmsg:$@\nstring_to_eval=«$Vis::string_to_eval»" if $@;
     @Vis::result
  };
  my $errmsg = $@;
  &Vis::SaveAndResetPunct;
  $Vis::save_stack[-1]->[0] = $Vis::user_dollarat;

  if ($errmsg) {
    #warn "##RAW ERRMSG:«$errmsg» evalarg=«$evalarg» at ${fname}:$lno\n";
    $errmsg =~ s/ at \(eval \d+\) line \d+[^\n]*\n?\z//s;
    #warn "##COOKED MSG:«$errmsg»\n";
    Carp::confess("${label_for_errmsg}: Error interpolating '$evalarg' at $fname line $lno:\n$errmsg\n");
  }

  wantarray ? @result : (do{die "bug" if @result>1}, $result[0])
}# DB_Vis_Eval

sub DB_Vis_Interpolate {
  my ($self, $input, $s_or_d) = @_;
  return "<undef arg>" if ! defined $input;

  my sub u(_)       { &Vis::u  }
  my sub oops(;@)   { goto &Vis::oops }
  my sub confess(@) { goto &Carp::confess }
  my sub carp(@)    { goto &Carp::carp }

  # cf man perldata
  state $userident_re = qr/ (?: (?=\p{Word})\p{XID_Start} | _ )
                            (?: (?=\p{Word})\p{XID_Continue}  )* /x;
  
  state $pkgname_re = qr/ ${userident_re} (?: :: ${userident_re} )* /x;

  state $anyvname_re = 
    qr/ ${pkgname_re} | [0-9]+ | \^[A-Z] 
                      | [-+!\$\&\;i"'().,\@\/:<>?\[\]\~\^\\] /x;
  
  state $anyvname_or_refexpr_re = qr/ ${anyvname_re} | ${Vis::curlies_re} /x;

  &Vis::SaveAndResetPunct;

  my $debug = $self->Debug;
  my $useqq = $self->Useqq;
  my $q = $useqq ? "" : "q";
  my $funcname = $s_or_d . "vis" .$q;
  my @pieces;  # list of [visfuncname or "", inputstring]
  { local $_ = $input;
    if (/\b((?:ARRAY|HASH)\(0x[a-fA-F0-9]+\))/) {
      state $warned=0;
      carp("Warning: String passed to ${s_or_d}vis may have been interpolated by Perl\n(use 'single quotes' to avoid this)\n") unless $warned++;
    }
    say "#Vis_Interp START «$_»" if $debug;
    while (
      /\G (
           # Stuff without variable references (might include \n etc. escapes)
           ( (?: [^\\\$\@\%] | \\[^\$\@\%] )++ )
           |
           # $#arrayvar $#$$...refvarname $#{aref expr} $#$$...{ref2ref expr}
           #
           (?: \$\#\$*+\K ${anyvname_or_refexpr_re} )
           |
           # $scalarvar $$$...refvarname ${sref expr} $$$...{ref2ref expr}
           #  followed by [] {} ->[] ->{} ->method() ... «zero or more»
           # EXCEPT $$<punctchar> is parsed as $$ followed by <punctchar>
           #
           (?:
             (?: \$\$++ ${pkgname_re} \K | \$ ${anyvname_or_refexpr_re} \K )
             (?:
               (?: ->\K(?: ${$Vis::curliesorsquares_re} | ${userident_re}${Vis::parens_re}? ))
               |
               ${$Vis::curliesorsquares_re}
             )*
           )
           |
           # @arrayvar @$$...varname @{aref expr} @$$...{ref2ref expr}
           #  followed by [] {} «zero or one»
           #
           (?: \@\$*+\K ${anyvname_or_refexpr_re} ${$Vis::curliesorsquares_re}? )
           |
           # %hash %$hrefvar %{href expr} %$$...sref2hrefvar «no follow-ons»
           (?: \%\$*+\K ${anyvname_or_refexpr_re} )
          ) /xsgc) 
    {
      local $_ = $1; oops unless length() > 0;
      say "#Vis Interp: expr «$_»" if $debug;
      if (/^[\$\@\%]/) {
        my $sigl = substr($_,0,1);
        if ($s_or_d eq 'd') {
          # Inject a "plain text" fragment containing the dvis "expr=" prefix,
          # omitting the '$' sigl if the expr is a plain '$name'.
          push @pieces, ["", 
                         "q(".(/^\$(?!_)(${userident_re})\z/ ? $1 : $_)."=)"];
        }
        if ($sigl eq '$') {
          push @pieces, ["vis", $_];
        }
        elsif ($sigl eq '@') {
          # FIXME verify that multi-value eval results work
          push @pieces, ["avis", $_];
        }
        elsif ($sigl eq '%') {
          push @pieces, ["hvis", $_];
        }
        else { confess "BUG:sigl='$sigl'"; }
      } else {
        if (/^.+?(?<!\\)([\$\@\%])/) { confess "Vis bug: Missed '$1' in «$_»" }
        push @pieces, ["", "\"${1}\""];
      }
    }
    if (!defined(pos) || pos() < length($_)) {
      my $leftover = substr($_,pos()//0);
      confess "Vis Bug:LEFTOVER «$leftover»";
    }
  }# local $_

  my $result = "";
  foreach my $p (@pieces) {
    my ($methname, $arg) = @$p;
    if ($methname eq "") {
      my @aa = DB::DB_Vis_Eval($funcname, $arg);
      $result .= DB::DB_Vis_Eval($funcname, $arg);
    } else {
      # Reduce indent before first wrap to account for stuff alrady there
      my $leftwid = length($result) - rindex($result,"\n") - 1;
      my $maxwidth = $self->{Maxwidth};
      local $self->{Maxwidth1} = $self->{Maxwidth1} // $maxwidth;
      if ($maxwidth) {
        $self->{Maxwidth1} -= $leftwid if $leftwid < $self->{Maxwidth1}
      }
      say "${s_or_d}vis: leftwid=$leftwid, temp Maxwidth1=",u($Vis::Maxwidth1)," Useqq=",u($self->Useqq) if $debug;

      $result .= $self->$methname( DB::DB_Vis_Eval($funcname, $arg) );
    }    
  }

  &Vis::RestorePunct;
  $result
}#DB::DB_Vis_Interpolate

1;
 __END__

=encoding UTF-8

=head1 NAME

Vis - Human-oriented debug utilities using Data::Dumper

=head1 SYNOPSIS

  use Vis;

  @ARGV = ('-i', '/file/path');
  my %hash = ( 'z'x20 => 42, 
               complicated => ['lengthy', 'stuff', [1..35]], 'a'x25 => 43 );
  my $ref = \%hash;

  # Interpolate strings, replacing $scalar @array and %hash etc.
  # with massaged Data::Dumper output.  
  
  say svis 'FYI ref is $ref\nThat hash is: %hash\nArgs are @ARGV'; 
    -->FYI ref is { aaaaaa... => 43, ... }  
       That hash is: (aaaaaa... => 43, ... )
       Args are ("-i", "/file/path")

  # dvis (d for debug) auto-labels values 
  
  say dvis '$ref\n%hash\n@ARGV';  
    -->ref={ aaaaaa... => 43, ... }  
       %hash=(aaaaaa... => 43, ... )
       @argv=("-i", "/file/path")

  # Format one item at a time.  Data::Dumper is called in Terse(1) mode
  # and the result wrapped to fit the terminal width.
  $Vis::Maxwidth = 40;  # overrides terminal width

  say vis $hash{complicated};
    -->["lengthy","stuff",[1,2,3,4,5,6,7,8,9,
        10,11,12,13,14,15,16,17,18,19,20,21,
        22,23,24,25,26,27,28,29,30,31,32,33,
        34,35]]

  say avis @ARGV;
    -->("-i","/file/path")

  say hvis %hash;
    -->(aaaaaaaaaaaaaaaaaaaaaaaaa => 43,
         complicated => ["lengthy","stuff",
           [1,2,3,4,5,6,7,8,9,10,11,12,13,14,
             15,16,17,18,19,20,21,22,23,24,25,
             26,27,28,29,30,31,32,33,34,35]],
         zzzzzzzzzzzzzzzzzzzz => 42)

  # Prefer single-quoted strings
  say svisq 'The path is $ARGV[1]';
    -->The path is '/file/path'
  say dvisq 'The path is $ARGV[1]';
    -->The path is ARGV[1]='/file/path'
  say visq $ARGV[1];
    -->'/file/path'
  say avisq @ARGV;
    -->
  say hvisq %hash;
    -->(aaaaaaaaaaaaaaaaaaaaaaaaa => 43,
         complicated => ['lengthy','stuff', ... )

  # Math::BigInt etc. are shown in stringified form. prefixed by a tag
  { use bigint;
    my $struct = { debt => 999_999_999_999_999_999.02 }; 

    say vis $struct;
       --> {debt => (Math::BigFloat)999999999999999999.02}
    
    # disable stringifying objects
    $Vis::Stringify = "";
    say vis $struct;
       --> {debt => bless({ ... }, 'Math::BigFloat')}
  }

  # Data::Dumper output is "unescaped" so printable wide characters
  # appear as themselves instead of \x{ABCD}
  use open IO => ':locale';  # Encode wide characters for your tty
  use utf8;                  # This Perl source contains Unicode literals
  my $h = {msg => "Just let me read my Unicode ☻ ☺ 😊 and \N{U+2757}!"};
  say dvis '$h' ;
    --> h={msg => "Just let me read my Unicode ☻ ☺ 😊 ❗"}

  #-------- OO API --------
  # The *same functions* can be called as methods on a pre-allocated
  # object.  This lets you adjust configuration settings on a 
  # case-by-case basis (rather than using the global config variables)
  
  say Vis->new()->MaxStringwidth(50)->Maxdepth($levels)->vis($datum);

  #-------- UTILITIES --------
  
  say qsh($pathname);      # quote if needed for /bin/sh

  # Change undef to "undef", but otherwise return the argument as-is
  say u($might_be_undef);

  # Quote arguments for the shell
  system "ls -l ".join(" ", map{ qsh } 
      ("My Documents", $ENV{HOME}, "Uck!", "Qu'ote", 'Qu"ote'));

  # Quote arguments for the shell except leave ~ or ~username prefix
  system "ls -l ".join(" ", map{ qshpath } ("~", "~sally/subdir"));

  
=head1 DESCRIPTION

The C<Vis> package is a wrapper for C<Data::Dumper> with a more
convenient/comfortable/human-oriented interface for causual debugging
purposes (in the eyes of the beholder, of course).

Or for any situation where structured data needs to be displayed 
in a condensed, line-wrapped form.

The output from C<Vis> is what comes from C<Data::Dumper> 
modified as follows:

=over 4

=item *

Entire structures are displayed on a single line if possible, otherwise 
wrapped to the terminal width with indentation appropriate to the 
structure level.

A final newline is I<not> included.

=item *

"Safe" Unicode characters appear as themselves, instead of \x{ABCD} escapes.

Note: Since the result may include 'wide characters', you must encode 
the result before displaying it, as explained in C<perluniintro>.  
For example with C<use IO ':utf8';> or C<use IO ':locale';>.

=item *

Objects are stringified if they provide a stringification operator.  
For example, numbers declared within the scope of C<use bignum;> 
will appear in readable form rather than showing the internals of
the implementation.  

Stingified objects are prefixed by "(classname)" to make clear what
has happened.

=item *

Hash keys are sorted with numeric "components" sorted numerically,
for example "A.20" sorts before "A.100".
 
=back

=head1 FUNTIONS

=head2 svis 'string to be interpolated'

Returns the argument with variable references and escapes interpolated
as in in Perl double-quotish strings except that variable values are
formatted using C<vis()> or C<avis()> for $ or @ expressions, respectively.

In addition, C<%name> is interpolated using C<hvis()>.  

Most complex
reference expressions including slices and method calls are recognized, 
for example C<< ${\@myarray}[42]->{$key}{$nextlevel}->method(...) >>.

Strings are displayed in "double quoted" form. 
Internally, Data::Dumper is called with C<Useqq(1)>.

=head2 dvis 'string to be interpolated'

The same as C<svis> but interpolated items are prefixed by a "label=".
If the interpolated item is a simple scalar "$name" then the label will
be "name=" without the '$' sigl; otherwise the complete item/expression
is used as the label.

=head2 vis

=head2 vis SCALAREXPR

Format a single single scalar (default $_).  
The resulting string will not have a final newline.

=head2 avis LIST

Format an @array or other list as "(item, item, ...)";

=head2 hvis LIST

Format a %hash (or other list with an even 
number of items) as "(key => value, ...)".

=head2 svisq, dvisq, visq, avisq, hvisq

Alternative forms which prefer to display strings in 'single quoted' form.

Internally, Data::Dumper is called with C<Useqq(0)>, however if wide
characters are present Data::Dumper may force "double quoted" form
anyway, depending on the version of Data::Dumper.


=head1 OBJECT-ORIENTED INTERFACES


=head2 Vis->new()

Creates a C<Vis> object with default initialization.  No arguments
are permitted.

All of the I<functions> described above can also be called as methods
on a C<Vis> object (they examine the first argument to see if it is
a C<Vis> object and act as a method if so).

For example:

   Vis->new()->Maxwidth(40)->avis(@ARGV);

C<Vis> provides the following methods.
Default values are given by global variables of the same name 
(e.g. C<$Vis::Maxwidth>) which you can change at will.

=head2 Vis->MaxStringwidth(INTEGER)

=head2 Vis->Truncsuffix("...")

Set the maximum length for displayed strings.  Longer strings are
truncated and I<Truncsuffix> appended.

=head2 Vis->Maxwidth(FOLDWIDTH)

Set the fold width.  Default is the terminal width at time of first call.

=head2 Vis->Stringify(BOOL)

=head2 Vis->Stringify([list of class names])

Control object stringification.  

If a simple boolean (e.g. 0,1,undef,"")
is given then stringification of all objects (which implement
a stringification operator) is enabled/disabled.

If a list of class names is given, only objects of those classes
will be stringified.

=head2 Vis->Debug(BOOL)

True to enable internal debug tracing


=head1 Data::Dumper pass-thru options

The following Data::Dumper methods may be called.  Currently C<Vis>
derives from C<Data::Dumper> and so these methods are inherited.  
This may change in the future.


=head2 Useqq(BOOL)

True to prefer "double quoted" rather than 'single quoted' strings.
C<Vis> defaults to true except for the 'q' variants.

=head2 Sortkeys(subref)

Control how hash keys are sorted.  See C<Data::Dumper> documentation.

=head2 Quotekeys(subref)

Control how hash keys are "quoted".


You probably should not use any of the other C<Data::Dumper> methods
on C<Vis> objects.

=head1 UTILITY FUNCTIONS


=head2 u

=head2 u SCALAR

Returns the argument ($_ by default) if it is defined, otherwise
the string "undef".

=head2 qsh

=head2 qsh $string

=head2 qshpath

=head2 qshpath $path_with_tilde_prefix

The string ($_ by default) is quoted if necessary for parsing
by /bin/sh, which has different quoting rules than Perl.
"Double quotes" are used when no escapes would be needed,
otherwise 'single quotes'.

An item which contains only "safe" characters is returned unchanged.

References are formatted as with C<vis()> and the resulting string quoted.
Undefined values appear as C<undef> without quotes.

C<qshpath> is like C<qsh> except that an initial ~ or ~username is left
unquoted.  Useful with shells such as bash or csh.


=head1 SEE ALSO

Data::Dumper

=head1 CREDITS

This module contains code snippets copied from perl5db.pl 
Data::Dumper and JSON::PP.

=head1 AUTHOR

Jim Avera  (jim.avera AT gmail dot com)

=cut

#!/usr/bin/perl
# Tester for module Vis.  TODO: Convert to CPAN module-test setup
use strict; use warnings ; use feature qw(state say);
srand(42);  # so reproducible
say "FIXME: Test qr/.../ as values to be dumped.";
  #use Regexp::Common qw/RE_balanced/;
  #my $re = RE_balanced(-parens=>'(){}[]');
use utf8;
use open IO => 'utf8', ':std';
select STDERR; $|=1; select STDOUT; $|=1;
use Scalar::Util qw(blessed reftype looks_like_number);
use Carp;
use English qw( -no_match_vars );;
use Data::Compare qw(Compare);
#use Guard qw(scope_guard);

# This script was written before the author knew anything about standard
# Perl test-harness tools.  Perhaps someday it will be wholely rewritten.
# Meanwhile, some baby steps...
use Test::More;
use Test::Deep qw(cmp_deeply);

#use lib "$ENV{HOME}/lib/perl";

my $unicode_str;

# We want to test the original version of the internal function
# Data::Dumper::qquote to see if the useqq="utf8" feature has been fixed, and
# if not allow Vis to over-ride it.  But perl nowadays seems to cache the sub
# lookup immediately in Data::Dumper, making the override ineffective.
#
# As of Vis v1.146 Vis no longer overrides that internal function,
# but instead parses hex escape sequences in output strings.
# So this test is just to detect if Useqq('utf8') is made to work in the future.
#
BEGIN{
  # [Obsolete comment:]
  #   This test must be done before loading Vis, which over-rides an internal
  #   function to fix the bug
  $unicode_str = join "", map { chr($_) } (0x263A .. 0x2650);
  require Data::Dumper;
  print "Loaded ", $INC{"Data/Dumper.pm"}, " qquote=", \&Data::Dumper::qquote,"\n";
  { my $s = Data::Dumper->new([$unicode_str],['unicode_str'])->Terse(1)->Useqq('utf8')->Dump;
    chomp $s;
    $s =~ s/^"(.*)"$/$1/s or die "bug";
    if ($s ne $unicode_str) {
      #warn "Data::Dumper with Useqq('utf8'):$s\n";
      warn "Note: Useqq('utf8') is broken in your Data::Dumper.\n"
    } else {
      print "Useqq('utf8') seems to have been fixed in Data::Dumper !!! \n";
      die "Consider changing Vis to not bother parsing hex escapes?";
    }
  }
}

use Vis;
print "Loaded ", $INC{"Vis.pm"}, "\n";

# Do an initial read of $[ so arybase will be autoloaded
# (prevents corrupting $!/ERRNO in subsequent tests)
eval '$[' // die;

sub unix_compatible_os() {
  state $result //=
    # There must be a better way...
    (($^O !~ /win|dos/i && $^O =~ /ix$|ux$|bsd|svr|uni|osf|sv$/)
     || $^O eq 'darwin'
     || $^O eq 'cygwin'
    )
    && -w "/dev/null";
  $result;
}
sub tf($) { $_[0] ? "true" : "false" }

sub fmt_codestring($;$) { # returns list of lines
  my ($str, $prefix) = @_;
  $prefix //= "line ";
  my $i=1; map{ sprintf "%s%2d: %s\n", $prefix,$i++,$_ } (split /\n/,$_[0]);
}

sub timed_run(&$@) {
  my ($code, $maxcpusecs, @codeargs) = @_;
  use Time::HiRes qw(clock);
  my $startclock = clock();
  my (@result, $result);
  if (wantarray) {@result = &$code(@codeargs)} else {$result = &$code(@codeargs)};
  my $cpusecs = clock() - $startclock;
  confess "TOOK TOO LONG ($cpusecs CPU seconds vs. limit of $maxcpusecs)\n"
    if $cpusecs > $maxcpusecs;
  if (wantarray) {return @result} else {return $result};
}

sub visMaxwidth() {
  "Vis::Maxwidth=".u($Vis::Maxwidth)." Vis::Maxwidth1=".u($Vis::Maxwidth1)
  .($Vis::Maxwidth ? ("\n".("." x $Vis::Maxwidth)) : "")
}
sub checkeq_literal($$$) {
  my ($testdesc, $exp, $act) = @_;
  $exp = show_white($exp); $act = show_white($act);
  return unless $exp ne $act;
  my $posn = 0;
  for (0..length($exp)) {
    my $c = substr($exp,$_,1);
    last if $c ne substr($act,$_,1);
    $posn = $c eq "\n" ? 0 : ($posn + 1);
  }
  @_ = ( "\n**************************************\n"
        ."${testdesc}\n"
        ."Expected:\n$exp«end»\n"
        ."Actual  :\n$act«end»\n"
        .(" " x $posn)."^\n"
        .visMaxwidth()."\n" ) ;
  goto &Carp::confess;
}

# check $code_display, qr/$exp/, $doeval->($code, $item) ;
# { my $code="Vis->new->hvis(k=>'v');"; check $code, '(k => "v")',eval $code }
sub check($$@) {
  my ($code, $expected_arg, @actual) = @_;
  local $_;  # preserve $1 etc. for caller
  my @expected = ref($expected_arg) eq "ARRAY" ? @$expected_arg : ($expected_arg);
  die "ARE WE USING THIS FEATURE?" if @actual > 1;
  die "ARE WE USING THIS FEATURE?" if @expected > 1;
  confess "\nTESTa FAILED: $code\n"
         ."Expected ".scalar(@expected)." results, but got ".scalar(@actual).":\n"
         ."expected=(@expected)\n"
         ."actual=(@actual)\n"
         ."\$@=$@\n"
    if @expected != @actual;
  foreach my $i (0..$#actual) {
    my $actual = $actual[$i];
    my $expected = $expected[$i];
    if (ref($expected) eq "Regexp") {
      confess "\nTESTb FAILED: ",$code,"\n"
             ."Expected (Regexp):u\n".${expected}."«end»\n"
             ."Got:\n".u($actual)."«end»\n"
             .visMaxwidth()
        unless $actual =~ ($expected // "Never Matched");
    } else {
      checkeq_literal "TESTc FAILED: $code", $expected, $actual;
    }
  }
}

# Run a variety of tests on an item which is a string or strigified object
# which is not presented as a bare number (i.e. it is shown in quotes).
# The caller provides a sub which does the eval in the desired context,
# for example with "use bignum".
# The expected_re matches the item without surrounding quotes.
sub checkstringy(&$$) {
  my ($doeval, $item, $expected_re) = @_;
  my $expqq_re = "\"${expected_re}\"";
  my $expq_re  = "'${expected_re}'";
  foreach (
    [ 'Vis->new()->vis($_[1])',  '_Q_' ],
    [ 'vis($_[1])',              '_Q_' ],
    [ 'visq($_[1])',             '_q_' ],
    [ 'avis($_[1])',             '(_Q_)' ],
    [ 'avisq($_[1])',            '(_q_)' ],
    #currently broken due to $VAR problem: [ 'avisq($_[1], $_[1])',     '(_q_, _q_)' ],
    [ 'lvis($_[1])',             '_Q_' ],
    [ 'lvisq($_[1])',            '_q_' ],
    [ 'svis(\'$_[1]\')',         '_Q_' ],
    [ 'svis(\'foo$_[1]\')',      'foo_Q_' ],
    [ 'svis(\'foo$\'."_[1]")',   'foo_Q_' ],
    [ 'dvis(\'$_[1]\')',         '$_[1]=_Q_' ],
    [ 'dvis(\'foo$_[1]bar\')',   'foo$_[1]=_Q_bar' ],
    [ 'dvisq(\'foo$_[1]\')',     'foo$_[1]=_q_' ],
    [ 'dvisq(\'foo$_[1]bar\')',  'foo$_[1]=_q_bar' ],
    [ 'vis({ aaa => $_[1], bbb => "abc" })', '{aaa => _Q_,bbb => "abc"}' ],
  ) {
    my ($code, $exp) = @$_;
    $exp = quotemeta $exp;
    $exp =~ s/_Q_/$expqq_re/g;
    $exp =~ s/_q_/$expq_re/g;
    my $code_display = $code . " with \$_[1]=«$item»";
    local $Vis::Maxwidth = 0;  # disable wrapping
    check $code_display, qr/$exp/, $doeval->($code, $item) ;
  }
}

# Run a variety of tests on non-string item, i.e. something which is a
# number or structured object (which might contains strings within, e.g.
# values or quoted keys in a hash).
#
# The given regexp specifies the expected result with Useqq(1), i.e.
# double-quoted; a single-quoted version is derived internally.
sub checklit(&$$) {
  my ($doeval, $item, $dq_expected_re) = @_;
  (my $sq_expected_re = $dq_expected_re) 
    =~ s{ ( [^\\"]++|(\\.) )*+ \K " }{'}xsg
       or do{ die "bug" if $dq_expected_re =~ /(?<![^\\])'/; }; #probably
  say "dq_expected_re=$dq_expected_re";
  say "sq_expected_re=$sq_expected_re";
  foreach (
    [ 'Vis->new()->vis($_[1])',  '_Q_' ],
    [ 'vis($_[1])',              '_Q_' ],
    [ 'visq($_[1])',             '_q_' ],
    [ 'avis($_[1])',             '(_Q_)' ],
    [ 'avisq($_[1])',            '(_q_)' ],
    #currently broken due to $VAR problem: [ 'avisq($_[1], $_[1])',     '(_q_, _q_)' ],
    [ 'lvis($_[1])',             '_Q_' ],
    [ 'lvisq($_[1])',            '_q_' ],
    [ 'svis(\'$_[1]\')',         '_Q_' ],
    [ 'svis(\'foo$_[1]\')',      'foo_Q_' ],
    [ 'svis(\'foo$\'."_[1]")',   'foo_Q_' ],
    [ 'dvis(\'$_[1]\')',         '$_[1]=_Q_' ],
    [ 'dvis(\'foo$_[1]bar\')',   'foo$_[1]=_Q_bar' ],
    [ 'dvisq(\'foo$_[1]\')',     'foo$_[1]=_q_' ],
    [ 'dvisq(\'foo$_[1]bar\')',  'foo$_[1]=_q_bar' ],
    [ 'vis({ aaa => $_[1], bbb => "abc" })', '{aaa => _Q_,bbb => "abc"}' ],
  ) {
    my ($code, $exp_template) = @$_;
    my $exp = quotemeta $exp_template;
    $exp =~ s/_Q_/$dq_expected_re/g;
    $exp =~ s/_q_/$sq_expected_re/g;
    my $code_display = $code . " with \$_[1]=«$item»";
    local $Vis::Maxwidth = 0;  # disable wrapping
    check $code_display, qr/$exp/, $doeval->($code, $item) ;
  }
}

# ---------- Check stuff other than formatting or interpolation --------

print "Vis::VERSION = $Vis::VERSION\n";

for my $varname (qw(PREMATCH MATCH POSTMATCH)) {
  $_ = "test"; /(\w+)/;
  no strict 'vars';
  die "Vis imports high-overhead English ($varname)"
    if eval "defined \$Vis::$varname";
  die "EVAL ERR: $@ " if $@;
}

my $byte_str = join "",map { chr $_ } 10..30;

##################################################
# Check default $Vis::Maxwidth
##################################################
{ chomp( my $expected = `tput cols` );  # may default to 80 if no tty
  die "Expected initial Vis::Maxwidth to be undef" if defined $Vis::Maxwidth;
  { local $ENV{COLUMNS} = $expected + 13;
    vis(123);
    die "Vis::Maxwidth does not honor ENV{COLUMNS}" unless $Vis::Maxwidth == $expected + 13;
    undef $Vis::Maxwidth;  # re-enable auto-detect
  }
  die "bug: Vis::Maxwidth not undef" if defined($Vis::Maxwidth);
  if (unix_compatible_os()) {
    delete local $ENV{COLUMNS};
    vis(123);
    die "Vis::Maxwidth ",u($Vis::Maxwidth)," not defaulted correctly, expecting $expected" unless $Vis::Maxwidth == $expected;
    undef $Vis::Maxwidth;  # re-enable auto-detect
  }
  die "bug: Vis::Maxwidth not undef" if defined($Vis::Maxwidth);
  if (unix_compatible_os()) {
    delete local $ENV{COLUMNS};
    my $pid = fork();
    if ($pid==0) {
      require POSIX;
      die "bug" unless POSIX::setsid()==$$;
      POSIX::close $_ for (0,1,2);
      vis(123);
      exit($Vis::Maxwidth // 253);
    }
    waitpid($pid,0);
    die "Vis::Maxwidth defaulted to ", ($? >> 8)|($? & !0xFF), " (not $expected as expected)"
      unless $? == ($expected << 8);
    $? = 0;
  }
}

##################################################
# Check Useqq('utf8') support
##################################################
{
  my $vis_outstr = vis($unicode_str);
  chomp $vis_outstr;
  my $dd_outstr = Data::Dumper->new([$unicode_str],['unicode_str'])->Terse(1)->Useqq('utf8')->Dump;
  print "   unicode_str=\"$unicode_str\"\n";
  print "    Vis output=$vis_outstr\n";
  if (substr($vis_outstr,1,length($vis_outstr)-2) ne $unicode_str) {
    die "Unicode does not come through unmolested!";
  }
  print "   D::D output=$dd_outstr\n";
}

my $undef_as_false = undef;
if (! ref Vis->new()->Useqq(undef)) {
  warn "WARNING: Data::Dumper methods do not recognize undef boolean args as 'false'.\n";
  $undef_as_false = 0;
}

# Basic test of OO interfaces
{ my $code="Vis->new->vis('foo')  ;"; check $code, '"foo"',     eval $code }
{ my $code="Vis->new->avis('foo') ;"; check $code, '("foo")',   eval $code }
{ my $code="Vis->new->hvis(k=>'v');"; check $code, '(k => "v")',eval $code }
{ my $code="Vis->new->dvis('foo') ;"; check $code, 'foo',       eval $code }
{ my $code="Vis->new->svis('foo') ;"; check $code, 'foo',       eval $code }

foreach (
          ['Maxwidth',0,1,80,9999],
          ['MaxStringwidth',undef,0,1,80,9999],
          ['Truncsuffix',"","...","(trunc)"],
          # FIXME: This will spew debug messages.  Trap them somehow??
          ['Debug',undef,0,1],
          # Now the 'q' interfaces force Useqq(0) internally
          # ['Useqq',0,1,'utf8'],
          ['Quotekeys',0,1],
          ['Sortkeys',0,1,sub{ [ sort keys %{shift @_} ] } ],
          # Changing Indent and Terse are no longer allowed.
          # ['Terse',0,1],
          # ['Indent',0,1,2,3],
          ['Sparseseen',0,1,2,3],
        )
{
  my ($confname, @values) = @$_;
  my $testval = [123];
  foreach my $value (@values) {
    foreach my $base (qw(vis avis hvis lvis hlvis dvis svis)) {
      foreach my $q ("", "q") {
        my $dumper = $base . $q . "(42";
         $dumper .= ", 43" if $base =~ /^[ahl]/;
         $dumper .= ")";
        {
          my $v = eval "{ local \$Vis::$confname = \$value;
                          my \$obj = Vis->new();
                          \$obj->$dumper ;   # discard dump result
                          \$obj->$confname() # fetch effective setting
                        }";
        confess "bug:$@ " if $@;
        confess "\$Vis::$confname value is not preserved by $dumper\n",
            "(Set \$Vis::$confname=",u($value)," but $confname() returned ",u($v),")\n"
         unless (! defined $v and ! defined $value) || ($v eq $value);
        }
      }
    }
  }
}

# ---------- Check formatting or interpolation --------

sub MyClass::meth {
  my $self = shift;
  return @_ ? [ "methargs:", @_ ] : "meth_with_noargs";
}

# Many tests assume this
$Vis::Maxwidth = 72;

@ARGV = ('fake','argv');
$. = 1234;
$ENV{EnvVar} = "Test EnvVar Value";


my %toplex_h = ("" => "Emp", A=>111,"B B"=>222,C=>{d=>888,e=>999},D=>{},EEEEEEEEEEEEEEEEEEEEEEEEEE=>\42,F=>\\\43);
   # EEE... identifer is long to force linewrap
my @toplex_a = (0,1,"C",\%toplex_h,[],[0..9]);
my $toplex_ar = \@toplex_a;
my $toplex_hr = \%toplex_h;
my $toplex_obj = bless {}, 'MyClass';

our %global_h = %toplex_h;
our @global_a = @toplex_a;
our $global_ar = \@global_a;
our $global_hr = \%global_h;
our $global_obj = bless {}, 'MyClass';

our %maskedglobal_h = (key => "should never be seen");
our @maskedglobal_a = ("should never be seen");
our $maskedglobal_ar = \@maskedglobal_a;
our $maskedglobal_hr = \%maskedglobal_h;
our $maskedglobal_obj = bless {}, 'ShouldNeverBeUsedClass';

our %local_h = (key => "should never be seen");
our @local_a = ("should never be seen");
our $local_ar = \@local_a;
our $local_hr = \%local_h;
our $local_obj = \%local_h;

our $a = "global-a";  # used specially used by sort()
our $b = "global-b";

package A::B::C;
our %ABC_h = %main::global_h;
our @ABC_a = @main::global_a;
our $ABC_ar = \@ABC_a;
our $ABC_hr = \%ABC_h;
our $ABC_obj = $main::global_obj;

package main;

$_ = "GroupA.GroupB";
/(.*)\W(.*)/sp or die "nomatch"; # set $1 and $2

{ my $code = 'qsh("a b")';           check $code, '"a b"',  eval $code; }
{ my $code = 'qsh(undef)';           check $code, "undef",  eval $code; }
#qsh no longer accepts multiple args
#{ my $code = 'qsh("a b","c d","e",undef,"g",q{\'ab\'"cd"})';
#   check $code, ['"a b"','"c d"',"e","undef","g","''\\''ab'\\''\"cd\"'"], eval $code; }
#{ my $code = 'qshpath("a b")';       check $code, '"a b"',  eval $code; }
{ my $code = 'qshpath("~user")';     check $code, "~user",  eval $code; }
{ my $code = 'qshpath("~user/a b")'; check $code, '~user/"a b"', eval $code; }
{ my $code = 'qshpath("~user/ab")';  check $code, "~user/ab", eval $code; }
{ my $code = 'qsh("~user/ab")';      check $code, '"~user/ab"', eval $code; }
{ my $code = 'qsh($_)';              check $code, "${_}",   eval $code; }
{ my $code = 'qsh()';                check $code, "${_}",   eval $code; }
{ my $code = 'qsh';                  check $code, "${_}",   eval $code; }
{ my $code = 'qshpath($_)';          check $code, "${_}",   eval $code; }
{ my $code = 'qshpath()';            check $code, "${_}",   eval $code; }
{ my $code = 'qshpath';              check $code, "${_}",   eval $code; }
{ my $code = 'forceqsh($_)';         check $code, "\"${_}\"", eval $code; }

# Basic checks
{ my $code = 'vis($_)'; check $code, "\"${_}\"", eval $code; }
{ my $code = 'vis()'; check $code, "\"${_}\"", eval $code; }
{ my $code = 'vis'; check $code, "\"${_}\"", eval $code; }
{ my $code = 'avis($_,1,2,3)'; check $code, "(\"${_}\",1,2,3)", eval $code; }
{ my $code = 'hvis("foo",$_)'; check $code, "(foo => \"${_}\")", eval $code; }
{ my $code = 'hlvis("foo",$_)'; check $code, "foo => \"${_}\"", eval $code; }
{ my $code = 'avis(@_)'; check $code, '()', eval $code; }
{ my $code = 'hvis(@_)'; check $code, '()', eval $code; }
{ my $code = 'hlvis(@_)'; check $code, '', eval $code; }
{ my $code = 'avis(undef)'; check $code, "(undef)", eval $code; }
{ my $code = 'hvis("foo",undef)'; check $code, "(foo => undef)", eval $code; }
{ my $code = 'vis(undef)'; check $code, "undef", eval $code; }
{ my $code = 'vis(\undef)'; check $code, "\\undef", eval $code; }
{ my $code = 'svis(undef)'; check $code, "<undef arg>", eval $code; }
{ my $code = 'dvis(undef)'; check $code, "<undef arg>", eval $code; }
{ my $code = 'dvisq(undef)'; check $code, "<undef arg>", eval $code; }

{ my $code = q/my $s; my @a=sort{ $s=dvis('$a $b'); $a<=>$b }(3,2); "@a $s"/ ;
  check $code, '2 3 a=3 b=2', eval $code;
}

# Vis v1.147ish+ : Check corner cases of re-parsing code 
{ my $code = q(my $v = undef; dvis('$v')); check $code, "v=undef", eval $code; }
{ my $code = q(my $v = \undef; dvis('$v')); check $code, "v=\\undef", eval $code; }
{ my $code = q(my $v = \"abc"; dvis('$v')); check $code, 'v=\\"abc"', eval $code; }
{ my $code = q(my $v = \"abc"; dvisq('$v')); check $code, "v=\\'abc'", eval $code; }
{ my $code = q(my $v = \*STDOUT; dvisq('$v')); check $code, "v=\\*::STDOUT", eval $code; }
{ my $code = q(open my $fh, "</dev/null" or die; dvis('$fh')); 
  check $code, "fh=\\*{\"::\\\$fh\"}", eval $code; }
{ my $code = q(open my $fh, "</dev/null" or die; dvisq('$fh')); 
  check $code, "fh=\\*{'::\$fh'}", eval $code; }

# Check that $1 etc. can be passed (this was once a bug...)
# The duplicated calls are to check that $1 is preserved
{ my $code = '" a~b" =~ / (.*)()/ && qsh($1); die unless $1 eq "a~b";qsh($1)'; 
  check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)()/ && qshpath($1); die unless $1 eq "a~b";qshpath($1)'; 
  check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)()/ && forceqsh($1); die unless $1 eq "a~b";forceqsh($1)'; 
  check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)()/ && vis($1); die unless $1 eq "a~b";vis($1)'; 
  check $code, '"a~b"', eval $code; }
{ my $code = 'my $vv=123; \' a $vv b\' =~ / (.*)/ && dvis($1); die unless $1 eq "a \$vv b"; dvis($1)'; 
  check $code, 'a vv=123 b', eval $code; }

# Check Deparse support
{ my $data = eval 'BEGIN{ ${^WARNING_BITS} = 0 } no strict; no feature;
                   sub{ my $x = 42; };';
  { my $code = 'vis($data)'; check $code, "sub { \"DUMMY\" }", eval $code; }
  $Data::Dumper::Deparse = 1;
  { my $code = 'vis($data)'; check $code, "sub { my \$x=42; }", eval $code; }
}

# Floating point values (single values special-cased to show not as 'string')
{ my $code = 'vis(3.14)'; check $code, '3.14', eval $code; }
# But multiple values are sent through Data::Dumper, so...
{ my $code = 'vis([3.14])'; check $code, '[3.14]', eval $code; }

# bigint, bignum, bigrat support
#
# Recently Vis was changed to prepend (objtype) to stringified values,
# e.g. "(Math::BigFloat)3.14159265358979323846264338327950288419"
# but we might later change this back, or make the prefix optional;
# therefore we accept the result with or without with (type) prefix.

my $bigfstr = '9988776655443322112233445566778899.8877';
my $bigistr = '9988776655443322112233445566778899887766';
my $ratstr  = '1/9';

{
  use bignum;  # BigInt and BigFloat together

  # stringify everything possible
  local $Vis::Stringify = 1;  # NOTE: the '1' will be a BigInt !

  my $bigf = eval $bigfstr // die;
  die unless blessed($bigf) =~ /^Math::BigFloat/;
  checklit(sub{eval $_[0]}, $bigf, qr/(?:\(Math::BigFloat[^\)]*\))?${bigfstr}/);

  my $bigi = eval $bigistr // die;
  die unless blessed($bigi) =~ /^Math::BigInt/;
  checklit(sub{eval $_[0]}, $bigi, qr/(?:\(Math::BigInt[^\)]*\))?${bigistr}/);

  # Confirm that various Stringify values disable
  foreach my $Sval (0, undef, "", [], [0], [""]) {
    local $Vis::Stringify = $Sval;
    my $s = vis($bigf);
    die "bug(",u($Sval),")($s)" unless $s =~ /^\(?bless.*BigFloat/s;
  }
}
{
  use bigrat;
  my $rat = eval $ratstr // die;
  die unless blessed($rat) =~ /^Math::BigRat/;
  checklit(sub{eval $_[0]}, $rat, qr/(?:\(Math::BigRat[^\)]*\))?${ratstr}/);
}
{
  # no 'bignum' etc. in effect, just explicit class names
  use Math::BigFloat;
  my $bigf = Math::BigFloat->new($bigfstr);
  die unless blessed($bigf) =~ /^Math::BigFloat/;

  use Math::BigRat;
  my $rat = Math::BigRat->new($ratstr);
  die unless blessed($rat) =~ /^Math::BigRat/;

  # Without stringification
  { local $Vis::Stringify = 0;
    my $s = vis($bigf); die "bug($s)" unless $s =~ /^bless.*BigFloat/s;
  }
  # With explicit stringification of BigFloat only
  { local $Vis::Stringify = [qr/^Math::BigFloat/];
    checklit(sub{eval $_[0]}, $bigf, qr/(?:\(Math::BigFloat[^\)]*\))?${bigfstr}/);
    # But not other classes
    my $s = vis($rat); die "bug($s)" unless $s =~ /^bless.*BigRat/s;
  }
}

# Check string truncation, and that the original data is not modified in-place
{ my $orig_str  = '["abcDEFG",["xyzABCD",{longkey => "fghIJKL"}]]';
  my $check_data = eval $orig_str; die "bug" if $@;
  my $orig_data  = eval $orig_str; die "bug" if $@;
  foreach my $MSw (1..9) {
    # hand-truncate to create "expected result" data
    (my $exp_str = $orig_str) =~ s/\b([a-zA-Z]{$MSw})([a-zA-Z]*)/
                                    $1 . (length($2) > 3 && $1.$2 ne "longkey"
                                           ? "..." : $2)
                                  /seg;
    local $Vis::MaxStringwidth = $MSw;
    check "with MaxStringwidth=$MSw", $exp_str, eval 'vis($orig_data)';
    die "MaxStringwidth=$MSw : Original data corrupted"
      unless Compare($orig_data, $check_data);
  }
}

# There was a bug for s/dvis called direct from outer scope, so don't use eval:
check 
  'global divs %toplex_h',
  '%toplex_h=("" => "Emp",A => 111, "B B" => 222, C => {d => 888, e => 999'."\n"
    .'    }, D => {}, EEEEEEEEEEEEEEEEEEEEEEEEEE => \\42, F => \\\\\\43)',
  dvis('%toplex_h');
check 'global divs @ARGV', q(@ARGV=("fake","argv")), dvis('@ARGV');
check 'global divs $.', q($.=1234), dvis('$.');
check 'global divs $ENV{EnvVar}', q("Test EnvVar Value"), svis('$ENV{EnvVar}');
sub func {
  check 'func args', q(@_=(1,2,3)), dvis('@_');
}
func(1,2,3);

# There was once a "took almost forever" backtracking problem
my @backtrack_bugtest_data = (
  42,
  {A => 0, BBBBBBBBBBBBB => "foo"},
);
timed_run {
  check 'dvis @backtrack_bugtest_data',
        '@backtrack_bugtest_data=(42,{A => 0, BBBBBBBBBBBBB => "foo"})',
        dvis('@backtrack_bugtest_data');
} 0.01;

sub doquoting($$) {
  my ($input, $useqq) = @_;
  my $quoted = $input;
  if ($useqq) {
    $quoted =~ s/([\$\@"\\])/\\$1/gs;
    $quoted =~ s/\n/\\n/gs;
    $quoted =~ s/\t/\\t/gs;
    $quoted = "\"${quoted}\"";
  } else {
    $quoted =~ s/([\\'])/\\$1/gs;
    $quoted = "'${quoted}'";
  }
  return $quoted;
}

sub show_white($) {
  local $_ = shift;
  return "(Is undef)" unless defined;
  s/\t/<tab>/sg;
  s/( +)$/"<space>" x length($1)/seg; # only trailing spaces
  s/\n/<newline>\n/sg;
  $_
}

sub get_closure(;$) {
 my ($clobber) = @_;

 my %closure_h = (%toplex_h);
 my @closure_a = (@toplex_a);
 my $closure_ar = \@closure_a;
 my $closure_hr = \%closure_h;
 my $closure_obj = $toplex_obj;
 if ($clobber) { # try to over-write deleted objects
   @closure_a = ("bogusa".."bogusz");
 }

 return sub {

  # Perl is inconsistent about whether an eval in package DB can see
  # lexicals in enclosing scopes.  Sometimes it can, sometimes not.
  # However explicitly referencing those "global lexicals" in the closure
  # seems to make it work.
  #   5/16/16: Perl v5.22.1 *segfaults* if these are included
  #   (at least *_obj).  But removing them all causes some to appear
  #   to be non-existent.
  my $forget_me_not = [
     \$unicode_str, \$byte_str,
     \@toplex_a, \%toplex_h, \$toplex_hr, \$toplex_ar, \$toplex_obj,
     \@global_a, \%global_h, \$global_hr, \$global_ar, \$global_obj,
  ];

  # Referencing these intermediate variables also prevents them from
  # being destroyed before this closure is executed:
  my $saverefs = [ \%closure_h, \@closure_a, \$closure_ar, \$closure_hr, \$closure_obj ];


  my $zero = 0;
  my $one = 1;
  my $two = 2;
  my $EnvVarName = 'EnvVar';
  my $flex = 'Lexical in sub f';
  my $flex_ref = \$flex;
  my $ARGV_ref = \@ARGV;
  eval { die "FAKE DEATH\n" };  # set $@
  my %sublexx_h = %toplex_h;
  my @sublexx_a = @toplex_a;
  my $sublexx_ar = \@sublexx_a;
  my $sublexx_hr = \%sublexx_h;
  my $sublexx_obj = $toplex_obj;
  our %subglobal_h = %toplex_h;
  our @subglobal_a = @toplex_a;
  our $subglobal_ar = \@subglobal_a;
  our $subglobal_hr = \%subglobal_h;
  our $subglobal_obj = $toplex_obj;
  our %maskedglobal_h = %toplex_h;
  our @maskedglobal_a = @toplex_a;
  our $maskedglobal_ar = \@maskedglobal_a;
  our $maskedglobal_hr = \%maskedglobal_h;
  our $maskedglobal_obj = $toplex_obj;
  local %local_h = %toplex_h;
  local @local_a = @toplex_a;
  local $local_ar = \@toplex_a;
  local $local_hr = \%local_h;
  local $local_obj = $toplex_obj;

  my @tests = (
    [ __LINE__, q(aaa\\\\bbb), q(aaa\bbb) ],

    #[ q($unicode_str\n), qq(unicode_str=\" \\x{263a} \\x{263b} \\x{263c} \\x{263d} \\x{263e} \\x{263f} \\x{2640} \\x{2641} \\x{2642} \\x{2643} \\x{2644} \\x{2645} \\x{2646} \\x{2647} \\x{2648} \\x{2649} \\x{264a} \\x{264b} \\x{264c} \\x{264d} \\x{264e} \\x{264f} \\x{2650}\"\n) ],
    [__LINE__, q($unicode_str\n), qq(unicode_str="${unicode_str}"\n) ],

    [__LINE__, q(unicodehex_str=\"\\x{263a}\\x{263b}\\x{263c}\\x{263d}\\x{263e}\\x{263f}\\x{2640}\\x{2641}\\x{2642}\\x{2643}\\x{2644}\\x{2645}\\x{2646}\\x{2647}\\x{2648}\\x{2649}\\x{264a}\\x{264b}\\x{264c}\\x{264d}\\x{264e}\\x{264f}\\x{2650}\"\n), qq(unicodehex_str="${unicode_str}"\n) ],

    [__LINE__, q($byte_str\n), qq(byte_str=\"\\n\\13\\f\\r\\16\\17\\20\\21\\22\\23\\24\\25\\26\\27\\30\\31\\32\\e\\34\\35\\36\"\n) ],
    #[__LINE__, q($byte_str\n), qq(byte_str=\"\\n\\x{B}\\f\\r\\x{E}\\x{F}\\x{10}\\x{11}\\x{12}\\x{13}\\x{14}\\x{15}\\x{16}\\x{17}\\x{18}\\x{19}\\x{1A}\\e\\x{1C}\\x{1D}\\x{1E}\"\n) ],

    [__LINE__, q($flex\n), qq(flex=\"Lexical in sub f\"\n) ],
    [__LINE__, q($$flex_ref\n), qq(\$\$flex_ref=\"Lexical in sub f\"\n) ],

    [__LINE__, q($_ $ARG\n), qq(\$_=\"GroupA.GroupB\" ARG=\"GroupA.GroupB\"\n) ],
    [__LINE__, q($a\n), qq(a=\"global-a\"\n) ],
    [__LINE__, q($b\n), qq(b=\"global-b\"\n) ],
    [__LINE__, q($1\n), qq(\$1=\"GroupA\"\n) ],
    [__LINE__, q($2\n), qq(\$2=\"GroupB\"\n) ],
    [__LINE__, q($3\n), qq(\$3=undef\n) ],
    [__LINE__, q($&\n), qq(\$&=\"GroupA.GroupB\"\n) ],
    [__LINE__, q(${^MATCH}\n), qq(\${^MATCH}=\"GroupA.GroupB\"\n) ],
    [__LINE__, q($.\n), qq(\$.=1234\n) ],
    [__LINE__, q($NR\n), qq(NR=1234\n) ],
    [__LINE__, q($/\n), qq(\$/=\"\\n\"\n) ],
    [__LINE__, q($\\\n), qq(\$\\=undef\n) ],
    [__LINE__, q($"\n), qq(\$\"=\" \"\n) ],
    [__LINE__, q($~\n), qq(\$~=\"STDOUT\"\n) ],
    #20 :
    [__LINE__, q($^\n), qq(\$^=\"STDOUT_TOP\"\n) ],
    [__LINE__, q($:\n), qq(\$:=\" \\n-\"\n) ],
    [__LINE__, q($^L\n), qq(\$^L=\"\\f\"\n) ],
    [__LINE__, q($?\n), qq(\$?=0\n) ],
    [__LINE__, q($[\n), qq(\$[=0\n) ],
    [__LINE__, q($$\n), qq(\$\$=$$\n) ],
    [__LINE__, q($^N\n), qq(\$^N=\"GroupB\"\n) ],
    [__LINE__, q($+\n), qq(\$+=\"GroupB\"\n) ],
    [__LINE__, q(@+ $#+\n), qq(\@+=(13,6,13) \$#+=2\n) ],
    [__LINE__, q(@- $#-\n), qq(\@-=(0,0,7) \$#-=2\n) ],
    #30 :
    [__LINE__, q($;\n), qq(\$;=\"\\34\"\n) ],
    #[__LINE__, q($;\n), qq(\$;=\"\\x{1C}\"\n) ],
    [__LINE__, q(@ARGV\n), qq(\@ARGV=(\"fake\",\"argv\")\n) ],
    [__LINE__, q($ENV{EnvVar}\n), qq(\$ENV{EnvVar}=\"Test EnvVar Value\"\n) ],
    [__LINE__, q($ENV{$EnvVarName}\n), qq(\$ENV{\$EnvVarName}=\"Test EnvVar Value\"\n) ],
    [__LINE__, q(@_\n), <<'EOF' ],  # N.B. Maxwidth was set to 72
@_=(42,[0,1,"C",{"" => "Emp",A => 111, "B B" => 222, C => {d => 888,
        e => 999}, D => {}, EEEEEEEEEEEEEEEEEEEEEEEEEE => \42, F =>
      \\\43}, [], [0,1,2,3,4,5,6,7,8,9]])
EOF
    [__LINE__, q($#_\n), qq(\$#_=1\n) ],
    [__LINE__, q($@\n), qq(\$\@=\"FAKE DEATH\\n\"\n) ],
    #37 :
    map({
      my ($LQ,$RQ) = (/^(.)(.)$/) or die "bug";
      map({
        my $name = $_;
        map({
          my ($dollar, $r) = @$_;
          my $dolname_scalar = $dollar ? "\$$name" : $name;
          # Make total prefix length constant to avoid wrap variations
          my $maxnamelen = 12;
          my $spfx = "x" x (
            (1+1+$maxnamelen+1)  # {dollar}$name{r}
            - (length($dollar)+length($dolname_scalar)+length($r)) );
          my $pfx = substr($spfx,0,length($spfx)-1);
          #state $depth=0;
          #say "##($depth) spfx=<$spfx> pfx=<$pfx> dollar=<$dollar> r=<$r> dns=<$dolname_scalar> n=<$name>"; $depth++;
          
          #my $p = " " x length("?${dollar}${name}_?${r}");
          my $p = "";

          [__LINE__, qq(${pfx}%${dollar}${name}_h${r}\\n), <<EOF ],
${pfx}\%${dollar}${name}_h${r}=("" => "Emp",A => 111, "B B" => 222, C => {d => 888,
${p}    e => 999}, D => {}, EEEEEEEEEEEEEEEEEEEEEEEEEE => \\42, F => \\\\\\43)
EOF


          [__LINE__, qq(${pfx}\@${dollar}${name}_a${r}\\n), <<EOF ],
${pfx}\@${dollar}${name}_a${r}=(0,1,"C",{"" => "Emp",A => 111, "B B" => 222, C => {
${p}      d => 888, e => 999}, D => {}, EEEEEEEEEEEEEEEEEEEEEEEEEE => \\42,
${p}    F => \\\\\\43}, [], [0,1,2,3,4,5,6,7,8,9])
EOF

          [__LINE__, qq(${pfx}\$#${dollar}${name}_a${r}),    
            qq(${pfx}\$#${dollar}${name}_a${r}=5)   
          ],
          [__LINE__, qq(${pfx}\$#${dollar}${name}_a${r}\\n), 
            qq(${pfx}\$#${dollar}${name}_a${r}=5\n) 
          ],

          [__LINE__, qq(${spfx}\$${dollar}${name}_a${r}[3]{C}{e}\\n),
            qq(${spfx}\$${dolname_scalar}_a${r}[3]{C}{e}=999\n)
          ],

          [__LINE__, qq(${spfx}\$${dollar}${name}_a${r}[3]->{A}\\n),
            qq(${spfx}\$${dolname_scalar}_a${r}[3]->{A}=111\n)
          ],
          [__LINE__, qq(${spfx}\$${dollar}${name}_a${r}[3]->{$LQ$RQ}\\n),
            qq(${spfx}\$${dolname_scalar}_a${r}[3]->{$LQ$RQ}="Emp"\n)
          ],
          [__LINE__, qq(${spfx}\$${dollar}${name}_a${r}[3]{C}->{e}\\n),
            qq(${spfx}\$${dolname_scalar}_a${r}[3]{C}->{e}=999\n)
          ],
          [__LINE__, qq(${spfx}\$${dollar}${name}_a${r}[3]->{C}->{e}\\n),
            qq(${spfx}\$${dolname_scalar}_a${r}[3]->{C}->{e}=999\n)
          ],
          [__LINE__, qq(${spfx}\@${dollar}${name}_a${r}[\$zero,\$one]\\n),
            qq(${spfx}\@${dollar}${name}_a${r}[\$zero,\$one]=(0,1)\n)
          ],
          [__LINE__, qq(${spfx}\@${dollar}${name}_h${r}{${LQ}A${RQ},${LQ}B B${RQ}}\\n),
            qq(${spfx}\@${dollar}${name}_h${r}{${LQ}A${RQ},${LQ}B B${RQ}}=(111,222)\n)
          ],
        }
          #(['',''], ['$','r'])
          (['$','r'],['',''])
        ), #map [$dollar,$r]

        ( $] >= 5.022001 && $] <= 5.022001
            ?  (do{ state $warned = 0;
                    warn "\n\n** obj->method() tests disabled ** due to Perl v5.22.1 segfault!\n\n"
                     unless $warned++; ()
                  },())
            : (
               [__LINE__, qq(\$${name}_obj->meth ()), qq(\$${name}_obj->meth="meth_with_noargs" ()) ],
               [__LINE__, qq(\$${name}_obj->meth(42)), qq(\$${name}_obj->meth(42)=["methargs:",42]) ],
              )
        ),

        map({
          my ($dollar, $r, $arrow) = @$_;
          my $dolname_scalar = $dollar ? "\$$name" : $name;
          [__LINE__, qq(\$${dollar}${name}_h${r}${arrow}{\$${name}_a[\$two]}{e}\\n),
            qq(\$${dolname_scalar}_h${r}${arrow}{\$${name}_a[\$two]}{e}=999\n)
          ],
          [__LINE__, qq(\$${dollar}${name}_a${r}${arrow}[3]{C}{e}\\n),
            qq(\$${dolname_scalar}_a${r}${arrow}[3]{C}{e}=999\n)
          ],
          [__LINE__, qq(\$${dollar}${name}_a${r}${arrow}[3]{C}->{e}\\n),
            qq(\$${dolname_scalar}_a${r}${arrow}[3]{C}->{e}=999\n)
          ],
          [__LINE__, qq(\$${dollar}${name}_h${r}${arrow}{A}\\n),
            qq(\$${dolname_scalar}_h${r}${arrow}{A}=111\n)
          ],
        } (['$','r',''], ['','r','->'])
        ), #map [$dollar,$r,$arrow]
        }
        qw(closure sublexx toplex global subglobal 
           maskedglobal local A::B::C::ABC)
      ), #map $name
      } ('""', "''")
    ), #map ($LQ,$RQ)
  );
  for my $test (@tests) {
    my ($lno, $dvis_input, $expected) = @$test;
    #warn "##^^^^^^^^^^^ lno=$lno dvis_input='$dvis_input' expected='$expected'\n";

    { local $@;  # check for bad syntax first, to avoid uncontrolled die later
      # For some reason we can't catch exceptions from inside package DB.
      # undef is returned but $@ is not set
      # 3/5/22: The above comment may not longer be true; there might have been
      #  a bug where $@ was not saved properly.  BUT VERIFY b4 deleting this comment.
      my $ev = eval { "$dvis_input" };
      die "Bad test string:$dvis_input\nPerl can't interpolate it (lno=$lno)"
         .($@ ? ":\n  $@" : "\n")
        if $@ or ! defined $ev;
    }

    my sub punctbug($$$) {
      my ($varname, $act, $exp) = @_;
      confess "ERROR (testing '$dvis_input' lno $lno): $varname was not preserved (expected $exp, got $act)\n"
    }
    my sub checkspunct($$$) {
      my ($varname, $actual, $expecting) = @_;
      check "dvis('$dvis_input') lno $lno : $varname NOT PRESERVED : ",
            $actual//"<undef>", $expecting//"<undef>" ;
    }
    my sub checknpunct($$$) {
      my ($varname, $actual, $expecting) = @_;
      # N.B. check() compaares as strings
      check "dvis('$dvis_input') lno $lno : $varname NOT PRESERVED : ",
            defined($actual) ? $actual+0 : "<undef>",
            defined($expecting) ? $expecting+0 : "<undef>" ;
    }

    for my $use_oo (0,1) {
      my $actual;
      my $dolatval = $@;
      eval { $@ = $dolatval;
        # Verify that special vars are preserved and don't affect Vis
        # (except if testing a punctuation var, then don't change it's value)

        my ($origAt, $origFs, $origBs, $origComma, $origBang, $origCarE, $origCarW)
          = ($@, $/, $\, $,, $!, $^E, $^W);

        # Don't change a value if being tested in $dvis_input
        my ($fakeAt, $fakeFs, $fakeBs, $fakeComma, $fakeBang, $fakeCarE, $fakeCarW)
          = ($dvis_input =~ /(?<!\\)\$@/    ? $origAt : "FakeAt",
             $dvis_input =~ /(?<!\\)\$\//   ? $origFs : "FakeFs",
             $dvis_input =~ /(?<!\\)\$\\\\/ ? $origBs : "FakeBs",
             $dvis_input =~ /(?<!\\)\$,/    ? $origComma : "FakeComma",
             $dvis_input =~ /(?<!\\)\$!/    ? $origBang : 6,
             $dvis_input =~ /(?<!\\)\$^E/   ? $origCarE : 6,  # $^E aliases $! on most OSs
             $dvis_input =~ /(?<!\\)\$^W/   ? $origCarW : 0); # $^W can only be 0 or 1

        ($@, $/, $\, $,, $!, $^E, $^W) = ($fakeAt, $fakeFs, $fakeBs, $fakeComma, $fakeBang,
                                          $fakeCarE, $fakeCarW);

        $actual = $use_oo
           ? Vis->new->dvis($dvis_input)
           : dvis($dvis_input);

        checkspunct('$@',  $@,   $fakeAt);
        checkspunct('$/',  $/,   $fakeFs);
        checkspunct('$\\', $\,   $fakeBs);
        checkspunct('$,',  $,,   $fakeComma);
        checknpunct('$!',  $!+0, $fakeBang);
        checknpunct('$^E', $^E+0,$fakeCarE);
        checknpunct('$^W', $^W+0,$fakeCarW);

        # Restore
        ($@, $/, $\, $,, $!, $^E, $^W)
          = ($origAt, $origFs, $origBs, $origComma, $origBang, $origCarE, $origCarW);
        $dolatval = $@;
      }; #// do{ $actual  = $@ };
      if ($@) { $actual = $@ }
      $@ = $dolatval;

      checkeq_literal(
        "dvis (oo=$use_oo) lno $lno failed: input «"
                                              . show_white($dvis_input)."»",
        $expected,
        $actual);
    }

    # Check Useqq
    for my $useqq (0, 1) {
      my $input = $expected.$dvis_input.'qqq@_(\(\))){\{\}\""'."'"; # gnarly
      # Now Data::Dumper (version 2.174) forces "double quoted" output
      # if there are any Unicode characters present.
      # So we can not test single-quoted mode in those cases
      next
        if $input =~ tr/0-\377//c; #
      my $exp = doquoting($input, $useqq);
      my $act = Vis->new->Useqq($useqq)->dvis($input);
      die "\n\nUseqq ",u($useqq)," bug:\n"
         ."   Input   «${input}»\n"
         ."  Expected «${exp}»\n"
         ."       Got «${act}»\n "
        unless $exp eq $act;
    }
  }
 };
} # get_closure()
sub f($) {
  get_closure(1);
  my $code = get_closure(0);
  get_closure(1);
  get_closure(1);
  $code->(@_);
}
sub g($) {
  local $_ = 'SHOULD NEVER SEE THIS';
  goto &f;
}
&g(42,$toplex_ar);
print "Tests passed.\n";
exit 0;

# End Tester
