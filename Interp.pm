# Copyright © Jim Avera 2012-2022.  This document contains code snippets
# from perl5db.pl and Data::Dumper as noted in adjacent comments, and 
# those extracts remain subject to the licenses of their respective sources.  
# Other portions have been released into the Public Domain in accordance with 
# Creative Commons CC0 (http://creativecommons.org/publicdomain/zero/1.0/)
use strict; use warnings FATAL => 'all'; use 5.012;

use utf8;
# This file contains UTF-8 characters in debug-output strings (e.g. « and »).
# But to see them correctly on a non-Latin1 terminal (e.g. utf-8), your
# program has to say
#   use open ':std' => ':locale';
# or otherwise set the STDERR encoding to match what your terminal expects
# (Perl assumes Latin-1 by default).

package Vis;
# See below for $VERSION and POD documentation

# *** The following must appear before anything is declared which might
# *** alias a user-provided thing referenced in svis/dvis strings we eval

package DB;

# eval a string in the user's context and return the result
# @Vis::saved = ($@, $!, $^E, $,, $/, $\, $^W) must be pre-set,
# and will be temporarily restored during the eval.
sub DB_Vis_Eval {   # Many ideas here taken from perl5db.pl

  # The call stack:
  # FIXME this may not be true any more. Probably user is immediate caller
  #   0: An internal Vis function which called us
  #   1: User's sub which called a Vis function or method
  #   2: The caller of the user's sub (this frame defines @_)

  ($Vis::pkg) = (caller(0))[0];  # package containig user's call

  # Get @_ values from the closest frame above the user's call which
  # has arguments.  This will be the very next frame (i.e. frame 2)
  # unless the user was called using "&subname;".
  # N.B. caller() leaves the args in @DB::args
  # FIXME check/debug this
  for ($Vis::DistToArgs = 1 ; ; $Vis::DistToArgs++) {
    my ($pkg,undef,undef,undef,$hasargs) = caller $Vis::DistToArgs;
    if (! $pkg) {
      undef $Vis::DistToArgs;
      last;
    }
    last if $hasargs;
  }
  # make visible as "@_" if the user is in a sub (otherwise we can't)
  local *_ = defined($Vis::DistToArgs)
               ? \@DB::args
               : ['<@_ is not defined in the outer scope>'] ;

  # At this point, nothing is in scope except the name of this sub
  # and the simulated @_.
  # FIXME: Use   package DB { ... } to simplify this and move to _Interpolate

  {
    no strict 'refs';
    ($!, $^E, $,, $/, $\, $^W) = @Vis::saved[1..6];
    # LHS of assignment must be inside eval to catch die from tie handlers
    $Vis::evalstr = "package $Vis::pkg;"
                   .' $@ = $Vis::saved[0];'
                   .' @Vis::result = "'.$Vis::evalarg.'";'
                   .' $Vis::saved[0] = $@;'  # might be set by a tie handler
                   ;
    eval $Vis::evalstr;
  };
  if (my $exmsg = $@) {
    my ($pkg, $fname, $lno) = (caller(0));
    $exmsg =~ s/ at \(eval \d+\) line \d+[^\n]*//sg;
    Carp::confess("${Vis::funcname}: Error interpolating '$Vis::evalarg' at $fname line $lno:\n$exmsg\n");
  }

  package Vis;

  # Because of tied variables, we may have just executed user code!
  # Re-save the possibly-modified punctuation variables and reset them to
  # sane values to ensure that our code can function.
  #
  # However $@ here is from our eval, not the result of any tie handlers.
  # $@ was saved to $saved[0] inside the eval, and we want to keep that value. 
  # The following "local $saved[0]" causes that value to be preserved 
  # (actually restored) instead of the value saved by &SaveAndResetPunct
  { local $Vis::saved[0]; &Vis::SaveAndResetPunct; }

  my @res = @Vis::result;

  # Erase package variables which might contain references to user objects
  # which would otherwise not be destroyed when expected
  undef @Vis::saved; undef @Vis::result; undef $Vis::evalarg;

  @res;
}# DB_Vis_Eval

package Vis;

use version 0.77; our $VERSION = version->declare(sprintf "v%s", q$Revision: 1.153 $ =~ /(\d[.\d]+)/);

use Exporter;
use Carp;
use feature qw(switch state);
use POSIX qw(INT_MAX);
use Encode ();
use Scalar::Util qw(blessed reftype refaddr looks_like_number);
use List::Util qw(min max first any);
use Regexp::Common qw/RE_balanced/;
use overload ();

use Terminalsize qw(get_terminal_columns);

require Data::Dumper;

use Exporter 'import';
our @EXPORT    = qw(vis  avis  lvis  svis  dvis  hvis  hlvis
                    visq avisq lvisq svisq dvisq hvisq hlvisq
                    u qsh forceqsh qshpath);
                    #Dumper
our @EXPORT_OK = qw($Maxwidth $MaxStringwidth $Truncsuffix $Debug
                    $Stringify $Usehex
                    $Useqq $Quotekeys $Sortkeys $Terse $Indent $Sparseseen);

our @ISA       = ('Data::Dumper');

# Used by non-oo functions, and initial settings for oo constructors.
our ($Maxwidth, $MaxStringwidth, $Truncsuffix, $Debug, $Stringify, $Usehex,
     $Useqq, $Quotekeys, $Sortkeys, $Terse, $Indent, $Sparseseen);

$Debug          = 0            unless defined $Debug;
$MaxStringwidth = 0            unless defined $MaxStringwidth;
$Maxwidth       = undef        unless defined $Maxwidth; # undef to auto-detect
$Truncsuffix    = "..."        unless defined $Truncsuffix;
$Stringify      = 1            unless defined $Stringify;
$Usehex         = 1            unless defined $Usehex; # for binary octets

# The following Vis defaults override Data::Dumper defaults
$Useqq          = 1            unless defined $Useqq;
$Quotekeys      = 0            unless defined $Quotekeys;
$Sortkeys       = \&__sortkeys unless defined $Sortkeys;
$Terse          = 1            unless defined $Terse;
$Indent         = 1            unless defined $Indent;
$Sparseseen     = 1            unless defined $Sparseseen;

sub Debug {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{VisDebug} = $v), return $s) : $s->{VisDebug};
}
sub MaxStringwidth {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{MaxStringwidth} = $v), return $s) : $s->{MaxStringwidth};
}
sub Maxwidth {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{Maxwidth} = $v), return $s) : $s->{Maxwidth};
}
sub Truncsuffix {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{Truncsuffix} = $v), return $s) : $s->{Truncsuffix};
}
sub Stringify {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{Stringify} = $v), return $s) : $s->{Stringify};
}
sub Usehex {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{Usehex} = $v), return $s) : $s->{Usehex};
}
sub VisType {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{VisType} = $v), return $s) : $s->{VisType};
}

sub debugvis(_) {  # for our internal debug messages
  my $s = Data::Dumper->new([shift])->Useqq(1)->Terse(1)->Indent(0)->Maxdepth(2)->Dump;
  chomp $s;
  $s
}
sub debugavis(@) { "(" . join(", ", map{debugvis} @_) . ")" }

sub oops(@) { @_ = ("\noops:",@_,"\n  "); goto &Carp::confess }

############### Functional (non-oo) APIs #################
sub u(_) { $_[0] // "undef" }
sub forceqsh(_) {
  # Unlike Perl, the bourne shell does not recognize backslash escapes
  # inside '...', but quoting must be interrupted, e.g. 'foo\''bar'
  local $_ = shift;
  return "undef" unless defined;
  $_ = Vis::vis($_) if ref;
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
  my ($tilde_prefix, $rest) = /(^~[^\/\\]*\/?)?(.*)/s or die;
  $rest eq "" ? $tilde_prefix : $tilde_prefix.qsh($rest)
}

########### FUNCTIONAL/OO APIs #############
sub _getobj {
  (blessed($_[0]) && $_[0]->isa(__PACKAGE__) ? shift : __PACKAGE__->new())
}
sub _getobj_scalar { &_getobj->Values([$_[0]]) }
sub _getobj_array  { &_getobj->Values([\@_])   } #->Values([[@_]])
sub _getobj_hash {
  my $o = &_getobj;
  (scalar(@_) % 2)==0 or croak "Uneven number args for hash key => val pairs";
  $o ->Values([{@_}])
}

# These can be called as *FUNCTIONS* or as *METHODS* of an 
# already-created Vis object.
sub vis(_)    { &_getobj_scalar->VisType('s' )->Dump; }
sub visq(_)   { &_getobj_scalar->VisType('s' )->Useqq(0)->Dump; }
sub avis(@)   { &_getobj_array ->VisType('a' )->Dump; }
sub avisq(@)  { &_getobj_array ->VisType('a' )->Useqq(0)->Dump; }
sub lvis(@)   { &_getobj_array ->VisType('l' )->Dump; }
sub lvisq(@)  { &_getobj_array ->VisType('l' )->Useqq(0)->Dump; }
sub hvis(@)   { &_getobj_hash  ->VisType('h' )->Dump; }
sub hvisq(@)  { &_getobj_hash  ->VisType('h' )->Useqq(0)->Dump; }
sub hlvis(@)  { &_getobj_hash  ->VisType('hl')->Dump; }
sub hlvisq(@) { &_getobj_hash  ->VisType('hl')->Useqq(0)->Dump; }

# Trampolines which replace the call frame with a call directly to the
# interpolation code which uses package DB to access the user's context.
sub svis(_)  { @_ = (&_getobj,           shift, 's'); goto &_Interpolate }
sub svisq(_) { @_ = (&_getobj->Useqq(0), shift, 's'); goto &_Interpolate }
sub dvis(_)  { @_ = (&_getobj,           shift, 'd'); goto &_Interpolate }
sub dvisq(_) { @_ = (&_getobj->Useqq(0), shift, 'd'); goto &_Interpolate }

sub new {
  croak "No args allowed for Vis::new" if @_ > 1;
  my ($class) = @_;
  (bless $class->SUPER::new([],[]), $class)->_config_defaults()
}
          
############# only internals follow ############
sub _config_defaults {
  my $self = shift;

  # Preserve any special $Data::Dumper::Useqq setting, e.g. 'utf8'
  $self->Useqq(1) unless $self->Useqq();

  if (! defined $Maxwidth) {
    local *_; 
    # perl bug: Localizing *_ does not deal with the special filehandle "_" 
    #  see https://github.com/Perl/perl5/issues/19142
    $Maxwidth = get_terminal_columns(debug => $self->{VisDebug})//80
  }

  $self
    ->Quotekeys($Quotekeys)
    ->Sortkeys($Sortkeys)
    ->Terse($Terse)
    ->Indent($Indent)
    ->Debug($Debug)
    ->Stringify($Stringify)
    ->Usehex($Usehex)
    ->Useqq($Useqq)
    ->Sparseseen($Sparseseen)
    ->Maxwidth($Maxwidth)
    ->MaxStringwidth($MaxStringwidth)
    ->Truncsuffix($Truncsuffix)
}

my $magic_num_prefix = "NUM:".\&new;        # "NUM:CODE(0x.......)"
my $magic_numstr_prefix = "NUMSTR:".\&new;

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
      # FIXME: use List::Util::any
      my $shouldwe;
      foreach my $mod (@$stringify) {
        # Stringify may be class name, a Regexp matching class name,
        # or the number 1 (to enable any class)
        $shouldwe=1, last if
          ref($mod) eq "Regexp"
            ? $class =~ /$mod/
            : ($class eq $mod || $mod eq "1")
      }
      if ($shouldwe) {
        return \undef if $detection_pass;  # halt immediately
        # Make the change.  We are on a 2nd pass on a cloned copy
        $_[0] = "($class)".$_[0];  # *calls stringify operator*
      }
    }
  }
  # Prepend a "magic prefix" to items which Data::Dumper is likely
  # to represent wrongly or anyway not how we want.  The prefix will
  # be removed from the result later.
  #
  # 1. Scalars is set to a string like "6" will come out as a number
  #    instead of string (except with Useqq(0) and Useperl(0)).
  #    This is a bug.
  # 2. All floating point values come out as "strings" to avoid
  #    some cross-platform issues.  We don't want that.
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
  if (! ref $self) { # ala Data::Dumper
    $self = $self->new(@_[1..$#_]);
  } else {
    croak "extraneous args" if @_ != 1;
  }

  my ($debug, $maxstringwidth, $stringify)
    = @$self{qw/VisDebug MaxStringwidth Stringify/};

  #------------------------------------------------------
  # Do desired substitutions in the data (cloning first)
  #------------------------------------------------------
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
  if ($debug) {
    say "##Vis Values(",scalar(@values),"): @values\n";
  }
  if (@values != 1) {
    croak "Only a single scalar value (possibly a ref) is allowed by Vis"
      if @values >= 1;
    croak "No Values set"
  } 

  # We always call Data::Dumper with Indent(0) and Pad("") to get a single
  # maximally-compact string, and then manually fold the result to Maxwidth,
  # and insert the user's Pad before each line.
  my $pad = $self->Pad();
  $self->Indent(0)->Pad("");
  {
    my ($sAt, $sQ) = ($@, $?); # Data::Dumper corrupts these
    $_ = $self->SUPER::Dump;
    ($@, $?) = ($sAt, $sQ);
  }
  $self->Pad($pad);
  $self->_postprocess_DD_result($_)
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

# Split keys into "components" (e.g. 2_16.A has 3 components) and sort each
# component numerically if the corresponding items are both numbers.
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

sub __insert_spaces() { # edits $_ in place
  s/([\]\}],)(\S)/$1 $2/sg;
}

my $curlies_re = RE_balanced(-parens=>'{}');
my $parens_re = RE_balanced(-parens=>'()');
my $curliesorsquares_re = RE_balanced(-parens=>'{}[]');

my $nonbracket_atom_re = qr/
       "(?:[^"\\]++|\\.)*+"   # "dq string"
     | '(?:[^'\\]++|\\.)*+'   # 'sq string'
     | -?\.?\d[-\d\.eE]*+ | (?i:NaN|[-+]?Inf)  # number
     | \([A-Z][\w:]++\)  # our stringification prefix
     | bless\( | \)
     | \b[A-Za-z]\w*+\b       # bareword e.g. hash key
     | =>
     | ,
/x;
my $atom_re = qr/ ${nonbracket_atom_re} | [\[\]\{\}] /x;

#my $brackpair_re = qr/ \[ (?!\[) ${atom_re}*? \] /x; # only one level
my $brackpair_re = qr/ \[ ${nonbracket_atom_re}*? \] /x; #TEMP

sub __fold($$) { # edits $_ in place
  my ($maxwid, $pad) = @_;

  #say "## fold input:", debugvis($_);
  #say "## pad:", debugvis($pad);

  $maxwid -= length($pad);
  my $smidgen = min(5, int($maxwid / 6));
  #say "## Adjusted maxwid=$maxwid smidgen=$smidgen";

  pos = 0;
  my $prev_indent = 0;
  my $next_indent;
  our $ind; local $ind = 0;
  s(   
       (?{ local $ind = $next_indent }) # initialize localized var 
       (
         (\s*${atom_re},?+)  # at least one even if too wide
         (?{ local $ind = $ind;
             { local $_=$^N; /^[\[\{\(]/ && $ind++; /^[\]\}\)]/ && $ind--; }
           })
         (?:
             \s*
             (${atom_re},?+)
             (?{ local $ind = $ind;
                 { local $_=$^N; /^[\[\{\(]/ && $ind++; /^[\]\}\)]/ && $ind--; }
               })
             (?(?{ die "urp undef pos!" unless defined(pos);
                   die "oop" unless defined($-[0]);
                   die "undef ind" unless defined($ind);
                   my $len = $prev_indent + pos() - $-[0];
                   #say "Testing pos=${\pos()} ($^N) len=$len ind=$ind next_indent=$next_indent";
                   $len <= ($^N eq "[" ? $maxwid-$smidgen : $maxwid)
                 })|(*FAIL))
         )*+
       )
       (?{ $next_indent = $ind }) # copy to non-localized storage
       (?<extra>\s*)
   )(do{
       my $len = length($1);
       if ($len > $maxwid) {
         #say "##VisFFOL over-long pos=${\pos()} (indent=$indent len=$len mw=$maxwid)\n«$1»";
       } else {
         #say "##VisFF   ok len    pos=${\pos()} (indent=$indent len=$len mw=$maxwid)\n«$1»";
       }
       my $indent = $prev_indent;
       $prev_indent = $next_indent;
       $pad . (" "x$indent) . $1 ."\n" 
     })exsg 
    or oops "\nnot matched (pad='$pad' maxwid=$maxwid):\n", debugvis($_),"\n";
  s/\n\z//
    or die "unmatched tail (pad='${pad}') in:\n",debugvis($_);
}

sub _postprocess_DD_result {
  (my $self, local $_) = @_;

  my ($debug, $vistype, $maxwidth)
    = @$self{qw/VisDebug VisType Maxwidth/};

  croak "invalid VisType ", u($vistype)
    unless ($vistype//0) =~ /^(?:[salh]|hl)$/;

  say "##RAW  :",$_ if $self->{VisDebug};

  s/(['"])\Q$magic_num_prefix\E(.*?)(\1)/$2/sg;
  s/\Q$magic_numstr_prefix\E//sg;

  __insert_spaces;
  __fold($maxwidth, $self->Pad());

  if (($vistype//"s") eq "s") {
  }
  elsif ($vistype eq "a") {
    oops debugvis($_) unless s/\A\[/(/ && s/\]\z/)/s;
  }
  elsif ($vistype eq "l") {
    oops unless s/\A\[// && s/\]\z//s;
  }
  elsif ($vistype eq "h") {
    oops($_) unless s/\A\{/(/ && s/\}\z/)/s;
  }
  elsif ($vistype eq "hl") {
    oops($_) unless s/\A\{// && s/\}\z//s;
  }
  elsif ($vistype eq "s") {
  }
  else {
    oops "invalid VisType '$vistype'\n";
  }

  $_
} #_postprocess_DD_result {

my $sane_cW = $^W;
our @saved;
sub SaveAndResetPunct() {
  # Save things which will later be restored, and reset to sane values.
  our @saved = ( $@, $!+0, $^E+0, $,, $/, $\, $^W );
  $,  = "";       # output field separator is null string
  $/  = "\n";     # input record separator is newline
  $\  = "";       # output record separator is null string
  $^W = $sane_cW; # our load-time warnings
}

# cf man perldata
my $userident_re = qr/ (?: (?=\p{Word})\p{XID_Start} | _ )
                       (?: (?=\p{Word})\p{XID_Continue}  )* /x;

my $varname_re = qr/ ${userident_re} | \^[A-Z] | [0-9]+ 
                                     | [-+!().,\/:<>?\[\]\^\\]
                                     /x;

my $varname_or_refexpr_re = qr/ ${varname_re} | ${curlies_re} /x;

my sub __check_notmissed($$) {
  my ($fragment, $posn) = @_;
  confess "\n***Vis bug: Unparsed '$1' at offset ",u($posn),
          " ->",substr($_,$posn//0) if $fragment =~ /(?<!\\)([\$\@\%])/;
}
sub _Interpolate {
  (my $self, local $_, my $s_or_d) = @_;
  SaveAndResetPunct;
  if (/\b((?:ARRAY|HASH)\(0x[a-fA-F0-9]+\))/) {
    state $warned=0;
    Carp::carp "Warning: String passed to ${s_or_d}vis may have been interpolated by Perl\n(use 'single quotes' to avoid this)\n" unless $warned++;
  }
  my $newstr = "";
  while (
    /\G (.*?) 
        (
         # $#arrayvar $#$$...refvarname $#{aref expr} $#$$...{ref2ref expr}
         #
         (?: \$\#\$*+\K ${varname_or_refexpr_re} )
         |
         # $scalarvar $$$...refvarname ${sref expr} $$$...{ref2ref expr}
         #  followed by [] {} ->[] ->{} ->method() ... «zero or more»
         #
         (?:
           \$++\K ${varname_or_refexpr_re}
           (?:
             (?: ->\K(?: ${curliesorsquares_re} | ${userident_re}${parens_re}? ))
             |
             ${curliesorsquares_re}
           )*
         )
         |
         # @arrayvar @$$...varname @{aref expr} @$$...{ref2ref expr}
         #  followed by [] {} «zero or one»
         #
         (?: \@\$*+\K ${varname_or_refexpr_re} ${curliesorsquares_re}? )
         |
         # %hash %$hrefvar %{href expr} %$$...sref2hrefvar «no follow-ons»
         (?: \%\$*+\K ${varname_or_refexpr_re} )
        )
    /xsgc) {
    $newstr .= $1;
    __check_notmissed($1, pos()-length($2)-length($1));
    if ($2) {
      my $sigl = substr($2,0,1);
      if ($s_or_d eq 'd') {
        local $_ = $2;
        # Show "foo=<value>" for "$foo" but otherwise use entire expr as label
        $newstr .= (/^\$(${userident_re})\z/ ? $1 : $2)."=";
      }
      if ($sigl eq '$') {
        $newstr .= '${\Vis::vis(' . $2 . ')}';
      }
      elsif ($sigl eq '@') {
        $newstr .= '${\Vis::avis(' . $2 . ')}';
      }
      elsif ($sigl eq '%') {
        $newstr .= '${\Vis::hvis(' . $2 . ')}';
      }
      else { confess "BUG:sigl='$sigl'" }
    }
  }
  if (!defined(pos) || pos() < length($_)) {
    my $leftover = substr($_,pos()//0);
    say "LEFTOVER: «$leftover»";
    __check_notmissed($leftover,pos);
    $newstr .= $leftover;
  }
  $Vis::funcname = __PACKAGE__."::".$s_or_d."vis";
  $Vis::evalarg = $newstr;
  goto &DB::DB_Vis_Eval
}

1;
