use strict; use warnings FATAL => 'all'; use 5.010;

# This file contains UTF-8 characters in debug-output strings (e.g. « and »).
# But to see them correctly on a non-Latin1 terminal (e.g. utf-8), your 
# program has to say 
#   use open ':std' => ':locale';
# or otherwise set the STDERR encoding to match what your terminal expects
# (Perl assumes Latin-1 by default).
use utf8;

$Vis::VERSION = sprintf "%d.%03d", q$Revision: 1.92 $ =~ /(\d+)/g;

# Copyright © Jim Avera 2012-2014.  Released into the Public Domain
# by the copyright owner.  (jim.avera AT gmail dot com)
# Please retain the preceeding attribution in any copies or derivitives.

# Documentation is at the end

# The following must appear before any global variables are declared,
# so that nothing is visible when evaluating user-provided expressions.

package DB;

# This must appear before any un-qualified variable declarations,
# to avoid accidentally masking the user's variables in the eval below.
#
sub Vis_Eval {   # Many ideas here were stolen from perl5db.pl

  { ($Vis::evalarg) = $_[0] =~ /(.*)/s; }  # untaint

  # The call stack:
  #   0: DB::DB_Vis_Interpolate calling us (Vis_Eval)
  #   1: User calling DB::DB_Vis_Interpolate (via trampoline gotos)
  #   2: The caller of the user's sub (this frame defines @_)

  ($Vis::pkg) = caller 1;   # name of package containing user's call

  # Get @_ values from the closest frame above the user's call which
  # has arguments.  This will be the very next frame (i.e. frame 2)
  # unless the user was called using "&subname;".
  # (caller() leaves the args in @DB::args)
  for ($Vis::DistToArgs = 2; ; $Vis::DistToArgs++) {
    my ($pkg,undef,undef,undef,$hasargs) = caller $Vis::DistToArgs;
    if (! $pkg) {
      undef $Vis::DistToArgs;
      last;
    }
    last if $hasargs;
  }

  # make user's @_ accessible, if the user is in a sub (otherwise we can't)
  local *_ = defined($Vis::DistToArgs) 
               ? \@DB::args
               : ['<@_ is not defined in the outer scope>'] ;

  # At this point, nothing is in scope except the name of this sub
  # and the simulated @_

  #OLD:
  #@Vis::result = eval '($@, $!, $^E, $,, $/, $\, $^W) = @Vis::saved;'
  #                   ."package $Vis::pkg; $Vis::evalarg ";

  # The LHS of this assignment has to be inside the eval to catch die 
  # from tied variable handlers.
  {
    no strict 'refs';
    ($!, $^E, $,, $/, $\, $^W) = @Vis::saved[1..6];
    eval 'package '.$Vis::pkg.'; $@=$Vis::saved[0]; @Vis::result='.$Vis::evalarg.' ';
  };


  package Vis;

  our (@saved, $evalarg, @result);

  my $at = $@;

  # Now save the possibly-modified values of punctuation variables.
  # Because of tied variables, we may have just executed user code!
  # So save the possibly-modified punctuation variables again and 
  # reset them to sane values.
  #
  # However we don't want to re-save $@, which we just cleared by
  # executing our own 'eval' above; we want to later restore the 
  # user's original $@, which was saved earlier.
  # By localizing $saved[0] here, the effect of the following call to
  # &SaveAndResetPunct will be un-done when we return for that value only, 
  # restoring the originally-saved value for $@ to $saved[0].
  local $saved[0];
  &Vis::SaveAndResetPunct;

  if ($at) {
    $at =~ s/ at (?:\(eval \d+\)|\S+) line \d+\.?\n?\z//s;
    push @DB::CARP_NOT, 'Vis';
#    { ###TEMP
#      require PadWalker;
#      for (my $level=1; defined(caller($level-1)); $level++) {
#        my ($pkg,$fn,$line) = caller($level-1);
#        my (undef,undef,undef,$subr) = caller($level-1+1);
#        $subr //= "";
#        my $my_h = PadWalker::peek_my($level);
#        warn "--- level $level ${fn}:$line ($pkg)($subr) ---\n";
#        while (my ($vn, $vr) = each %$my_h) {
#          warn "  my $vn = ", debugvis($vr), "\n";
#        }
#        my $our_h = PadWalker::peek_our($level);
#        while (my ($vn, $vr) = each %$our_h) {
#          warn "  our $vn = ", debugvis($vr), "\n";
#        }
#      }
#    }
    Carp::croak(
      $Vis::error_prefix,
      (index($at,$evalarg) >= 0 ? "" : "Error interpolating '$evalarg', "),
      "$at (package ",scalar(caller),")\n");
  }

  return @result;
}

package Vis;

use Exporter;
use Carp;
use feature qw(switch state);
use POSIX qw(INT_MAX);
use Encode ();
use Scalar::Util qw(looks_like_number);
use List::Util qw(any);

our $Utf8patch //= 1;

use Data::Dumper ();

my %esc = (  
    "\a" => "\\a",
    "\b" => "\\b",
    "\t" => "\\t",
    "\n" => "\\n",
    "\f" => "\\f",
    "\r" => "\\r",
    "\e" => "\\e",
);
# Following is a copy of qquote() from Data::Dumper version 2.130_02 with 
# minimal changes to make Useqq('utf8') work.
sub fixed_qquote {
  local($_) = shift;
  s/([\\\"\@\$])/\\$1/g;
  # ...do not replace [:^ascii:] with hex escapes here...
  return qq("$_") unless 
    /[^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~]/;  # fast exit

  my $high = shift || "";
  s/([\a\b\t\n\f\r\e])/$esc{$1}/g;

  if (ord('^')==94)  { # ascii
    # no need for 3 digits in escape for these
    s/([\0-\037])(?!\d)/'\\'.sprintf('%o',ord($1))/eg;
    s/([\0-\037\177])/'\\'.sprintf('%03o',ord($1))/eg;
    # all but last branch below not supported --BEHAVIOR SUBJECT TO CHANGE--
    if ($high eq "iso8859") {
      s/([\200-\240])/'\\'.sprintf('%o',ord($1))/eg;
    } elsif ($high eq "utf8") {
      # Leave printable wide chars alone so humans can read them.
      # The caller must guarantee that the input data consists of
      # decoded characters (not octets), and that the result will later
      # be encoded to an encoding which can represent them (e.g. UTF-8).
      
      state $printable_chars = '[:graph:] ';
      #state $printable_chars = '\p{XID_Continue} ';

      state $printable_re = qr/[${printable_chars}]/s;
      state $nonprintable_re = qr/[^${printable_chars}]/s;

      #s/([^[:graph:] ])/ sprintf("\\x{%x}",ord($1)) /uge;
      #  The above s/// was very slow with large binary blobs due to reallocs,
      #  so trying a copy-to-preallocated-array technique...
      if (/${nonprintable_re}/s) {
        my $new = ""; vec($new,length($_)*8,8)=0; $new = ""; # pre-allocate
        my $limit = length($_);
        my $prevpos = 0;
        until ($prevpos == $limit) {
          while (/\G${printable_re}+/gsc) {
            $new .= substr($_,$prevpos,pos()-$prevpos); # use ${^MATCH} with /p?
            $prevpos = pos;
          }
          while (/\G${nonprintable_re}+/gscp) {
            my $fmt = "\\x{%x}" x length(${^MATCH});
            $new .= sprintf $fmt, map{ord} 
                      split(//, substr($_,$prevpos,pos()-$prevpos));
            $prevpos = pos;
          }
        }
        $_ = $new;
      }
    } elsif ($high eq "8bit") {
        # leave it as it is
    } else {
      s/([\200-\377])/'\\'.sprintf('%03o',ord($1))/eg;
      s/([^\040-\176])/sprintf "\\x{%04x}", ord($1)/ge;
    }
  }
  else { # ebcdic
      s{([^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~])(?!\d)}
       {my $v = ord($1); '\\'.sprintf(($v <= 037 ? '%o' : '%03o'), $v)}eg;
      s{([^ !"\#\$%&'()*+,\-.\/0-9:;<=>?\@A-Z[\\\]^_`a-z{|}~])}
       {'\\'.sprintf('%03o',ord($1))}eg;
  }

  return qq("$_");
}

sub debugvis($) {  # for our internal debug messages
  local $/ = "\n";
  confess "should call debugavis" if @_ != 1;
  my $s = Data::Dumper->new([shift])->Useqq(1)->Terse(1)->Indent(0)->Dump;
  chomp $s;
  return $s;
}
sub debugavis(@) {  # for our internal debug messages
  local $/ = "\n";
  my $s = "(";
  foreach my $a (@_) {
    $s .= "," unless $s eq "(";
    $s .= Data::Dumper->new([$a])->Useqq(1)->Terse(1)->Indent(1)->Dump;
    chomp $s;
  }
  return "$s)";
}

our @ISA       = qw(Exporter Data::Dumper);
our @EXPORT    = qw(vis  avis  hvis svis  dvis
                    visq avisq svisq dvisq u
                    Dumper qsh forceqsh qshpath);
our @EXPORT_OK = qw($Maxwidth $MaxStringwidth $Truncsuffix $Debug 
                    $Useqq $Quotekeys $Sortkeys 
                    $Terse $Indent $Sparseseen);

# Used by non-oo functions, and initial settings for oo constructors.
our ($Maxwidth, $MaxStringwidth, $Truncsuffix, $Debug, $Stringify,
     $Useqq, $Quotekeys, $Sortkeys,
     $Terse, $Indent, $Sparseseen);

$Maxwidth       = undef       unless defined $Maxwidth; # undef to auto-detect
$MaxStringwidth = 0           unless defined $MaxStringwidth;
$Truncsuffix    = "..."       unless defined $Truncsuffix;
$Debug          = 0           unless defined $Debug;
$Stringify      = undef       unless defined $Stringify;

# The following Vis defaults override Data::Dumper defaults
$Useqq          = 1           unless defined $Useqq;
$Quotekeys      = 0           unless defined $Quotekeys;
$Sortkeys       = \&_sortkeys unless defined $Sortkeys;
$Terse          = 1           unless defined $Terse;
$Indent         = 1           unless defined $Indent;
$Sparseseen     = 1           unless defined $Sparseseen;

# Functional (non-oo) APIs
sub u(@);
sub u(@) { 
  @_ == 0 ? u($_) :
  @_ == 1 ? (defined($_[0]) ? $_[0] : "undef") :
  wantarray ? (map { u($_) } @_) : croak("u(): Multiple args in scalar context")
}

sub vis(;$)   { return __PACKAGE__->vnew(@_)->Dump1; }
sub visq($)   { return __PACKAGE__->vnew(@_)->Useqq(0)->Dump1; }
sub avis(@)   { return __PACKAGE__->anew(@_)->Dump1; }
sub avisq(@)  { return __PACKAGE__->anew(@_)->Useqq(0)->Dump1; }
sub hvis(@)   { return __PACKAGE__->hnew(@_)->Dump; }
sub hvisq(@)  { return __PACKAGE__->hnew(@_)->Useqq(0)->Dump1; }

# trampolines
#   The interpolation code for svis, etc. must live in package DB and
#   the closest non-DB call frame must be the user's context.
sub svis($;@)  { goto &DB::DB_Vis_svis }
sub svisq($;@) { goto &DB::DB_Vis_svisq }
sub dvis($;@)  { goto &DB::DB_Vis_dvis }
sub dvisq($;@) { goto &DB::DB_Vis_dvisq }

# Provide Data::Dumper non-oo APIs
# Declared without prototypes (would be just (@)) to match Data::Dumper
sub Dumper { return __PACKAGE__->Dump1([@_]); }
sub Dumpf  { return __PACKAGE__->Dump1(@_); }
sub Dumpp  { print __PACKAGE__->Dump1(@_); }

# Note: All Data::Dumper methods can be called on Vis objects

# Split keys into "components" (e.g. 2_16.A has 3 components) and sort each
# component numerically if the corresponding items are both numbers.
sub _sortkeys {
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

sub _unix_compatible_os() {
  state $result //=
    # There must be a better way...
    (($^O !~ /win|dos/i && $^O =~ /ix$|ux$|bsd|svr|uni|osf|sv$/)
     || $^O eq 'drawin'
     || $^O eq 'cygwin'
    )
    && -w "/dev/null";
  $result;
}
sub _get_default_width() {
  local ($_, $!, $^E);
  my ($self) = @_;
  my $r;
  if ($ENV{COLUMNS}) {
    $r = $ENV{COLUMNS};
  }
  elsif (_unix_compatible_os) {
    if (-t STDERR) {
      no warnings;
      ($r = qx'tput cols') # on Linux, seems to print 80 even if no tty
      ||
      (($r) = ((qx'stty -a'//"") =~ /.*; columns (\d+);/s))
      ;
      { local $/ = "\n"; chomp $r if $r; }
      print "## Vis detected terminal width is ",u($r),"\n" if $self->{VisDebug};
    }
  }
  # elsif(...) { ... }
  else {
    warn "(fixme) Unrecognized OS $^O or no /dev/null"
  }
  return $r || 80;
}

sub _config_defaults {
  my $self = shift;

  ### Set double-quote style, but preserve any special
  ### $Data::Dumper::Useqq setting, e.g. 'utf8'
  $self->Useqq(1) unless $self->Useqq();

  $Maxwidth = $self->_get_default_width() if ! defined $Maxwidth;

  $self
    ->Quotekeys($Quotekeys)
    ->Sortkeys($Sortkeys)
    ->Terse($Terse)
    ->Indent($Indent)
    ->Debug($Debug)
    ->Stringify($Stringify)
    ->Useqq($Useqq)
    ->Maxwidth($Maxwidth)
    ->MaxStringwidth($MaxStringwidth)
    ->Truncsuffix($Truncsuffix)
    ->Sparseseen($Sparseseen)
}

# vnew  # $_ by default
# vnew(items...)
# anew(items...)
# hnew(items...)
# snew(strings...)
# dnew(strings...)
sub vnew {
  my $class = shift;
  my $obj = (bless($class->SUPER::new(@_ ? \@_ : [$_]), $class))->_config_defaults();
  $obj->{VisType} = 'v';
  $obj;
}
sub anew {
  my $class = shift;
  my $obj = (bless($class->SUPER::new([\@_]), $class))->_config_defaults();
  $obj->{VisType} = 'a';
  $obj;
}
sub hnew {
  my $class = shift;
  croak "hvis: Odd number of arguments can not be hash keys and values\n"
    if (@_ % 2) != 0;
  my $obj = (bless($class->SUPER::new([{@_}]), $class))->_config_defaults();
  $obj->{VisType} = 'a';
  $obj;
}
sub snew {
  my $class = shift;
  # The string we pass here to the Dumper constructor (which, by the way,
  # the user can get or replace via the inherited Value method) will not
  # actually be formatted as-is.  Instead, our overload Dump() method
  # will parse the string and then re-use the dumper object to format
  # each interpolated $varname etc. separately, thereby using any
  # configurations (such as Useqq or Indent) set by the user.
  my $obj = (bless($class->SUPER::new([@_]), $class))->_config_defaults();
  $obj->{VisType} = 's';
  $obj;
}
sub dnew {
  my $obj = snew(@_);
  $obj->{VisType} = 'd';
  $obj;
}

# new([items...],[names...])
# Functionally compatible with Data::Dumper->new(), including final newline.
# The differences include:
#   * Condensed output format
#   * Defaults to Purity(1)
sub new {
  my $class = shift;
  bless($class->SUPER::new(@_), $class)
    ->_config_defaults()
    ->Terse(0)
    ->Deepcopy(0)
    ->Purity(1)
    ;
}

sub Debug {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{VisDebug} = $v), return $s) : $s->{VisDebug};
}
sub Maxwidth {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{Maxwidth} = $v), return $s) : $s->{Maxwidth};
}
sub MaxStringwidth {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{MaxStringwidth} = $v), return $s) : $s->{MaxStringwidth};
}
sub Truncsuffix {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{Truncsuffix} = $v), return $s) : $s->{Truncsuffix};
}
sub Stringify {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{Stringify} = $v), return $s) : $s->{Stringify};
}

my $qstr_re = qr{ " (?: [^"\\]++ | \\. )*+ " | ' (?: [^'\\]++ | \\. )*+ ' }x;

# Match one balanced block (NOTE: Uses one capture group)
my $balanced_re = qr{
     (
          \{ (?: [^"'{}()\[\]]++ | (?>$qstr_re) | (?-1)++ )*+ \}
        | \[ (?: [^"'{}()\[\]]++ | (?>$qstr_re) | (?-1)++ )*+ \]
        | \( (?: [^"'{}()\[\]]++ | (?>$qstr_re) | (?-1)++ )*+ \)
     )
   }x;
my $balanced_squished_re = qr{  # match [a,...  but not [ a,...
     (
          \{ (?!\ ) (?: [^"'{}()\[\]]++ | (?>$qstr_re) | (?-1)++ )*+ \}
        | \[ (?!\ ) (?: [^"'{}()\[\]]++ | (?>$qstr_re) | (?-1)++ )*+ \]
        | \( (?!\ ) (?: [^"'{}()\[\]]++ | (?>$qstr_re) | (?-1)++ )*+ \)
     )
   }x;

# This matches as much innocuous stuff and/or balanced blocks as possible
# (with unpredictable capture group use)
my $balanced_or_safe_re = qr{
     #(?{ print "### START balanced_or_safe_re at $+[0]\n"; })
     ( [^"'{}()\[\]]++ | $qstr_re | $balanced_re )*+
     #(?{ print "### END balanced_or_safe_re at $+[0] $^N\n"; })
   }x;

# Similar but only matches *squished* balanced blocks. 
my $balancedsquished_or_safe_re = qr{
     ( [^"'{}()\[\]]++ | $qstr_re | $balanced_squished_re )*+
   }x;

my $key_re  = qr{ \w+ | $qstr_re }x;

sub _debug_show($$$$$$) {
  my ($linesaref, $I, $Iindent, $J, $Jindent, $restart) = @_;
  print "===== I=$I Iind=$Iindent J=$J Jind=$Jindent restart=$restart\n";
  my $nd = @$linesaref <= 9 ? 1 : @$linesaref <= 99 ? 2 : 3;
  for my $ix(0..$#$linesaref) {
    next if $linesaref->[$ix] eq "" && $ix != $I && $ix != $J;
    printf "[%${nd}d] ", $ix;
    print($ix==$I ? "I" : " ");
    print($ix==$J ? "J" : " ");
    print ":",($linesaref->[$ix] eq "" ? "(empty)":debugvis($linesaref->[$ix])), "\n";
  }
  print "=====================\n";
}

sub trunc_strings {
  my $self = shift;
  my $ret = 0;
  my $kind = ref($_[0]);
  if ($kind eq "") { # scalar
    my $maxlen = $self->{MaxStringwidth};
    if (length($_[0]) > $maxlen) {
      $_[0] = substr($_[0],0,$maxlen).$self->{Truncsuffix};
      $ret = 1;
    }
  }
  elsif($kind eq "ARRAY") {
    foreach (@{$_[0]}) {
      $ret = 1 if $self->trunc_strings($_);
    }
  }
  elsif($kind eq "HASH") {
    foreach (values %{$_[0]}) {
      $ret = 1 if $self->trunc_strings($_);
    }
  }
  else { die }
  return $ret;
}

sub _qquote_wrapper {
  goto &fixed_qquote if $_[1] eq 'utf8';
  goto &{$Vis::original_qquote};
}
sub _first_time_init() {
  $Vis::original_qquote = \&Data::Dumper::qquote;
  # Over-ride Data::Dumper::qquote to fix bug with Useqq('utf8')
  # N.B. This might not work if the sub ref has been cached by Perl,
  # e.g. if the user called Data::Dumper directly before using Vis.
  if ($Vis::Utf8patch 
      && &{$Vis::original_qquote}("\N{U+263A}","utf8") =~ /26/i) 
  {
    # Data::Dumper still has the bug with Useqq("utf8")
    { no warnings; *Data::Dumper::qquote = \&Vis::_qquote_wrapper; }
    # Recent versions of Data::Dumper always call the xs implementation
    # regardless of Useqq (and it doesn't handle Useqq='utf8' correctly).
    # So force using the Perl implementation in that case.
    $Vis::Useperl_for_utf8 =
      Data::Dumper->new(["\N{U+263A}"])->Useqq('utf8')->Dump() =~ /26/i;
  }
}

sub Dump {
  ### BUG HERE: Should do implicit new if extra args are passed
  ### (same as Data::Dumper::Dump)
  return Dump1(@_);
} 
  
# Dump1 is like Dump but assumes the callers frame is up one level
sub Dump1 {
  _first_time_init() unless defined $Vis::original_qquote;

  my ($self) = @_;

  if (! ref $self) {
    $self = $self->new(@_[1..$#_]);
  } else {
    goto &DB::DB_Vis_Dump if ($self->{VisType}//"") =~ /[sd]/;
  }

  my ($debug, $maxwidth, $maxstringwidth) 
    = @$self{qw/VisDebug Maxwidth MaxStringwidth/};

  if (($maxstringwidth//0) > 0) {
    # This could be more efficient if we were willing to reach directly
    # into Data::Dumper's internals and edit the data without copying
    my @todump = $self->Values; 
    if ($self->trunc_strings(\@todump)) {
      $self->Values( \@todump );
    }
  }

  my $useqq = $self->Useqq();
  $self->Useperl(1) if $useqq eq 'utf8' && $Vis::Useperl_for_utf8;

  # Allow top level objects of certain classes to stringify themselves;
  # all others will apper as "bless(...content...), 'modulename'" i.e. as
  # formatted by Data::Dumper.  Stringification is attempted by concatenating
  # a string to the object.
  #
  # The Stringify property may be a ref to an array of module (i.e. class) 
  # names and qr/regexs/ to match class names.  Items blessed into
  # a matching class will be allowed to stringify themselves.
  #
  # If Stringify == undef (the default), then an appropriate class names
  # are assumed if 'use bigint' or similar directives are in effect
  # for the caller (e.g. Math::BigInt).
  #
  # Setting Stringify = [] disables this feature completely.
  #
  # Note: This applies only to variables interplated by dvis() and friends which
  # directly contain object references, and single objects passed directly
  # to vis().  It does NOT apply to items passed to avis(), or objects
  # reached indirectly via a container (array or hash); the latter are always 
  # displayed as "bless(...) 'modname'".  This is because we do not directly 
  # touch such objects, they are always formatted by Data::Dumper before
  # we massage the result.
  #
  my $stringify = $self->Stringify();
  if (! defined $stringify) {
    # N.B. Trampolines arrange to interpose exactly one level, so 
    # caller(1) is always the user's context
    if (my $hinthash = (caller(1))[10]) {
      if (any {/^big/} keys %$hinthash) {
         push @$stringify, qr/^Math::Big/;
      }
    }
  }

  local $_;
  if (defined $stringify) {
    $stringify = [$stringify] unless ref($stringify) eq 'ARRAY';
    my @values = $self->Values;
    if (@values==1) {
      if (my $ref = ref($values[0])) {
        foreach my $mod (@$stringify) {
          if (ref($mod) ? $ref =~ /$mod/ : $ref eq $mod) {
            # Allow this object to stringify itself.  The result is a
            # raw unformatted string, not enclosed in quotes, 
            # followed by a newline (mimicing Data::Dumper output)
            $_ = $values[0]."\n";
          }
        }
      }
    }
  }

  # If not stringified above, call Data::Dumper to do it
  $_ //= $self->SUPER::Dump;

  my $pad = $self->Pad();
  if ($pad ne ""){
    s/^\Q${pad}\E//mg; # will be put back later
    $maxwidth -= length($pad);
    if ($maxwidth < 20) {
      #state $warned;
      #$warned=1, warn "Warning: Pad is too wide, Vis may exceed Maxwidth\n"
      #  if (! $warned);
      $maxwidth = $self->{Maxwidth};
    }
  }

  print "===== RAW =====\n${_}---------------\n" if $debug;
  
  #return $_ if $maxwidth == 0; # no condensation
  return $_ if /sub\s*\{/s && $self->Deparse; # we can't handle this

  # Split into logical lines, being careful to preserve newlines in strings.
  # The "delimiter" is the whole (logical) line, including final newline,
  # which is returned because it is in a (capture group).
  my @lines;
  my $limit = length($_);
  my $startpos = 0;
  while ($startpos != $limit) {
    if ($useqq) {
      #while (/\G"(?:[^"\\]++|\\.)*+"/gsc || /\G[^"\n]+/gsc) {}
      # Ack!  Perl implements (...)* using recursion even when partial
      # backtracking is not possible, and there is a 64K recursion limit.
      # Benchmarking shows that ~200 is the sweet spot.
      while (
        (/\G"/cgs 
         && do{ while(/\G(?:[^"\\]++|\\.){0,200}+/cgs){} /\G"/cgs })
        || /\G[^"\n]+/gsc
      ) {}
    } else {
      #while (/\G'(?:[^'\\]++|\\['\\])*+'/gsc || /\G[^'\n]+/gsc) {}
      while (
        (/\G'/cgs 
         && do{ while(/\G(?:[^'\\]++|\\['\\]){0,200}+/cgs){} /\G'/cgs })
        || /\G[^'\n]+/gsc
      ) {}
    }
    /\G\n/gsc or die "oops";
    push @lines, substr($_, $startpos, pos()-$startpos);
    $startpos = pos();
  }
  undef $_;
#  my $split_re = $self->Useqq()
#       ? qr/( (?: "(?:[^"\\]++|\\.)*+" | [^"\n]++ )* \n)/xs
#       : qr/( (?: '(?:[^'\\]++|\\['\\]|\\(?=[^'\\]))*+' | [^'\n]++ )* \n)/xs;
#  my @lines = (grep {defined($_) && $_ ne ""} split /$split_re/, $_);

  #print "### Useqq=",$self->Useqq()," split_re=/$split_re/ lines:\n  ",debugvis(\@lines),"\n";

  # Data::Dumper output with Indent(1) is very structured, with a
  # fixed 2-space indent increment:
  # [
  #   value,
  #   value,
  #   {
  #     key => [
  #       value,
  #       value
  #     ],
  #     key => {
  #       key => value
  #       longerkey => value,
  #     },
  #   },
  #   value
  # ]

  # Combine appropriate lines to make it more "horizontal"
  local $@;  # preserve user's $@ value
  eval {
    use warnings FATAL => 'all';
    my $restart = 0;
    while ($restart < $#lines)
    {
      print "RESTART at $restart \n" if $debug;
      LOOP:
      for (my $I=$restart, my $J=$restart+1, $restart=INT_MAX-1;
           $J <= $#lines;
           $I=$J, $J=$I+1)
      {
        # Find the next pair of lines which haven't been "deleted" yet
        while ($lines[$J] eq "") { next LOOP if ++$J > $#lines; }
        while ($lines[$I] eq "") { next LOOP if ++$I >= $J; }
  
        my ($Iprefix,$Icode) = ($lines[$I] =~ /^( *)(.*)\n\z/s);
        my $Iindent = length($Iprefix);
  
        my ($Jprefix,$Jcode) = ($lines[$J] =~ /^( *)(.*)\n\z/s);
        my $Jindent = length($Jprefix);
  
        my $J_is_closing = ($Jcode =~ /[\]\}]$/);
  
        _debug_show(\@lines, $I, $Iindent, $J, $Jindent, $restart) if $debug;
  
        if (($Iindent <= $Jindent)
            # I is not closing an aggregate
            && $Icode !~ /^[\]\}]/ 
  
            # J does not end with an un-closed block (we only join atoms or
            # [whole blocks]).  This prevents hard-to-see keys such as KEY2:
            #    { "KEY" => [ 1, 2, 3, 4,
            #      "five", "six", 7 ], "KEY2" => ... }
            && $Jcode !~ /^ (?> ${balanced_or_safe_re}) [\{\[] /x
           )
        {
          # The lines are elegible to be joined, if there is enough space
          # (or in any case if the joining would not increase line length)
          my $Ilen = $Iindent + length($Icode);
          my $Jlen = length($Jcode);
          my $sep  = $Icode =~ /,$/ && $Jcode =~ / => ${balanced_or_safe_re},?$/x
                      ? " " : "";
          my $adj = $Ilen + length($sep) - $Jindent; # posn change, + or -
          print "[Iind=$Iindent Ilen=$Ilen Jlen=$Jlen sep='${sep}' adj=$adj mw=$maxwidth]\n" if $debug;
  
          if ($Ilen + $Jlen + length($sep) <= $maxwidth || $adj <= 0) {
            substr($lines[$I],$Ilen) = $sep.substr($lines[$J], $Jindent); 
            $lines[$J] = "";
            print "## joined:",debugvis($lines[$I])," (len=",length($lines[$I]),")\n" if $debug;
            if ($J < $#lines && $adj < 0) {
              # Adjust the indentation of remaining items in the same block
              # to line up with the item just joined, the indent decreased.
              # This occurs when joining members onto an opening bracket.
              #my $extra_prefix = " " x $adj;
              for (my $K=$J+1; ;$K++) {
                die "bug" if $K > $#lines;
                $lines[$K] =~ /^( *)/;
                last if length($1) <= $Iindent; # end of nested block
                #if ($adj > 0) {
                #  $lines[$K] = $extra_prefix . $lines[$K];
                #} else { 
                  substr($lines[$K],0,-$adj) = "";
                #}              
              }
            }
            #$restart = $I if $I < $restart;
            $restart = 0 if $I < $restart;
            last LOOP if ++$J > $#lines;
            redo LOOP;
          } else {
            print "NO ROOM\n" if $debug;
          }
        } else {
          print "NOT ELEGIBLE\n" if $debug;
        }
      }
    }
  }; # eval
  unshift @lines, "VIS ERROR:$@" if $@; # show the error, but keep the Data::Dumper output

  $_ = join "", @lines;

  if ($self->{VisType}) {
    s/\s+\z//s;  # omit final newline except when emulating Data::Dumper
    if ($self->{VisType} eq 'a') {
      s/^( *)[\[{]/$1\(/ or confess "bug($_)"; # convert to "(list,of,args)"
      s/[\]}]$/\)/ or confess "bug($_)";
    }
  }

  s/^/$pad/meg if $pad ne "";

  return $_;
}

sub forceqsh($) {
  # Unlike Perl, the bourne shell does not recognize backslash escapes
  # inside '...'.  Therefore the quoting has to be interrupted for any
  # embedded single-quotes so they can be contatenated as \' or "'"
  #

  local $_ = shift;
  if (ref) {
    # If a ref to an aggregate, convert to a string
    $_ = __PACKAGE__->vnew($_)->Useqq(0)->Dump;
  }
  
  # Now we have a simple scalar.  
  # Don't use Data::Dumper because it will change
  # wide characters to \x{...} escapes.  For qsh() we assume the user
  # will encode the resulting string as needed, so leave wide chars as-is.
  if (/[^"\\]/ and not /[^\x{40}-\x{7E}]/) {
    return "\"${_}\"";  # prefer "..." if no escapes needed
  } else {
    s/'/'\\''/g; # foo'bar => foo'\''bar
    return "'${_}'";
  }
}

sub qsh(_;@) {  # N.B. "_" prototype defaults to $_
  my @args = @_;  # needed in case $_ is being used
  my @results = map {
                  defined $_
                    ? (/[^-\w_\/:\.,]/ || $_ eq "" || ref) ? forceqsh($_) : $_
                    : "undef";
                }
                @args;
  return wantarray ? @results : join(" ",@results);
}

# Quote paths for shell: Like qsh but doesn't quote an initial ~ or ~username
sub qshpath(_;@) {
  my @args = @_;
  my @results = map {
                  defined $_
                    ? do {
                        local $_ = $_;
                        my ($tilde_prefix, $rest) = /(^~[^\/\\]*\/?)?(.*)/s;
                        $rest eq "" ? $tilde_prefix : ($tilde_prefix // "").qsh($rest);
                      }
                    : "undef";
                }
                @args;
  return wantarray ? @results : join(" ",@results);
}

our @saved;
sub SaveAndResetPunct {
  # Save things which will later be restored, and reset to sane values.
  our @saved = ( $@, $!+0, $^E+0, $,, $/, $\, $^W );
  $,  = "";      # output field separator is null string
  $/  = "\n";    # input record separator is newline
  $\  = "";      # output record separator is null string
  $^W = 0;       # warnings are off
}

package DB;

# Interface functions, entered by goto from trampolines called by the user
#sub DB_Vis_svis(@)  { DB_Vis_Interpolate(Vis->snew(@_)); }
#sub DB_Vis_dvis(@)  { DB_Vis_Interpolate(Vis->dnew(@_)); }
#sub DB_Vis_svisq(@) { DB_Vis_Interpolate(Vis->snew(@_)->Useqq(0)); }
#sub DB_Vis_dvisq(@) { DB_Vis_Interpolate(Vis->dnew(@_)->Useqq(0)); }
#sub DB_Vis_Dump     { DB_Vis_Interpolate($_[0]); }

sub DB_Vis_svis(@)  { @_ = (Vis->snew(@_)); goto &DB_Vis_Interpolate; }
sub DB_Vis_dvis(@)  { @_ = (Vis->dnew(@_)); goto &DB_Vis_Interpolate; }
sub DB_Vis_svisq(@) { @_ = (Vis->snew(@_)->Useqq(0)); goto &DB_Vis_Interpolate; }
sub DB_Vis_dvisq(@) { @_ = (Vis->dnew(@_)->Useqq(0)); goto &DB_Vis_Interpolate; }
sub DB_Vis_Dump     { @_ = ($_[0]); goto &DB_Vis_Interpolate; }

# Interpolate strings for svis and dvis.   This must be in package
# DB and the closest scope not in package DB must be the user's context.
#
# To accomplish this, interface functions in package Vis are trampolines
# which just goto a function in package DB without modifying @_.
#
sub DB_Vis_Interpolate {

  &Vis::SaveAndResetPunct;

  my ($self) = @_;
  my ($debug, $maxwidth) = @$self{'VisDebug','Maxwidth'};
  my $pad = $self->Pad();
  my $d_or_s = $self->{VisType} eq 'd' ? "d" : "s";

  local $Vis::error_prefix = "$self->{VisType}vis: "; # see Vis_Eval

  # NOTE: These may or may not use capture groups internally, so capture
  # groups must NOT be opened after the first point of use in a regexp!
  
  state $interior_re = qr/${balanced_or_safe_re}/;

  state $scalar_index_re = qr{
    ( (?:->)? (?: \{ $interior_re \} | \[ $interior_re \] ) )+
  }x;
  state $slice_re = qr{
    (?: \{ $interior_re \} | \[ $interior_re \] )
  }x;
  state $variable_re = qr/   # sigl could be $ or @
      \#?\w+(?:::\w+)*   # @name $pkg::name $#name $1
    | \#?\$\w+(?:::\w+)* # $$ref $#$ref
    | \^\w               # $^N   (control-character 'punctuation' variable)
    | \#?[^\w\s\{\$]     # $#- $^ $? (regular 'punctuation' variable)
    | (?<=\$)\$          # $$
    | \{ $interior_re \} # ${ref expression} or ${^SPECIALNAME}
  /x;

  my @actions;
  {
    # N.B. The evals are executed below after $_ is restored.
    local $_ = join "", map {defined($_) ? $_ : '<undef arg>'} $self->Values();
    while (1) {
      #print "### pos()=",Vis::u(pos()),"\n" if $debug;
      if (
        # \G does not work with (?|...) in Perl 5.12.4
        # https://rt.perl.org/rt3//Public/Bug/Display.html?id=112894

        # $name $name[expr] $name->{expr} ${refexpr}->[expr] etc.
        /\G (?!\\)(\$)( $variable_re ${scalar_index_re}? )/xsgc
        ||
        # ${name} ${name->[expr]} etc. (loosing the curlies)
        /\G (?!\\)(\$) \{ ( $variable_re ${scalar_index_re}? ) \}/xsgc
        ||
        # @name @name[slice] @name{slice} @{refexpr}[slice] etc.
        /\G (?!\\)(\@)( $variable_re ${slice_re}? )/xsgc
        ||
        # @{name} @{name[slice]} etc. (loosing the curlies)
        /\G (?!\\)(\@) \{ ( $variable_re ${slice_re}? ) \}/xsgc
        ||
        # %name
        /\G (?!\\)(\%)( $variable_re )/xsgc
       )
      {
        push @actions, ['e',$1,$2];  # eval $1$2
        if ($debug) {
          print "### regex match ($_):";
          for my $i (1..$#+) {
            eval "print \" $i=«\$$i»\" if defined \$$i;"; die "bug" if $@;
          }
          print "\n";
        }
      }
      elsif (/\G ( (?: [^\$\@%\\]+ | \\. )+ ) /xsgc) {
        push @actions, ['t',$1];  # interpolate plain text including \$ etc.
      }
      elsif (/\G   (?!\\)([\$\@\%])(\s|$)   /xsgc) {
        Carp::carp "Warning: Dangling '$1' in string interpolated by ${d_or_s}vis\n";
        push @actions, ['t',"\\${1}${2}"];  # treat as literal text
      }
      else {
        if (/\G./) {
          my $tmp = $_; $tmp =~ s/[^\x{20}-\x{7E}]/?/g;
          die "Vis bug: next:",substr($tmp,pos,4),
              "... pos=",pos," in:\n$tmp\n".(" "x pos)."^\n "
        }
        last;
      }
    }
  }

  # $_ and $1 etc. have now been restored to the caller's values
  # DO NOT use regex with (capture groups) between here and the eval below
  # except inside { interior blocks }.

  print "### actions=",Vis::debugavis(@actions),"\n" if $debug;
  my $result = $pad;
  foreach my $action (@actions) {
    my $act = $action->[0];
    if ($act eq 'e') {
      my ($sigl, $rhs) = @$action[1,2];
      # "$sigl$rhs" is a Perl expression giving the desired value.
      # Note that the curlies were dropped from ${name} in the string
      # (because braces can not be used that way in expressions, only strings)

      my @items = Vis_Eval("$sigl$rhs");
      print "### items=",Vis::debugavis(@items),"\n" if $debug;

      my $varlabel = "";
      if ($d_or_s eq 'd') {
        # Don't show initial $ if it's a non-special scalar variable name
        $varlabel = (($sigl eq '$' && $rhs =~ /^[A-Za-z]/) ? "" : $sigl)
                        . $rhs . "=" ;
      }

      my $extra_padlen;
      { $result =~ /([^\n]*)\z/s;  # already includes user's Pad
        $extra_padlen = length($1) - length($pad) + length($varlabel);
        # Wrap if we're getting ridiculous
        if ($extra_padlen > $maxwidth-15 && length($1) > length($pad)
            && $maxwidth > 0) {
          $result .= "\n$pad";
          redo;
        }
      }

      my $autopad = $pad.(" " x $extra_padlen);
      if ($sigl eq '$') {
        $self->Reset()->Pad($autopad)->Values([$items[0]])->{VisType}='v';
      }
      elsif ($sigl eq '@') {
        $self->Reset()->Pad($autopad)->Values([\@items])->{VisType} = 'a';
      }
      elsif ($sigl eq '%') {
        my %hash = @items;
        $self->Reset()->Pad($autopad)->Values([\%hash])->{VisType} = 'a';
      }
      else { die "bug" }

      my $s = $self->Dump1;
      #print "### Dump result:\n«$s»\n";
      substr($s, 0, length($autopad)) = $varlabel;
      $result .= $s;
    }
    elsif ($act eq 't') {
      # Interpolate \n etc.
      my $rawtext = $action->[1];
      if ($rawtext =~ /\b((?:ARRAY|HASH)\(0x[^\)]*\))/) {
        state $warned=0;
        Carp::carp "Warning: String passed to ${d_or_s}vis may have been interpolated by Perl\n(use 'single quotes' to avoid this)\n" unless $warned++;
      }
      print "### PLAIN «$rawtext»\n" if $debug;
      my $saved_eval_err = $@;
      #??? WHY ARENT WE USING local $@ HERE???
      { #local $@;
        my $itext = eval qq{<<"ViSEoF"
$rawtext
ViSEoF
};
        Carp::confess "bug($rawtext)$@ " if $@;
        #??? $/ is now reset by SaveAndResetPunct, should be ok to use chomp
        $itext =~ s/\n\z//;  # don't use chomp(): $/ may have been changed
        $itext =~ s/\n(?!\z)/"\n$pad"/seg if $pad;
        $result .= $itext;  # plain text
      }
      $@ = $saved_eval_err;
    } else {
      die "bug";
    }
    print "### After act '$act' : result=«${result}»\n" if $debug;
  }
  ( $@, $!, $^E, $,, $/, $\, $^W ) = @saved;
  return $result;
}

1;
__END__

=head1 NAME

Vis - Enhance Data::Dumper for use in messages

=head1 SYNOPSIS

  use Vis;

  my %hash = ( complicated => ['lengthy', 'stuff', [1..20]] );
  my $ref = \%hash;

  # Interpolate strings, stingifying aggregates with auto-indenting
  print svis 'ref=$ref\n hash=%hash\n ARGV=@ARGV\n'; # SINGLE quoted!

  # dvis (d for debug) is like svis but shows variable= prefixes
  print dvis 'FYI $ref %hash @ARGV\n';

  # Format one item at a time (sans final newline)
  print "ref=", vis($ref), "\n";
  print "ARGV=", avis(@ARGV), "\n";  # (array,values,in,parens)

  dvisq(), svisq(), visq(), and avisq() show strings 'single quoted'

  # Equivalent OO APIs allow using configuration methods
  print Vis->snew('var=$var ary=@ary hash=%hash\n')
             ->Maxwidth(120)
             ->Maxdepth($levels)
             ->Pad("| ")
             ->Dump;                                  # like svis()
  print Vis->dnew('Howdy! $var @ary %hash\n')->Dump;  # like dvis()
  print Vis->vnew($scalar)->Dump, "\n";               # like vis()
  print Vis->anew(@array)->Dump, "\n";                # like avis()

  { use bigint;
    # Show large number values
    my $big = 1_000_000_000 * 1_234_567_890;
    print "Value is ", vis($big), "\n";
    # disable evaluating big numbers
    $Vis::Stringify = []; 
    print "Object is ", vis($big), "\n";
  }

  # Avoid unnecessary hex escaping in Unicode strings
  $Vis::Useqq = 'utf8';
  binmode(STDOUT, ":utf8");
  my $var = "Just let me read my Unicode \N{U+263A} ";
  print svis('var=$var\n');

  print Dumper($ref);                                 # Data::Dumper
  print Vis->new([$ref],['$ref'])->Dump;              #  compatible APIs

  # Change undef to "undef", but otherwise return the argument as-is
  print u($value),"\n";

  # Quote arguments for the shell
  foreach ($ENV{HOME}, "/dir/safe", "Uck!",
           "My Documents", "Qu'ote", 'Qu"ote')
  {
    system( "set -x; /bin/ls -ld ".qsh($_) );
  }

=head1 DESCRIPTION

The Vis package provides additional interfaces to Data::Dumper
which may be more convenient for error/debug messages
(plus a few related utilities).
Aggregate data structures are formatted in a condensed format.
Wide characters can optionally be preserved in readable form.

=over 2

=item *

A final newline is not automatically included when using the new interfaces.

=item *

Multiple array and hash members are shown on the same line, 
subject to a maximum width (see "Maxwidth" below).

For example  B<print "ref=", vis($ref), "\n";>  produces

  ref={
    complicated => [ "lengthy", "stuff",
      [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
        17, 18, 19, 20
      ]
    ]
  }

=item *

Default configuration settings are optimized for human consumption.

=item *

Hash keys appear sorted with numeric "components" sorted numerically.
For example: "A.20_999" "A.100_9" "A.100_80" and "B" sort in that order.

=back

Vis is a subclass of Data::Dumper and is a drop-in replacement.  The old
APIs are all available through Vis but produce condensed output formatting.
Data::Dumper may still be called directly (see "PERFORMANCE").

=head2 svis 'string to be interpolated', ...

The arguments are concatenated, interpolating variables and escapes
as in in Perl double-quotish strings except that values are formatted
unambiguously using C<vis()> or C<avis()> 
for $ or @ expressions, respectively.
In addition, C<%name> is interpolated as S<<< C<< (key => value ...) >> >>>.

Multi-line structures are indented to line up with their starting position,
taking into account any preceeding text on the same line.

The argument string(s) should be written SINGLE QUOTED so Perl will not
interpolate them before passing to svis().

=head2 dvis 'string to be interpolated', ...

'd' is for 'debug display'.  C<dvis> is identical to C<svis>
except that interpolated expressions are prefixed by
the name of the variable or the expression.  For example,

  my $foo = 'Nazdrave
  ';
  my @bar = (0..18);
  my %hash = (A=>100,B=>200);
  print dvis '$foo @bar[3..5] %hash\n';

produces

  foo="Nazdrave\n" @bar[3..5]=(3, 4, 5) %hash=(A => 100, B => 200)

Note: "@_" shows your original sub arguments as they were before any C<shift> 
statements were executed.   This is because svis/dvis use Perl's debug mechanism,
which has this quirk.

=head2 vis

=head2 vis $scalar

Format a scalar for printing ($_ by default), without a final newline.

=head2 avis @array

Format the arguments as a list in parenthesis: C<(arg1,arg2,arg3,...)>.
This allows @arrays to be shown without taking a reference.

=head2 svisq, dvisq, visq, and avisq

These alternatives use 'single quotes' when formatting strings.

=head2 OO interfaces

OO interfaces allow setting Configuration options on a case-by-case basis.

B<< Vis->snew >>, B<< Vis->dnew >>, B<< Vis->vnew >> and B<< Vis->anew >>
are constructors corresponding to the functions
B<svis>, B<dvis>, B<vis> and B<avis>, respectively.  See SYNOPSIS above.

Additionally, B<< Vis->new >> provides the same API as Data::Dumper->new.

=head2 Configuration Variables or Methods

=over 4

=item $Vis::Maxwidth  I<or>  I<$OBJ>->Maxwidth(I<[NEWVAL]>)

Sets or gets the maximum number of characters for formatted lines,
including any prefix set via I<Pad()>.
Default is the terminal width or 80 if STDERR is not a terminal.
If Maxwidth=0 output is not folded, appearing similar to Data::Dumper
(but still without a final newline).

=item $Vis::MaxStringwidth  I<or>  I<$OBJ>->MaxStringwidth(I<[NEWVAL]>)

Sets or gets the maximum number of characters to be displayed
for an individual string.  Longer strings are truncated
and an ellipsis (...) appended.  Zero or undef for no limit.

=back

The following Methods have the same meaning as in Data::Dumper except that
default values come from global variables in package B<Vis> :

=over 4

=item $Vis::Quotekeys           I<or>  I<$OBJ>->Quotekeys(I<[NEWVAL]>)

=item $Vis::Sortkeys            I<or>  I<$OBJ>->Sortkeys(I<[NEWVAL]>)

=item $Vis::Terse               I<or>  I<$OBJ>->Terse(I<[NEWVAL]>)

=item $Vis::Indent              I<or>  I<$OBJ>->Indent(I<[NEWVAL]>)

=item $Vis::Sparseseen          I<or>  I<$OBJ>->Sparseseen(I<[NEWVAL]>)

=item $Vis::Useqq               I<or>  I<$OBJ>->Useqq(I<[NEWVAL]>)

Note that the B<Useqq(0)> is called implicitly
by B<svisq>, B<dvisq>, B<visq>, and B<avisq>.

B<Useqq('utf8')> is like B<Useqq(1)> but also leaves printable wide 
characters as-is so you can read them (anything in the POSIX [:graph:] set).
Input strings must contain characters (not bytes), and the output must 
later be encoded to UTF-8 or another encoding which can represent the data
(see perlunitut).
[Data::Dumper has long contained code to implement this but a bug prevented it from working; B<Vis> overrides a Data::Dumper internal function to repair it.]

=back

Other inherited methods may also be used, with default values
from global variables in Data::Dumper .  Here is a partial list:

=over 4

=item $Data::Dumper::Deepcopy   I<or>  I<$OBJ>->Deepcopy(I<[NEWVAL]>)

=item $Data::Dumper::Freezer    I<or>  I<$OBJ>->Freezer(I<[NEWVAL]>)

=item $Data::Dumper::Maxdepth   I<or>  I<$OBJ>->Maxdepth(I<[NEWVAL]>)

=item $Data::Dumper::Pad        I<or>  I<$OBJ>->Pad(I<[NEWVAL]>)

=item $Data::Dumper::Purity     I<or>  I<$OBJ>->Purity(I<[NEWVAL]>)

=item $Data::Dumper::Toaster    I<or>  I<$OBJ>->Toaster(I<[NEWVAL]>)

=item $Data::Dumper::Varname    I<or>  I<$OBJ>->Varname(I<[NEWVAL]>)

=back

These inherited methods should not be used with Vis :

=over 4

=item $Data::Dumper::Pair      I<or>  I<$OBJ>->Pair(I<[NEWVAL]>)

=item $Data::Dumper::Deparse   I<or>  I<$OBJ>->Deparse(I<[NEWVAL]>)

=back


=head2 u $data, ...

The arguments ($_ by default) are returned unchanged, except that undefined 
argument(s) are replaced by the string "undef".  Refs are not stringified.

=head2 qsh

=head2 qsh $word, ...

=head2 qshpath $path_with_tilde_prefix, ...

The "words" ($_ by default) are quoted if necessary for parsing
by /bin/sh, which has different quoting rules than Perl.
In scalar context multiple items are concatenated separated by spaces.
"Double quotes" are used when no escapes would be needed, 
otherwise 'single quotes'.

Items which contain only "safe" characters are returned unquoted.

References are formatted as with C<vis()> and the resulting string quoted.
Undefined values appear as C<undef> without quotes.

C<qshpath> is like C<qsh> except that an initial ~ or ~username is left
unquoted.  Useful with shells such as bash or csh.

=head2 forceqsh $word

The argument is quoted for /bin/sh even if not necessary.

Unlike C<qsh>, C<forceqsh> requires a single argument which must be defined.

=head1 PERFORMANCE

Vis calls Data::Dumper and then condenses the output.
C<svis> and C<dvis> must also parse the strings to be interpolated.
For most purposes the extra overhead is not significant.

Single-quote style (C<Useqq(0)>) runs faster because
the underlying Data::Dumper implementation uses XS (C code).

For high-volume applications, such as to serialize a large data set,
C<< Data::Dumper->new() >> may be called directly.

=head1 SEE ALSO

Data::Dumper

=head1 AUTHOR

Jim Avera  (jim.avera AT gmail dot com)

=cut

#!/usr/bin/perl
# Tester for module Vis.  TODO: Convert to CPAN module-test setup
use strict; use warnings ; use feature qw(state switch);
use utf8; 
use open IO => 'utf8', ':std';
select STDERR; $|=1; select STDOUT; $|=1;
use Scalar::Util qw(blessed);
use Carp;
use English qw( -no_match_vars );;
#use lib "$ENV{HOME}/lib/perl";

my $unicode_str;

# We want to test the original version of the internal function 
# Data::Dumper::qquote to see if the useqq="utf8" but has been fixed.
# We used to just load Data::Dumper here in a BEGIN block before Vis
# is loaded and use Data::Dumper to test it, but Perl now seems to cache
# the sub lookup immediately in Data::Dumper, making the override 
# ineffective.
#
my $utf8fix_not_needed;
BEGIN{ 
  # This test must be done before loading Vis, which over-rides an internal
  # function to fix the bug
  $unicode_str = join "",map {eval sprintf "\" \\N{U+%04X}\"",$_} 
                             (0x263A..0x2650); 
  require Data::Dumper;
  print "Loaded ", $INC{"Data/Dumper.pm"}, " qquote=", \&Data::Dumper::qquote,"\n";
  { my $s = Data::Dumper->new([$unicode_str],['unicode_str'])->Terse(1)->Useqq('utf8')->Dump;
    chomp $s;
    $s =~ s/^"(.*)"$/$1/s or die "bug";
    if ($s ne $unicode_str) {
      #warn "Data::Dumper with Useqq('utf8'):$s\n";
      warn "WARNING: Useqq('utf8') is broken in your Data::Dumper.\n"
    } else {
      print "Useqq('utf8') seems to have been fixed in Data::Dumper,\n";
      $utf8fix_not_needed = 1;
    }
  }
}

use Vis;
#use Vis 'u';  now exported by default...

print "Loaded ", $INC{"Vis.pm"}, " _qquote_wrapper=", \&Vis::_qquote_wrapper, "\n";

# Do an initial read of $[ so arybase will be autoloaded
# (prevents corrupting $!/ERRNO in subsequent tests)
eval '$[' // die;
      
sub tf($) { $_[0] ? "true" : "false" }

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
die "Expected initial Vis::Maxwidth to be undef" if defined $Vis::Maxwidth;
{ local $ENV{COLUMNS} = 66;
  vis(123); 
  die "Vis::Maxwidth does not honor ENV{COLUMNS}" unless $Vis::Maxwidth == 66;
  undef $Vis::Maxwidth;  # re-enable auto-detect
}
if (Vis::_unix_compatible_os()) {
  delete local $ENV{COLUMNS};
  my $expected = `tput cols`;  # may default to 80 if no tty
  vis(123); 
  die "Vis::Maxwidth no defaulted correctly" unless $Vis::Maxwidth == $expected;
  undef $Vis::Maxwidth;  # re-enable auto-detect
}
if (Vis::_unix_compatible_os()) {
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
  die "Vis::Maxwidth defaulted to ", ($? >> 8)|($? & !0xFF), " (not 80 as expected)\n"
    unless $? == (80 << 8);
}

##################################################
# Check Useqq('utf8') support
##################################################
{
  print "                   unicode_str=\"$unicode_str\"\n";
  my $s = Vis->new([$unicode_str."\x{FFFF}"])->Terse(1)->Useqq('utf8')->Dump;
  chomp $s;
  $s =~ s/^"(.*)"$/$1/s or die "bug";
  print "         Vis with Useqq('utf8'):$s\n";
  if ($s ne $unicode_str.'\x{ffff}') {
    die "***Useqq('utf8') fix does not work!\n","s=<${s}>\n";
  } else {
    print "Useqq('utf8') works with Vis.\n";
  }
}

my $undef_as_false = undef;
if (! ref Vis->new([1])->Useqq(undef)) {
  warn "WARNING: Data::Dumper methods do not recognize undef boolean args as 'false'.\n";
  $undef_as_false = 0;
}

foreach ( 
          ['Maxwidth',0,1,80,9999],
          ['MaxStringwidth',undef,0,1,80,9999],
          ['Truncsuffix',"","...","(trunc)"],
          ['Debug',undef,0,1],
          ['Useqq',0,1,'utf8'],
          ['Quotekeys',0,1],
          ['Sortkeys',0,1,sub{return sort keys %{shift @_}}],
          ['Terse',0,1],
          ['Indent',0,1,2,3],
          ['Sparseseen',0,1,2,3],
        )
{
  my ($name, @values) = @$_;
  my $testval = [123];
  foreach my $value (@values) {
    foreach my $ctor (qw(vnew anew snew dnew)) {
      {
        my $v = eval "{ local \$Vis::$name = \$value;
                        Vis->\$ctor([\"test \$testval\"])->$name();
                      }";
        die "bug:$@ " if $@;
        die "\$Vis::$name value is not preserved by Vis->$ctor\n",
            "(Set \$Vis::$name=",u($value)," but $name() returned ",u($v),")\n"
         unless (! defined $v and ! defined $value) || ($v eq $value);
      }
      {
        my $v = eval "{ Vis->\$ctor([\"test \$testval\"])->$name(\$value)->$name() }";
        die "bug:$@ " if $@;
        die "Vis::$name(v) does not set it's value!\n",
            "(called $name(",u($value),") but $name() returned ",u($v),")\n"
         unless (! defined $v && ! defined $value) || ($v eq $value);
      }
    }
  }
}

# ---------- Check formatting or interpolation --------

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

sub check($$@) {
  my ($code, $expected_arg, @actual) = @_;
  confess "BUG(EVAL ERR): $@" if $@;
  my @expected = ref($expected_arg) ? @$expected_arg : ($expected_arg);
  $actual[0] //= 'undef';
  confess "\nTEST FAILED: $code\n"
         ."Expected ".(@expected)." results, but got ".(@actual).":\n"
         ."(@actual)\n"
    if @expected != @actual;
  foreach my $actual (@actual) {
    my $expected = shift @expected;
    confess "\nTEST FAILED: $code\n"
           ."Expected:\n${expected}«end»\n"
           ."Got:\n${actual}«end»\n"
      unless $actual eq $expected;
  }
}

$Vis::Maxwidth = 72;

@ARGV = ('fake','argv');
$. = 1234;
$ENV{EnvVar} = "Test EnvVar Value";

my %toplex_h = ("" => "Emp", A=>111,"B B"=>222,C=>{d=>888,e=>999},D=>{});
my @toplex_a = (0,1,"C",\%toplex_h,[],[0..9]);
my $toplex_ar = \@toplex_a;
my $toplex_hr = \%toplex_h;

our %global_h = %toplex_h;
our @global_a = @toplex_a;
our $global_ar = \@global_a;
our $global_hr = \%global_h;

our %maskedglobal_h = (key => "should never be seen");
our @maskedglobal_a = ("should never be seen");
our $maskedglobal_ar = \@maskedglobal_a;
our $maskedglobal_hr = \%maskedglobal_h;

our %localized_h = (key => "should never be seen");
our @localized_a = ("should never be seen");
our $localized_ar = \@localized_a;
our $localized_hr = \%localized_h;

our $a = "global-a";  # used specially used by sort()
our $b = "global-b";

package A::B::C;
our %ABC_h = %main::global_h;
our @ABC_a = @main::global_a;
our $ABC_ar = \@ABC_a;
our $ABC_hr = \%ABC_h;

package main;

$_ = "GroupA.GroupB";
/(.*)\W(.*)/sp or die "nomatch"; # set $1 and $2

{ my $code = 'qsh("a b")';           check $code, "'a b'",  eval $code; }
{ my $code = 'qsh(undef)';           check $code, "undef",  eval $code; }
{ my $code = 'qsh("a b","c d","e",undef,"g",q{\'ab\'"cd"})';       
   check $code, ["'a b'","'c d'","e","undef","g","''\\''ab'\\''\"cd\"'"], eval $code; }
{ my $code = 'qshpath("a b")';       check $code, "'a b'",  eval $code; }
{ my $code = 'qshpath("~user")';     check $code, "~user",  eval $code; }
{ my $code = 'qshpath("~user/a b")'; check $code, "~user/'a b'", eval $code; }
{ my $code = 'qshpath("~user/ab")';  check $code, "~user/ab", eval $code; }
{ my $code = 'qsh("~user/ab")';      check $code, "'~user/ab'", eval $code; }
{ my $code = 'qsh($_)';              check $code, "${_}",   eval $code; }
{ my $code = 'qsh()';                check $code, "${_}",   eval $code; }
{ my $code = 'qsh';                  check $code, "${_}",   eval $code; }
{ my $code = 'qshpath($_)';          check $code, "${_}",   eval $code; }
{ my $code = 'qshpath()';            check $code, "${_}",   eval $code; } 
{ my $code = 'qshpath';              check $code, "${_}",   eval $code; } 
{ my $code = 'forceqsh($_)';         check $code, "'${_}'", eval $code; }

# Basic checks
{ my $code = 'vis($_)'; check $code, "\"${_}\"", eval $code; }
{ my $code = 'vis()'; check $code, "\"${_}\"", eval $code; }
{ my $code = 'vis'; check $code, "\"${_}\"", eval $code; }
{ my $code = 'avis($_,1,2,3)'; check $code, "(\"${_}\",1,2,3)", eval $code; }
{ my $code = 'hvis("foo",$_)'; check $code, "(foo => \"${_}\")", eval $code; }
{ my $code = 'avis(@_)'; check $code, '()', eval $code; }
{ my $code = 'hvis(@_)'; check $code, '()', eval $code; }
{ my $code = 'svis(q($_ con),q(caten),q(ated\n))';
  check $code, "\"${_}\" concatenated\n", eval $code;
}
{ my $code = 'dvis(q($_ con),q(caten),q(ated\n))';
  check $code, "\$_=\"${_}\" concatenated\n", eval $code;
}
{ my $code = 'avis(undef)'; check $code, "(undef)", eval $code; }
{ my $code = 'hvis("foo",undef)'; check $code, "(foo => undef)", eval $code; }
{ my $code = 'vis(undef)'; check $code, "undef", eval $code; }
{ my $code = 'svis("foo",undef)'; check $code, "foo<undef arg>", eval $code; }
{ my $code = 'dvis("foo",undef)'; check $code, "foo<undef arg>", eval $code; }
{ my $code = 'dvisq("foo",undef)'; check $code, "foo<undef arg>", eval $code; }

{ my $code = q/my $s; my @a=sort{ $s=dvis('$a $b'); $a<=>$b }(3,2); "@a $s"/ ;
  check $code, '2 3 a=3 b=2', eval $code; 
}

# Check that $1 etc. can be passed (this was once a bug...)
{ my $code = '" a~b" =~ / (.*)/ && qsh($1)'; check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)/ && qshpath($1)'; check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)/ && forceqsh($1)'; check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)/ && vis($1)'; check $code, '"a~b"', eval $code; }

{ my $code = 'my $vv=123; \' a $vv b\' =~ / (.*)/ && dvis($1)'; check $code, 'a vv=123 b', eval $code; }

# bigint, bignum, bigrat support (works only for top-level arguments)
{
  use bignum;

  my $bigfdisp = '9988776655443322112233445566778899.8877';
  my $bigf = eval $bigfdisp // die;
  die unless blessed($bigf) =~ /^Math::BigFloat/;

  my $bigidisp = '9988776655443322112233445566778899887766';
  my $bigi = eval $bigidisp // die;
  die unless blessed($bigi) =~ /^Math::BigInt/;

  { my $code = 'Vis->vnew($bigf)->Dump'; check $code, "${bigfdisp}", eval $code; }
  { my $code = 'vis($bigf)'; check $code, "${bigfdisp}", eval $code; }
  { my $code = 'visq($bigf)'; check $code, "${bigfdisp}", eval $code; }
  { my $code = 'svis("foo","\$"."bigf")'; check $code, "foo${bigfdisp}", eval $code; }
  { my $code = 'svisq("foo","\$"."bigf")'; check $code, "foo${bigfdisp}", eval $code; }
  { my $code = 'dvis("foo","\$"."bigf")'; check $code, "foobigf=${bigfdisp}", eval $code; }
  { my $code = 'dvisq("foo","\$"."bigf")'; check $code, "foobigf=${bigfdisp}", eval $code; }

  { my $code = 'vis($bigi)'; check $code, "${bigidisp}", eval $code; }
  { my $code = 'visq($bigi)'; check $code, "${bigidisp}", eval $code; }
  { my $code = 'svis("foo","\$"."bigi")'; check $code, "foo${bigidisp}", eval $code; }
  { my $code = 'svisq("foo","\$"."bigi")'; check $code, "foo${bigidisp}", eval $code; }
  { my $code = 'dvis("foo","\$"."bigi")'; check $code, "foobigi=${bigidisp}", eval $code; }
  { my $code = 'dvisq("foo","\$"."bigi")'; check $code, "foobigi=${bigidisp}", eval $code; }
  
  # Confirm that Stringify=[] disables
  { local $Vis::Stringify = [];
    my $s = vis($bigf); die "bug($s)" unless $s =~ /^\(?bless.*BigFloat/s; 
  }

  # Confirm that indirectly-referenced objects are _not_ self-stringified
  { my $s = avis($bigf); die "Unexpected($s)" unless $s =~ /^\(?bless.*BigFloat/s; }
  { my $s = avis($bigi); die "Unexpected($s)" unless $s =~ /^\(?bless.*BigInt/s; }
}
{
  use bigrat;

  my $ratdisp = '1/9';
  my $rat = eval $ratdisp // die;
  die unless blessed($rat) =~ /^Math::BigRat/;

  { my $code = 'vis($rat)'; check $code, "${ratdisp}", eval $code; }
  { my $code = 'Vis->vnew($rat)->Dump'; check $code, "${ratdisp}", eval $code; }
  { my $code = 'visq($rat)'; check $code, "${ratdisp}", eval $code; }
  { my $code = 'svis("foo","\$"."rat")'; check $code, "foo${ratdisp}", eval $code; }
  { my $code = 'svisq("foo","\$"."rat")'; check $code, "foo${ratdisp}", eval $code; }
  { my $code = 'dvis("foo","\$"."rat")'; check $code, "foorat=${ratdisp}", eval $code; }
  { my $code = 'dvisq("foo","\$"."rat")'; check $code, "foorat=${ratdisp}", eval $code; }
}
{
  use Math::BigFloat;
  my $bigfdisp = '9988776655443322112233445566778899.8877';
  my $bigf = Math::BigFloat->new($bigfdisp);

  my $bigidisp = '9988776655443322112233445566778899887766';
  my $bigi = Math::BigInt->new($bigidisp);

  { local $Vis::Stringify = [qr/^Math::BigFloat/];
    { my $code = 'vis($bigf)'; check $code, "${bigfdisp}", eval $code; }
    { my $code = 'visq($bigf)'; check $code, "${bigfdisp}", eval $code; }
    { my $code = 'svis("foo","\$"."bigf")'; check $code, "foo${bigfdisp}", eval $code; }
    { my $code = 'svisq("foo","\$"."bigf")'; check $code, "foo${bigfdisp}", eval $code; }
    { my $code = 'dvis("foo","\$"."bigf")'; check $code, "foobigf=${bigfdisp}", eval $code; }
    { my $code = 'dvisq("foo","\$"."bigf")'; check $code, "foobigf=${bigfdisp}", eval $code; }
  
    my $s=vis($bigi); die "Unexpected($s)" unless $s =~ /^\(?bless.*BigInt/s; 
    foreach my $modmatch ('qr/^Math::BigInt/',
                          '[qr/^Math::BigInt/]',
                          '["No::Match", qr/^Math::BigInt/]',
                          '[qr/^Math::BigFloat/, qr/^Math::BigInt/]',
                         )
    { my $code = 'Vis->vnew($bigi)->Stringify('.$modmatch.')->Dump';
      check $code, "${bigidisp}", eval $code; 
    }
  }
}

# Check string truncation
{ local $Vis::MaxStringwidth = 3;
  { my $code = 'vis("abcde")'; check $code, "\"abc...\"", eval $code; }
  { my $code = 'vis(["abcDE","xyzAB"])'; check $code, "[\"abc...\",\"xyz...\"]", eval $code; }
  { my $code = 'vis(["abcDE", ["xyzAB", {longkey => "fghI"}, ]])'; 
       check $code, '["abc...",["xyz...",{longkey => "fgh..."}]]', eval $code; }
  local $Vis::Truncsuffix = "(trunc)";
  { my $code = 'vis("abcde")'; check $code, "\"abc(trunc)\"", eval $code; }
  foreach my $MSw (-1, 0, undef) {
    local $Vis::MaxStringwidth = $MSw;
    { my $code = 'vis(["abcDE", ["xyzAB", {longkey => "fghI"}, ]])'; 
         check $code, '["abcDE",["xyzAB",{longkey => "fghI"}]]', eval $code; }
  }
}

# There was a bug for s/dvis called direct from outer scope, so don't use eval:
check 'global divs %toplex_h', 
      '%toplex_h=("" => "Emp", A => 111, "B B" => 222,'."\n"
     .'           C => {d => 888, e => 999}, D => {}'."\n"
     ."          )\n",
      dvis('%toplex_h\n'); 
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
  s/\t/<tab>/msg;
  s/( +)$/"<space>" x length($1)/mseg;
  $_
}

sub get_closure(;$) {
 my ($clobber) = @_;

 my %closure_h = (%toplex_h);
 my @closure_a = (@toplex_a);
 my $closure_ar = \@closure_a;
 my $closure_hr = \%closure_h;
 if ($clobber) { # try to over-write deleted objects
   @closure_a = ("bogusa".."bogusz");
 }

 return sub {

  # Perl is inconsistent about whether an eval in package DB can see 
  # lexicals in enclosing scopes.  Sometimes it can, sometimes not.
  # However explicitly referencing those "global lexicals" in the closure
  # seems to make it work.
  my $forget_me_not = [ \$unicode_str, \$byte_str, \$toplex_hr, \$toplex_ar,
                        \%global_h, \@global_a, \$global_ar, \$global_hr, 
                      ];

  # Referencing these intermediate variables also prevents them from
  # being destroyed before this closure is executed:
  my $saverefs = [ \%closure_h, \@closure_a, \$closure_ar, \$closure_hr, ];
  

  my $zero = 0;
  my $one = 1;
  my $two = 2;
  my $EnvVarName = 'EnvVar';
  my $flex = 'Lexical in sub f';
  my $flex_ref = \$flex;
  my $ARGV_ref = \@ARGV;
  eval { die "FAKE DEATH\n" };  # set $@
  my %sublex_h = %toplex_h;
  my @sublex_a = @toplex_a;
  my $sublex_ar = \@sublex_a;
  my $sublex_hr = \%sublex_h;
  our %subglobal_h = %toplex_h;
  our @subglobal_a = @toplex_a;
  our $subglobal_ar = \@subglobal_a;
  our $subglobal_hr = \%subglobal_h;
  our %maskedglobal_h = %toplex_h;
  our @maskedglobal_a = @toplex_a;
  our $maskedglobal_ar = \@maskedglobal_a;
  our $maskedglobal_hr = \%maskedglobal_h;
  local %localized_h = %toplex_h;
  local @localized_a = @toplex_a;
  local $localized_ar = \@toplex_a;
  local $localized_hr = \%localized_h;

  for my $test (
    [ q(aaa\\\\bbb), q(aaa\bbb) ],
    [ q($unicode_str\n), qq(unicode_str=\" \\x{263a} \\x{263b} \\x{263c} \\x{263d} \\x{263e} \\x{263f} \\x{2640} \\x{2641} \\x{2642} \\x{2643} \\x{2644} \\x{2645} \\x{2646} \\x{2647} \\x{2648} \\x{2649} \\x{264a} \\x{264b} \\x{264c} \\x{264d} \\x{264e} \\x{264f} \\x{2650}\"\n) ],
    [ q($byte_str\n), qq(byte_str=\"\\n\\13\\f\\r\\16\\17\\20\\21\\22\\23\\24\\25\\26\\27\\30\\31\\32\\e\\34\\35\\36\"\n) ],
    [ q($flex\n), qq(flex=\"Lexical in sub f\"\n) ],
    [ q($$flex_ref\n), qq(\$\$flex_ref=\"Lexical in sub f\"\n) ],
    [ q($_ $ARG\n), qq(\$_=\"GroupA.GroupB\" ARG=\"GroupA.GroupB\"\n) ],
    [ q($a\n), qq(a=\"global-a\"\n) ],
    [ q($b\n), qq(b=\"global-b\"\n) ],
    [ q($1\n), qq(\$1=\"GroupA\"\n) ],
    [ q($2\n), qq(\$2=\"GroupB\"\n) ],
    [ q($3\n), qq(\$3=undef\n) ],
    [ q($&\n), qq(\$&=\"GroupA.GroupB\"\n) ],
    [ q(${^MATCH}\n), qq(\${^MATCH}=\"GroupA.GroupB\"\n) ],
    [ q($.\n), qq(\$.=1234\n) ],
    [ q($NR\n), qq(NR=1234\n) ],
    [ q($/\n), qq(\$/=\"\\n\"\n) ],
    [ q($\\\n), qq(\$\\=undef\n) ],
    [ q($"\n), qq(\$\"=\" \"\n) ],
    [ q($~\n), qq(\$~=\"STDOUT\"\n) ],
    [ q($^\n), qq(\$^=\"STDOUT_TOP\"\n) ],
    [ q($:\n), qq(\$:=\" \\n-\"\n) ],
    [ q($^L\n), qq(\$^L=\"\\f\"\n) ],
    [ q($?\n), qq(\$?=0\n) ],
    [ q($[\n), qq(\$[=0\n) ],
    [ q($$\n), qq(\$\$=$$\n) ],
    [ q($^N\n), qq(\$^N=\"GroupB\"\n) ],
    [ q($+\n), qq(\$+=\"GroupB\"\n) ],
    [ q(@+ $#+\n), qq(\@+=(13,6,13) \$#+=2\n) ],
    [ q(@- $#-\n), qq(\@-=(0,0,7) \$#-=2\n) ],
    [ q($;\n), qq(\$;=\"\\34\"\n) ],
    [ q(@ARGV\n), qq(\@ARGV=(\"fake\",\"argv\")\n) ],
    [ q($ENV{EnvVar}\n), qq(ENV{EnvVar}=\"Test EnvVar Value\"\n) ],
    [ q($ENV{$EnvVarName}\n), qq(ENV{\$EnvVarName}=\"Test EnvVar Value\"\n) ],
    [ q(@_\n), <<'EOF' ],  # N.B. Maxwidth was set to 72
@_=(42,
    [0,1,"C",
     {"" => "Emp", A => 111, "B B" => 222,
      C => {d => 888, e => 999}, D => {}
     },
     [],[0,1,2,3,4,5,6,7,8,9]
    ]
   )
EOF
    [ q($#_\n), qq(\$#_=1\n) ],
    [ q($@\n), qq(\$\@=\"FAKE DEATH\\n\"\n) ],
    map({
      my ($LQ,$RQ) = (/^(.*)(.)$/) or die "bug";
      map({ 
        my $name = $_;
        map({ 
          my ($dollar, $r) = @$_;
          my $dolname_scalar = ($dollar ? "\$$dollar" : "").$name;
          my $p = " " x length("?${dollar}${name}_?${r}");
          [ qq(%${dollar}${name}_h${r}\\n), <<EOF ],
\%${dollar}${name}_h${r}=("" => "Emp", A => 111, "B B" => 222,
${p}  C => {d => 888, e => 999}, D => {}
${p} )
EOF
          [ qq(\@${dollar}${name}_a${r}\\n), <<EOF ],
\@${dollar}${name}_a${r}=(0,1,"C",
${p}  {"" => "Emp", A => 111, "B B" => 222,
${p}   C => {d => 888, e => 999}, D => {}
${p}  },
${p}  [],[0,1,2,3,4,5,6,7,8,9]
${p} )
EOF
          [ qq(\$#${dollar}${name}_a${r}),    qq(\$#${dollar}${name}_a${r}=5)   ],
          [ qq(\$#${dollar}${name}_a${r}\\n), qq(\$#${dollar}${name}_a${r}=5\n) ],
          [ qq(\$${dollar}${name}_a${r}[3]{C}{e}\\n),
            qq(${dolname_scalar}_a${r}[3]{C}{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]{C}{e}\\n),
            qq(${dolname_scalar}_a${r}[3]{C}{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]->{A}\\n),
            qq(${dolname_scalar}_a${r}[3]->{A}=111\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]->{$LQ$RQ}\\n),
            qq(${dolname_scalar}_a${r}[3]->{$LQ$RQ}="Emp"\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]{C}->{e}\\n),
            qq(${dolname_scalar}_a${r}[3]{C}->{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]->{C}->{e}\\n),
            qq(${dolname_scalar}_a${r}[3]->{C}->{e}=999\n)
          ],
          [ qq(\@${dollar}${name}_a${r}[\$zero,\$one]\\n),
            qq(\@${dollar}${name}_a${r}[\$zero,\$one]=(0,1)\n)
          ],
          [ qq(\@${dollar}${name}_h${r}{${LQ}A${RQ},${LQ}B B${RQ}}\\n),
            qq(\@${dollar}${name}_h${r}{${LQ}A${RQ},${LQ}B B${RQ}}=(111,222)\n)
          ],
        } (['',''], ['$','r'])
        ), #map [$dollar,$r]
        map({ 
          my ($dollar, $r, $arrow) = @$_;
          my $dolname_scalar = ($dollar ? "\$$dollar" : "").$name;
          [ qq(\$${dollar}${name}_h${r}${arrow}{\$${name}_a[\$two]}{e}\\n),
            qq(${dolname_scalar}_h${r}${arrow}{\$${name}_a[\$two]}{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}${arrow}[3]{C}{e}\\n),
            qq(${dolname_scalar}_a${r}${arrow}[3]{C}{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}${arrow}[3]{C}->{e}\\n),
            qq(${dolname_scalar}_a${r}${arrow}[3]{C}->{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_h${r}${arrow}{A}\\n),
            qq(${dolname_scalar}_h${r}${arrow}{A}=111\n)
          ],
        } (['$','r',''], ['','r','->'])
        ), #map [$dollar,$r,$arrow]
        }
        qw(closure sublex toplex global subglobal maskedglobal localized
           A::B::C::ABC)
      ), #map $name
      } ('""', "''")
    ), #map ($LQ,$RQ)
  )
  {
    my ($dvis_input, $expected) = @$test;
    # warn "dvis_input=$dvis_input\n";
    
    { local $@;  # check for bad syntax first, to avoid uncontrolled die later
      # For some reason we can't catch exceptions from inside package DB.
      # undef is returned but $@ is not set
      my $ev = eval { "$dvis_input" };
      die "Bad test string:$dvis_input\nPerl can't interpolate it"
         .($@ ? ":\n  $@" : "\n")
        if $@ or ! defined $ev;
    }

    my $actual;
    { # Verify that special vars are preserved and don't affect Vis
      # (except if testing a punctuation var, then don't change it's value)

      my @fake = (do {$dvis_input =~ /\$(?: [\@!,\/\\] | \^[EW] )/x})
                    ? ($@, $!, $^E, $,, $/, $\, $^W)
                    : ('FakeAt', 111, 111, 'Fake,', 'Fake/', 'FakeBS\\', 333);
      my @nfake;
      { local ($@, $!, $^E, $,, $/, $\, $^W) = @fake;
        $actual = dvis $dvis_input; ### HERE IT IS ###
        @nfake = ($@, $!, $^E, $,, $/, $\, $^W);
      }
      for my $i (0..5) {
        no warnings;
        my $varname = (qw($@ $! $^E $, $/ $\ $^W))[$i];
        # was warn...
        die "ERROR: $varname was not preserved (expected $fake[$i], got $nfake[$i])\n"
          unless ($fake[$i] =~ /^\d/ ? ($fake[$i]==$nfake[$i]) : ($fake[$i] eq $nfake[$i]));
      }
    }
    unless ($expected eq $actual) {
      confess "\ndvis test failed: input «",show_white($dvis_input),"»\n"
           ."Expected:\n",show_white($expected),"«end»\n"
           ."Got:\n",show_white($actual),"«end»\n"
    }

    # Check Useqq 
    for my $useqq (0, 1) {
      my $input = $expected.$dvis_input.'qqq@_(\(\))){\{\}\""'."'"; # gnarly
      my $exp = doquoting($input, $useqq);
      my $act =  Vis->vnew($input)->Useqq($useqq)->Dump;
      die "\n\nUseqq ",u($useqq)," bug:\n"
         ."   Input   «${input}»\n"
         ."  Expected «${exp}»\n"
         ."       Got «${act}»\n"
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
