use strict; use warnings FATAL => 'all'; use 5.010;

# This file contains UTF-8 characters in debug-output strings (e.g. « and »).
# But to see them correctly on a non-Latin1 terminal (e.g. utf-8), your 
# program has to say 
#   use open ':std' => ':locale';
# or otherwise set the STDERR encoding to match what your terminal expects
# (Perl assumes Latin-1 by default).
use utf8;

$Vis::VERSION = sprintf "%d.%03d", q$Revision: 1.121 $ =~ /(\d+)/g;

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
  for ($Vis::DistToArgs = 2 ; ; $Vis::DistToArgs++) {
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
package DB;
our @CARP_NOT;

package Vis;

use Exporter;
use Carp;
use feature qw(switch state);
use POSIX qw(INT_MAX);
use Encode ();
use Scalar::Util qw(looks_like_number blessed reftype refaddr);
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
  my $s = Data::Dumper->new([shift])->Useqq(1)->Terse(1)->Indent(0)->Sortkeys(\&_sortkeys)->Dump;
  chomp $s;
  return $s;
}
sub debugavis(@) {  # for our internal debug messages
  local $/ = "\n";
  my $s = "(";
  foreach my $a (@_) {
    $s .= "," unless $s eq "(";
    $s .= debugvis($a);
  }
  return $s.")";
}

use Exporter 'import';
our @EXPORT    = qw(vis  avis  lvis  svis  dvis  hvis
                    visq avisq lvisq svisq dvisq hvisq
                    u qsh forceqsh qshpath);
                    #Dumper
our @EXPORT_OK = qw($Maxwidth $MaxStringwidth $Truncsuffix $Debug 
                    $Useqq $Quotekeys $Sortkeys 
                    $Terse $Indent $Sparseseen);

our @ISA       = ('Data::Dumper');

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
#
# N.B. u() used to take a list and return a list, but now only one scalar
#sub u(@) { map{defined ? $_ : "undef"} (@_==0 ? ($_) : @_) }
sub u(_) { $_[0] // "undef" }

sub vis(_)    { return __PACKAGE__->vnew(@_)->Dump1; }
sub visq(_)   { return __PACKAGE__->vnew(@_)->Useqq(0)->Dump1; }
sub avis(@)   { return __PACKAGE__->anew(@_)->Dump1; }
sub avisq(@)  { return __PACKAGE__->anew(@_)->Useqq(0)->Dump1; }
sub lvis(@)   { return __PACKAGE__->lnew(@_)->Dump1; }
sub lvisq(@)  { return __PACKAGE__->lnew(@_)->Useqq(0)->Dump1; }
sub hvis(@)   { return __PACKAGE__->hnew(@_)->Dump; }
sub hvisq(@)  { return __PACKAGE__->hnew(@_)->Useqq(0)->Dump1; }

# trampolines
#   The interpolation code for svis, etc. must live in package DB and
#   it's immediate caller must be the user's context.
sub svis(@)  { @_ = (Vis->snew(@_)); goto &DB::DB_Vis_Interpolate; }
sub dvis(@)  { @_ = (Vis->dnew(@_)); goto &DB::DB_Vis_Interpolate; }
sub svisq(@) { @_ = (Vis->snew(@_)->Useqq(0)); goto &DB::DB_Vis_Interpolate; }
sub dvisq(@) { @_ = (Vis->dnew(@_)->Useqq(0)); goto &DB::DB_Vis_Interpolate; }

sub Dump { 
  my $self = $_[0];
  if (! ref $self) { # ala Data::Dumper
    $self = $self->new(@_[1..$#_]);
  } else {
    die "extraneous args" if @_ != 1;
  }

  if (($self->{VisType}//"") =~ /^[sd]/) {
    #@_ = ($self);
    goto &DB::DB_Vis_Interpolate;
  }

  goto &DB::DB_Vis_Dump; # calls Dump1 from package DB
}

# Disabled -- causes "Dumper redefined" warnings if user also has Data::Dumper
# Provide Data::Dumper non-oo API
##sub Dumper { return __PACKAGE__->Dump1([@_]); }

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
    #if (-t STDERR) {
    {
      no warnings;
      ($r = qx'tput cols 2>/dev/null') # on Linux, seems to print 80 even if no tty
      ||
      (($r) = ((qx'stty -a 2>/dev/null'//"") =~ /.*; columns (\d+);/s))
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
sub lnew {
  my $class = shift;
  my $obj = (bless($class->SUPER::new([\@_]), $class))->_config_defaults();
  $obj->{VisType} = 'l';
  $obj;
}
sub hnew {
  my $class = shift;
  croak "hvis: Odd number of arguments (expecting hash keys and values)\n"
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
  return $obj;
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

# perl v5.24.1 gives "regular subexpression recursion limit (32766) exceeded"
# for very large quoted strings with many \escapes (e.g. binary blobs).
# This occurs even if (?> ) or the greedy *+ is used; I don't understand
# why any recursion is needed for (A|B)*+ because backtracking inside of
# the matched sequence can never occur.  But anyway perl has this limit.
# To work around, use multiple nested groupings:
#my $qstr_re = qr{ " (?: [^"\\]++ | (?:\\.)++ )*+ " | ' (?: [^'\\]++ | (?:\\.)++ )*+ ' }x;
my $qstr_re= qr{ " (?> (?> (?> [^"\\]++ | (?>\\.)* ){0,32760} ){0,32760} )* " 
                 |
                 ' (?> (?> (?> [^'\\]++ | (?>\\.)* ){0,32760} ){0,32760} )* ' 
               }x;

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

sub trunc_strings { # returns true if anything changed
  my $self = shift;
  my $ret = 0;
  # ** FIXME: Maybe should use Scalar::Util::reftype() here (and elsewhere!)
  # so as to recurse into object internals?
  my $kind = ref($_[0]);
  if (! defined $_[0]) {
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
  elsif ($kind eq "") { # a defined scalar
    my $truncsuf = $self->{Truncsuffix};
    my $tslen = length($truncsuf);
    my $maxlen = $self->{MaxStringwidth};
    if (length($_[0]) > ($maxlen+$tslen)) {
      # FIXME
      ###die "BUG HERE: Must clone before modifying user data!";
      $_[0] = substr($_[0],0,$maxlen).$truncsuf;
      $ret = 1;
    }
  }
  else {
    # Something else, REGEXP, CODE, etc. or an object ref
  }
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

sub _try_stringify($$$);
sub _try_stringify($$$) {
  my ($item, $stringify, $seen) = @_;
  if (my $class = blessed($item)) {
    foreach my $mod (@$stringify) {
      next if $mod eq "";
      if (ref($mod) eq "Regexp" ? $class =~ /$mod/ : $class eq $mod) {
        # Allow this object to stringify itself.  The result is an
        # unfortunately-always-quoted string
        $_[0] = "($class)".$_[0];
        return 1;
      }
    }
  }
  my $reftype = reftype($item);
  return 0 if ! defined $reftype;
  my $refaddr = refaddr($item);
  return 0 if $seen->{$refaddr}++;
  my $changed = 0;
  if ($reftype eq 'ARRAY') {
    foreach (@$item) {
      $changed=1 if _try_stringify($_, $stringify, $seen);
    }
  }
  elsif ($reftype eq 'HASH') {
    foreach (values %$item) {
      $changed=1 if _try_stringify($_, $stringify, $seen);
    }
    #while (my($k,$v) = each %$item) {
    #  $changed=1,$item->{$k}=$v if _try_stringify($v, $stringify, $seen);
    #}
  }
  $changed;
}

# Reformat Data::Dumper::Dump output in $_
sub _reformat_dumper_output {
  my ($self, $maxwidth, $debug, $useqq) = @_;
  print "===== RAW =====\n${_}---------------\n" if $debug;
  
  #return $_ if $maxwidth == 0; # no condensation
  
  ##return $_ if /sub\s*\{/s && $self->Deparse; # we can't handle this

  # Split into logical lines, being careful to preserve newlines in strings.
  # The "delimiter" is the whole (logical) line, including final newline,
  # which is returned because it is in a (capture group).
  my @lines;
  my $limit = length($_);
  my $startpos = 0;
  while ($startpos != $limit) {
    if ($useqq) {
      ### FIXME: TODO: BUG HERE:  
      #while (/\G"(?:[^"\\]++|\\.)*+"/gsc || /\G[^"\n]+/gsc) {}
      # Ack!  Perl implements (...)* using recursion even when partial
      # backtracking is not possible, and there is a 64K recursion limit.
      # Benchmarking shows that ~200 is the sweet spot.
      while (
        (/\G"/cgs && do{ while(/\G(?:[^"\\]++|\\.){0,200}+/cgs){} /\G"/cgs })
        || 
        /\G\s*sub\s*(?=\{)/cgs && do{ require Text::Balanced;
                                      () = Text::Balanced::extract_bracketed(
                                                                      $_, '{[(\'"');
                                    }
        || 
        /\G[^s"\n]+/gsc  # eat until just before 's', then loop to catch 'sub' above
        || 
        /\Gs/gsc         # eat an 's' which was not "sub..."
      ) {}
    } else {
      #while (/\G'(?:[^'\\]++|\\['\\])*+'/gsc || /\G[^'\n]+/gsc) {}
      while (
        (/\G'/cgs 
         && do{ while(/\G(?:[^'\\]++|\\['\\]){0,200}+/cgs){} /\G'/cgs })
        || /\G[^'\n]+/gsc
      ) {}
    }
    /\G\n/gsc or die "oops:".debugvis(substr($_,pos));
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
  
        # The wierd (1||/^[\{]/) is so vim will see a matching bracket
        my $J_is_closing = (1||/^[\{]/) && ($Jcode =~ /[\]\}]$/);
  
        print "##Icode=",debugvis($Icode)," Jcode=",debugvis($Jcode)," maxwidth=$maxwidth\n" if $debug;
        _debug_show(\@lines, $I, $Iindent, $J, $Jindent, $restart) if $debug;
  
        if (($Iindent <= $Jindent)
            # I is not closing an aggregate
            && (1||/^[\{]/) && $Icode !~ /^[\]\}]/ 
  
            # J does not end with an un-closed block (we only join atoms or
            # [whole blocks]).  This prevents hard-to-see keys such as KEY2:
            #    { "KEY" => [ 1, 2, 3, 4,
            #      "five", "six", 7 ], "KEY2" => ... }
            && $Jcode !~ /^ (?> ${balanced_or_safe_re}) [\{\[] /x
            && (1||/^[\}]/)
           )
         { 
          # The lines are elegible to be joined, if there is enough space
          # (or in any case if the joining would not increase line length)
          my $Ilen = $Iindent + length($Icode);
          my $Jlen = length($Jcode);
          my $sep  = $Icode =~ /,$/ && $Jcode =~ / => ${balanced_or_safe_re},?$/x
                      ? " " : "";
          my $adj = $Ilen + length($sep) - $Jindent; # posn change, + or -
          print "[Iind=$Iindent Ilen=$Ilen Jind=$Jindent Jlen=$Jlen sep='${sep}' adj=$adj mw=$maxwidth]\n" if $debug;
  
          if ($Ilen + $Jlen + length($sep) <= $maxwidth || $adj <= 0) {
            substr($lines[$I],$Ilen) = $sep.substr($lines[$J], $Jindent); 
            $lines[$J] = "";
            print "## joined:",debugvis($lines[$I])," (len=",length($lines[$I]),")\n" if $debug;
            if ($J < $#lines && $adj < 0) {
              # Adjust the indentation of remaining items in the same block
              # to line up with the item just joined, the indent decreased.
              # This occurs when joining members onto an opening bracket.
              #my $extra_prefix = " " x $adj;
              for (my $K=$J+1; $K <= $#lines; $K++) {
                die "bug J=$J K=$K\n   lines=(\n   ",join("\n   ",map{debugvis($_)} @lines),"\n   )" if $K > $#lines;
                $lines[$K] =~ /^( *)/;
                last if length($1) <= $Iindent; # end of nested block
                die "BUG: WOUld remove non-indent chars" if length($1) < (-$adj);
                #if ($adj > 0) {
                #  $lines[$K] = $extra_prefix . $lines[$K];
                #} else { 
                  substr($lines[$K],0,-$adj) = "";  # N.B. -$adj is positive
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
}

# Dump1 is like Dump except:
#   1. The user's frame is up one level.
#   2. The immediate caller is in package DB
# For dvis etc., this is called from the interpolation code in package DB
# For user-called Dump(), this is called via a thunk in package DB
sub Dump1 {
  local $_;
  _first_time_init() unless defined $Vis::original_qquote;

  my $self = $_[0];

  #print "##Dump1 caller(0)=",Vis::debugavis((caller(0))[0,1,2])," VT=",Vis::debugvis($self->{VisType}),"\n";
  #print "##Dump1 caller(1)=",Vis::debugavis((caller(1))[0,1,2])," VT=",Vis::debugvis($self->{VisType}),"\n";

  if (($self->{VisType}//"") =~ /^[sd]/) {
    @_ = ($_[0]); 
    goto &DB::DB_Vis_Interpolate;
  }

  my ($debug, $maxwidth, $maxstringwidth) 
    = @$self{qw/VisDebug Maxwidth MaxStringwidth/};

  my $cloned;

  my $useqq = $self->Useqq();
  $self->Useperl(1) if $useqq eq 'utf8' && $Vis::Useperl_for_utf8;

  my $stringify = $self->Stringify();
  if (! defined $stringify) {
    $stringify = [];
    # N.B. Trampolines arrange to interpose exactly one level, so 
    # caller(1) is always the user's context
    if (my $hinthash = (caller(1))[10]) {
      if (any {/^big/} keys %$hinthash) {
         push @$stringify, qr/^Math::Big/;
      }
    }
  }
  $stringify = [$stringify] unless ref($stringify) eq 'ARRAY';
  if (@$stringify > 0 && $stringify->[0] ne "") {
    require Clone;
    my @values = map{ Clone::clone($_) } $self->Values;
    $cloned = 1;
    my %seen;
    my $changed = 0;
    foreach (@values) {
      $changed=1 if _try_stringify($_, $stringify, \%seen);
    }
    $self->Values(\@values) if $changed;
  }

  if (($maxstringwidth//0) > 0) {
    require Clone;
    my @todump = $cloned ? ($self->Values) : (map{ Clone::clone($_) } $self->Values);
    if ($self->trunc_strings(\@todump)) {
      $self->Values( \@todump );
    }
  }

  my $pad = $self->Pad();

  # Maxwidth==0 means do not fold at all, so we use Indent(0) when
  # calling Data::Dumper.  However in that case Data::Dumper inserts Pad()
  # between every token; so we have to force Pad() to "" and prepend 
  # the user's Pad() string ourself to the overall result.
  my $user_Indent;
  if ($maxwidth == 0) {
    $user_Indent = $self->Indent(0);
    #$self->Pad("");
    $self->Pad(" ");
  }

  # Hack -- save/restore punctuation variables corrupted by Data::Dumper
  my ($sAt, $sQ) = ($@, $?);

  $_ //= $self->SUPER::Dump;

  ($@, $?) = ($sAt, $sQ);

  s/^ *// if $maxwidth==0;  # remove initial forced pad from Indent(0)

  if ($pad ne ""){
    # Remove initial pad; will be put back later
    s/^\Q${pad}\E//mg || $maxwidth==0 || die "bug($_)";
    if ($maxwidth > 0) {
      $maxwidth -= length($pad);
      if ($maxwidth < 20) {
        #state $warned;
        #$warned=1, warn "Warning: Pad is too wide, Vis may exceed Maxwidth\n"
        #  if (! $warned);
        $maxwidth = $self->{Maxwidth};
      }
    }
  }

  if ($maxwidth > 0) {
    $self->_reformat_dumper_output($maxwidth, $debug, $useqq);
  }
  # else: one long line, produced using Indent(0) above;

  if ($self->{VisType}) {
    s/\s+\z//s;  # omit final newline except when emulating Data::Dumper
    if ($self->{VisType} eq 'a') {
      s/^( *)[\[{]/$1\(/ or confess "bug1($_)"; # convert to "(list,of,args)"
      s/[\]}]$/\)/ or confess "bug2($_)";
    }
    elsif ($self->{VisType} eq 'l') {
      s/^( *)[\[{]/$1/ or confess "bug3($_)"; # convert to "bare,list,of,items"
      s/[\]}]$// or confess "bug4($_)";
    }
  }

  s/^/$pad/meg if $pad ne "";

  if ($maxwidth == 0) {
    $self->Pad($pad);
    $self->Indent($user_Indent);
  }

  #print "### Dump1 pad=",Vis::debugvis($pad)," returning ",Vis::debugvis($_),"\n";

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
                    ? (/[^-=\w_\/:\.,]/ || $_ eq "" || ref) ? forceqsh($_) : $_
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

sub DB_Vis_Dump     { return Vis::Dump1(@_) }

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

  #print "##DB_Vis_Interpolate caller=",Vis::debugavis(caller)," VT=",Vis::debugvis($self->{VisType}),"\n";

  # NOTE: These REs may or may not use capture groups internally, so capture
  # groups must NOT be opened after the first point of use in a regexp!
  # TODO: Rewrite using named capture groups
  
  state $interior_re = qr/${balanced_or_safe_re}/;

  state $variable_re = qr/   # sigl could be $ or @
      \#?\w+(?:::\w+)*   # @name $pkg::name $#name $1
    | \#?\$\w+(?:::\w+)* # $$ref $#$ref
    | \^\w               # $^N   (control-character 'punctuation' variable)
    | \#?[^\w\s\{\$]     # $#- $^ $? (regular 'punctuation' variable)
    | (?<=\$)\$          # $$
    | \{ $interior_re \} # ${ref expression} or ${^SPECIALNAME}
  /x;
  state $dummy_for_vim = qr/\}/; # (provide matching '}')

  state $scalar_index_re = qr{
    (?: (?:->)? (?: \{ $interior_re \} | \[ $interior_re \] ) )+
  }x;
  state $slice_re = qr{
    (?: \{ $interior_re \} | \[ $interior_re \] )
  }x;

  state $method_call_re = qr{
    (?: -> (?: \w+ | $variable_re ) (?: \( $interior_re \) | (?=[\s\\=,;]) | \z ) )
  }x;

  my @actions;
  {
    # N.B. The evals are executed below after $_ is restored.
    local $_ = join "", map {defined($_) ? $_ : '<undef arg>'} $self->Values();
    while (1) {
      #print "### pos()=",Vis::u(pos()),"\n" if $debug;
      if (
        # \G does not work with (?|...) in Perl 5.12.4
        # https://rt.perl.org/rt3//Public/Bug/Display.html?id=112894

        # $name->method
        # $name->method(...)
        /\G (?!\\)(\$)( $variable_re ${method_call_re} )/xsgc
        ||
        # $name $name[expr] $name->{expr} ${refexpr}->[expr] etc.
        /\G (?!\\)(\$)( $variable_re ${scalar_index_re}? )/xsgc
        ||
        # ${name} ${name->[expr]} etc. (loosing the curlies)
        /\G (?!\\)(\$) \{ ( $variable_re ${scalar_index_re}? ) \}/xsgc
        ||
        # @name @name[slice] @name{slice} @{refexpr}[slice1][slice2] etc.
        /\G (?!\\)(\@)( $variable_re ${slice_re}* )/xsgc
        ||
        # @{name} @{name[slice]} etc. (loosing the curlies)
        /\G (?!\\)(\@) \{ ( $variable_re ${slice_re}* ) \}/xsgc
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
          die "Vis bug: next:",substr($tmp,pos//0,4),
              "... pos=",Vis::u(pos)," in:\n$tmp\n".(" "x (pos//0))."^\n "
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
        print "### extra_padlen=$extra_padlen\n" if $debug;
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

      my $s = $self->Dump1;  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<

      print "### Vis::Indent=$Vis::Indent Indent()=", $self->Indent(), 
            " Dump1 result=", Vis::debugvis($s),"\n" if $debug;
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

  # Erase package variables which might contain references to user objects
  # which would otherwise not be destroyed when expected
  #@Vis::saved = (); @Vis::result = (); $Vis::evalarg = undef;
  undef @Vis::saved; undef @Vis::result; undef $Vis::evalarg;

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

  # Interpolate strings, stingifying aggregates with auto-indenting.
  print svis 'FYI $ref\n Info: %hash\n args are @ARGV\n'; # SINGLE quoted!

  # dvis (d for debug) is like svis but auto-prefixes values with "varname="
  print dvis 'FYI $ref %hash @ARGV\n';

  # Format one item at a time (sans final newline)
  print "ref=", vis($ref), "\n";
  print "ARGV=", avis(@ARGV), "\n";  # (array,values,in,parens)

  dvisq(), svisq(), visq(), and avisq()  show strings in 'single quoted' style

  { use bigint; # Show large numbers as strings
    my $big = 1_000_000_000 * 1_234_567_890;
    print vis($big), "\n";  # "1234567890000000000"
    # disable stringifying "big" numbers
    $Vis::Stringify = ""; 
    print vis($big), "\n"; # bless({...}, 'Math::BigInt')
  }

  # Avoid unnecessary hex escaping in Unicode strings
  $Vis::Useqq = 'utf8';
  binmode(STDOUT, ":utf8");
  my $var = "Just let me read my Unicode \N{U+263A} ";
  print svis('var=$var\n');

  # Equivalent OO APIs allow using configuration methods
  print Vis->snew('FYI $ref\n Info: %hash\n args are @ARGV\n')
             ->Maxwidth(120)
             ->Maxdepth($levels)
             ->Pad("| ")
             ->Dump;                                  # like svis()
  print Vis->dnew('Howdy! $var @ary %hash\n')->Dump;  # like dvis()
  print Vis->vnew($scalar)->Dump, "\n";               # like vis()
  print Vis->anew(@array)->Dump, "\n";                # like avis()

  print Dumper($ref);                         # Data::Dumper
  print Vis->new([$ref],['$ref'])->Dump;      #  compatible APIs

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
as in in Perl double-quotish strings except that interpolated variables
are formatted using C<vis()> or C<avis()> for $ or @ expressions, respectively.  
In addition, C<%name> is interpolated as S<<< C<< (key => value ...) >> >>>, and
C<< $name->method(...) >> is interpolated as a method call (if args are given, 
no space is allowed before the open parenthesis).

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

These alternatives use 'single quotes' when formatting strings, with no \=escapes 
except for \\.

=head2 OO interfaces

OO interfaces allow setting Configuration options on a case-by-case basis.

B<< Vis->snew >>, B<< Vis->dnew >>, B<< Vis->vnew >> and B<< Vis->anew >>
are constructors corresponding to the functions
B<svis>, B<dvis>, B<vis> and B<avis>, respectively.  See SYNOPSIS above.

Additionally, B<< Vis->new >> provides the same API as Data::Dumper->new.

=head2 Configuration Variables or Methods

=over 4

=item $Vis::Maxwidth   or  $OBJ->Maxwidth(B<[NEWVAL]>)

Sets or gets the maximum number of characters for formatted lines,
including any prefix set via C<Pad()>.
Default is the your terminal width or 80 if not be detected.
If Maxwidth=0 output is not folded at all, appearing similar to Data::Dumper
with Indent(0) but without a final newline.

=item $Vis::MaxStringwidth   or   $OBJ->MaxStringwidth(B<NEWVAL>)

Sets or gets the maximum number of characters to be displayed
for an individual string.  Longer strings are truncated
and an ellipsis (...) appended.  Zero or undef for no limit.

=item $Vis::Stringify   or   $OBJ->Stringify(B<[...]>)

Objects of the specified class(es) are allowed to convert themselves to 
strings before being dumped.  The Stringify property value may be:

1) undef (the default): If the caller has 'use bigint' or 'use bignum' 
or similar in effect, then appropriate class names are inferred,
(e.g. Math::BigInt) so that those objects appear as strings.

2) A string giving the name of a class, a qr/regex/ matching class names,
or a ref to an array of those things; objects blessed into 
any matching class will be self-stringified by concatenating "".

3) "" or [] : The feature is completely disabled

When self-stringification is performed, a _deep copy_ is made of the 
entire data, which will impact performance for large structures.

Stringified values will include a (classname) prefix.

Note: 'Stringify' is conceptually related to the 'Freezer' property
provided by Data::Dumper.  However 'Freezer' can only be used to modify
existing objects in-place, not entirely replace them with something else
such as a string.  Also, 'Stringify' is irreversible; there is no provision 
for reconstituting the original objects if the output is eval'd.
  
=back

The following Methods have the same meaning as in Data::Dumper except that
default values come from global variables in package B<Vis> :

=over 4

=item $Vis::Quotekeys            or   $OBJ->Quotekeys(B<NEWVAL>)

=item $Vis::Sortkeys             or   $OBJ->Sortkeys(B<NEWVAL>)

=item $Vis::Terse                or   $OBJ->Terse(B<NEWVAL>)

=item $Vis::Indent               or   $OBJ->Indent(B<NEWVAL>)

=item $Vis::Sparseseen           or   $OBJ->Sparseseen(B<NEWVAL>)

=item $Vis::Useqq                or   $OBJ->Useqq(B<NEWVAL>)

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

=item $Data::Dumper::Deepcopy    or   $OBJ->Deepcopy(B<NEWVAL>)

=item $Data::Dumper::Freezer     or   $OBJ->Freezer(B<NEWVAL>)

=item $Data::Dumper::Maxdepth    or   $OBJ->Maxdepth(B<NEWVAL>)

=item $Data::Dumper::Pad         or   $OBJ->Pad(B<NEWVAL>)

=item $Data::Dumper::Purity      or   $OBJ->Purity(B<NEWVAL>)

=item $Data::Dumper::Toaster     or   $OBJ->Toaster(B<NEWVAL>)

=item $Data::Dumper::Varname     or   $OBJ->Varname(B<NEWVAL>)

=item $Data::Dumper::Deparse     or   $OBJ->Deparse(B<NEWVAL>)

=back

This inherited method should not be used with Vis :

=over 4

=item $Data::Dumper::Pair        or   $OBJ->Pair(B<NEWVAL>)

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
# Tester
use strict; use warnings; use 5.010;
our @ISA;

use Test::More;
my $debug = @ARGV && $ARGV[0] =~ /^(-d|--debug)/;

plan tests => ($debug ? 1 : 2);

# Run this script with --debug to see debug output!
#
sub dprint(@)   { print(@_)                if $debug };
sub dprintf($@) { printf($_[0],@_[1..$#_]) if $debug };

my ($infile, $testdata);
my ($infile2, $testdata2);
BEGIN {
  select STDERR; $|=1; select STDOUT; $|=1;
  # N.B. &title_info encodes knowledge about this data!
  $infile = "/tmp/input.csv";
  $testdata = <<'EOF' ;
Pre-title-row stuff (this is rowx 0)
Atitle,Btitle,Multi Word Title C,,H,F,Gtitle,Z
A2,B2,C2,D2,E2,F2,G2,H2
A3,B3,C3,D3,E3,F3,G3,H3
A4,B4,C4,D4,E4,F4,G4,H4
A5,B5,C5,D5,E5,F5,G5,H5
A6,B6,C6,D6,E6,F6,G6,H6
EOF
  dprint "> Creating $infile\n";
  open(F,">$infile") || die $!; print F $testdata; close F || die "Error writing $infile :$!";

  $infile2 = "/tmp/input2.csv";
  $testdata2 = <<'EOF' ;
TitleP,TitleQ,,TitleS,TitleT
P1,Q1,R1,S1,T1,U1
P2,Q2,R2,S2,T2,U2
P3,Q3,R3,S3,T3,U3,V3
P4,Q4,R4,S4,T4,U4
P5,Q5,R5,S5,T5,U5
EOF
  dprint "> Creating $infile2\n";
  open(F,">$infile2") || die $!; print F $testdata2; close F || die "Error writing $infile2 :$!";
}

use Carp;
use File::Temp qw/ tempfile /;
use open ':std' => ':locale';  # Make STDOUT/ERR match terminal
$SIG{__WARN__} = sub {
  die "bug:$_[0]" if $_[0] =~ "uninitialized value";
  print STDERR $_[0];
};
#$SIG{_1G_DIE__} = sub{ return unless defined($^S) && $^S==0; confess @_ };
use Vis;
use Text::CSV::Spreadsheet qw(let2cx cx2let);
use Guard qw(scope_guard);
sub bug(@) { @_=("BUG ",@_); goto &Carp::confess }
sub outercroak (@) { # shows only outer-scope call
  for my $c (0..5) {
    warn "caller($c)=",avis(caller($c)),"\n";
  }
  my $c=0; while ((caller($c+1))[3] =~ /main::[a-z]/a) { $c++ }
  $Carp::CarpLevel += $c;
  goto &Carp::croak;
}

sub arrays_eq($$) {
  my ($a,$b) = @_;
  return 0 unless @$a == @$b;
  for(my $i=0; $i <= $#$a; $i++) {
    return 0 unless $a->[$i] eq $b->[$i];
  }
  return 1;
}

require Exporter;
BEGIN { $Exporter::Debug = $debug }
use Text::CSV::Edit qw(:DEFAULT :STDVARS);
BEGIN { $Exporter::Debug = 0 }

BEGIN {
  #$Carp::Verbose = 1;
  $Carp::MaxArgLen = 0;
  $Carp::MaxArgNums = 25;

  if ($debug) {
    $Text::CSV::Edit::verbose = 1; # turn on for debugging implied new calls
    $Text::CSV::Edit::debug   = 1; # turn on for debugging implied new calls
  }
}

##########################################################################
package Other;
BEGIN {*dprint = *main::dprint; *dprintf = *main::dprintf;}
sub bug(@) { @_=("BUG ",@_); goto &Carp::confess }
use Text::CSV::Edit qw(:DEFAULT :STDVARS);
use vars '$Gtitle';
$Gtitle = "non-tied-Gtitle-in-Other";
new_sheet
          data_source => "Othersheet",
          rows => [ [qw(OtitleA OtitleB OtitleC)],
                    [   999,    000,    26      ],
                    [   314,    159,    26      ],
                    [   777,    888,    999     ],
                  ],
          linenums => [ 1..4 ],
          silent => (! $debug),
          ;
our $Othersheet = sheet();
dprint "Othersheet = $Othersheet\n";
title_rx 0;
tie_column_vars qw(OtitleA OtitleB);
our ($OtitleA, $OtitleB);

our ($OtitleC);

##########################################################################
package main;

sub check_other_package() {
  bug unless $Other::Gtitle eq "non-tied-Gtitle-in-Other";
  package Other;
  bug unless $Gtitle eq "non-tied-Gtitle-in-Other";
  apply_torx { bug unless $Other::OtitleA == 314 } [2];
  our $OtitleA;
  apply_torx { bug unless $OtitleA == 314 } [2];
  bug unless $Other::Gtitle eq "non-tied-Gtitle-in-Other";
  bug unless $num_cols == 3;
  bug unless @rows==4 && $rows[2]->[0]==314;
  bug unless @Other::rows==4 && $Other::rows[2]->[1]==159;
}
check_other_package;

tie_column_vars {package=>'Other'}, qw(OtitleA OtitleB); # redundant call is ok
check_other_package;

sub hash_subset($@) {
  my ($hash, @keys) = @_;
  return undef if ! defined $hash;
  return { map { exists($hash->{$_}) ? ($_ => $hash->{$_}) : () } @keys }
}
sub fmtsheet() {
  my $s = sheet();
  return "sheet=undef" if ! defined $s;
  "sheet->".vis(hash_subset($$s, qw(colx rows linenums num_cols current_rx title_rx)))
  #"\nsheet=".Vis->vnew($s)->Maxdepth(2)->Dump
}

sub expect1($$) {
  my ($actual, $expected) = @_;
  if (! defined $expected) {
    return if ! defined $actual;
  } else {
    return if defined($actual) && $actual eq $expected;
  }
  die "Expected ",vis($expected), " but got ", vis($actual),
      " at line ", (caller(0))[2], "\n";
}

# Return value of a cell in the 'current' row, verifying that various access
# methods return the same result (could be undef if the item is not defined).
# RETURNS ($value, undef or "bug description");

sub getcell_bykey($;$) { # title, ABC name, alias, etc.
  my ($key, $err) = @_;
  bug unless defined $rx;  # not in apply?

  # Access using the global "current row hash" feature
  my $v = $row{$key};
  my $vstr = vis( $v );

  # Access using the overloaded hash-deref operator of the sheet object
  # (accesses the 'current' row)
  my $hashov_vstr = vis( sheet()->{$key} );
  $err //= "\$row{$key} returned $vstr but sheet()->{$key} returned $hashov_vstr"
    if $vstr ne $hashov_vstr;

  # Access using the overloaded array-deref operator of the sheet object,
  # indexing the resulting Magicrow with the Title key.
  my $magicrow = sheet()->[$rx];
  my $aov_key_vstr = vis( $magicrow->{$key} );
  $err //= "\$row{$key} returned $vstr but sheet()->[$rx]->{$key} returned $aov_key_vstr"
    if $vstr ne $aov_key_vstr;

  # Index the Magicrow as an array indexed by cx
  my $cx = $colx{$key};
  $err //= "%colx does not match sheet()->colx for key $key"
    if u($cx) ne u(sheet()->colx->{$key});
  my $aov_cx_vstr = vis( defined($cx) ? $magicrow->[$cx] : undef );
  $err //= "\$row{$key} returned $vstr but sheet()->[$rx]->[$cx] returned $aov_cx_vstr"
    if $vstr ne $aov_cx_vstr;

  return ($v, $err);
}
sub getcell_byident($;$) { # access by imported $identifier, and all other ways
  my ($ident, $inerr) = @_;
  my ($v, $err) = getcell_bykey($ident, $inerr);
  my $vstr = vis($v);

  my $id_v = eval "\$$ident";   # undef if not defined, not in apply(), etc.
  my $id_vstr = vis($id_v);
  $err //= "\$$ident returned $id_vstr but other access methods returned $vstr"
    if $vstr ne $id_vstr;

  return ($v, $err);
}

# Given a column data indicator L and current cx, return
# indicators of what titles should be usable.
sub title_info($$) {
  my ($L,$cx) = @_;

  # infile:
  #   Original col D has an empty title
  #   Original col E has title 'H' which looks like ABC code
  #   Original col F has title 'F' which looks like ABC code
  #   Original col H has title 'Z' which looks like ABC code
  # infile2:
  #   Original col R has an empty title
  #   Original col U has a missing title
  #   Original col V has a missing title and irregular data
  #
  # Other columns have titles which do not look like ABC codes

  my ($title_L, $ABC_usable, $QT_usable) = ($L,1,1);

  my $ABC = cx2let($cx);
  if ($ABC eq "H") {
    $ABC_usable = ($L eq "E");
  }
  elsif ($ABC eq "F") {
    $ABC_usable = ($L eq "F");
  }
  elsif ($ABC eq "Z") {
    $ABC_usable = ($L eq "H");
  }

  if ($L =~ /^[RDUV]$/) {
    $title_L = "";
  }

  if ($L eq 'E') {
    $title_L = "H";
  }
  elsif($L eq 'F') {
    # $title_L = "F";
  }
  elsif($L eq 'H') {
    $title_L = "Z";
    $ABC_usable = ($cx == let2cx("E"));
  }
  elsif($L eq 'Z') { # in case many columns are added...
    $ABC_usable = ($cx == let2cx("H"));
  }

  #TODO test dup titles; when we do, set QT_usable = 0

  if ($title_L eq "") {
    $QT_usable = undef;
  }

  return ($title_L, $ABC_usable, $QT_usable);
}

sub check_currow_data($) {
  my $letters = shift;  # specifies order of columns. "*" means don't check
  confess dvis 'WRONG #COLUMNS @letters @$row $rx'
    if length($letters) != @$row;
  die "\$rx not right" unless ${ sheet() }->{current_rx} == $rx;

  for (my $cx=0, my $ABC="A"; $cx < length($letters); $cx++, $ABC++) {
    my $L = substr($letters,$cx,1);

    my ($ABC_v, $err) = getcell_byident($ABC);
    if ($@) { $err //= "ABC $ABC aborts ($@)" }
    elsif (! defined $ABC_v) { $err //= "ABC $ABC is undef" }

    # Manually locate the cell
    my $man_v = $row->[$cx];

    # The Titles    H, F, and Z mask the same-named ABC codes, and refer to
    # orig. columns E, F, and H .
    if ($L ne "*") { # data not irregular
      my $exp_v = "$L$rx"; # expected data value

      if ($man_v ne $exp_v) {
        $err //= svis 'WRONG DATA accessed by cx: Expecting $exp_v, got $man_v';
      }

      if (defined $title_row) { # Access the cell by title
        # Titles are always valid [with new implementation...]
        my $title = $title_row->[$cx];
        my ($title_L, $ABC_usable, $QT_usable) = title_info($L, $cx);
        if ($title_L ne "") {
          $err //= dvis('$title_L is TRUE but $title is EMPTY')
            if $title eq "";
          my $vt; ($vt, $err) = getcell_bykey($title, $err);
          if (u($vt) ne $exp_v) {
            $err //= svis('row{Title=$title} yields $vt but expecting $exp_v')
          }
        }
        if ($QT_usable) {
          die "bug" if $title_L eq "";
          my $qtitle = "'$title'";
          my $vqt; ($vqt, $err) = getcell_bykey($qtitle, $err);
          if (u($vqt) ne $exp_v) {
            $err //= svis('row{QT=$qtitle} yields $vt expecting $exp_v')
          }
        }
      }
    }

    if (defined $err) {
      confess "BUG DETECTED...\n", fmtsheet(), "\n",
              dvis('$rx $letters $cx $ABC $L $man_v $row->[$cx]\n$row\n'),
              $err;
    }
  }
  check_other_package();
}
sub check_titles($) {
  my $letters = shift;  # specifies current order of columns
  confess dvis('title_row is UNDEF\n').fmtsheet()
    unless defined $title_row;
  for (my $cx=0; $cx < length($letters); $cx++) {
    my $L = substr($letters, $cx, 1);
    my ($title_L, $ABC_usable, $QT_usable) = title_info($L, $cx);
    # $title_L is a letter which must appear in the title
    #   or "" if the title should be empty
    # $ABC_usable means the column can be accessed via its ABC letter code.
    # $QT_usable means the column can be accessed via its 'single-quoted Title'
    # FIXME: Shouldn't QT_usable _always_ be true?!?
    die "bug" unless $rows[$title_rx]->[$cx] eq $title_row->[$cx];
    my $title = $title_row->[$cx];
    my $qtitle = "'${title}'";
    my $err;
    if ($title_L eq "") {
      $err //= svis 'SHOULD HAVE EMPTY TITLE, not $title'
        unless $title eq "";
    }
    elsif ($title !~ /\Q$title_L\E/) {
      $err //= svis 'WRONG TITLE $title (expecting $title_L)'
    }
    apply_torx {
      if ($row->[$cx] ne $title) {
        $err //= svis 'apply_torx title_rx : row->[$cx] is WRONG';
      }
    } $title_rx;
    apply_torx {
      if ($row->[$cx] ne $title) {
        $err //= svis 'apply_torx [title_rx] : row->[$cx] is WRONG';
      }
    } [$title_rx];
    if ($ABC_usable) {
      my $ABC = cx2let($cx);
      my $v = $colx{$ABC};
      $err //= svis('WRONG colx{ABC=$ABC} : Got $v, expecting $cx')
        unless u($v) eq $cx;
    }
    if ($QT_usable) {
      my $v = $colx{$qtitle};
      $err //= svis('WRONG colx{QT=$qtitle} : Got $v, expecting $cx')
        unless u($v) eq $cx;
    }
    if (defined $err) {
      confess fmtsheet(), "\n", dvis('$L $cx $title_L \n   '), $err;
    }
  }
  check_other_package();
}

sub check_both($) {
  my $letters = shift;  # indicates current column ordering

  my $prev_verbose = options(verbose => 0);
  scope_guard { options(verbose => $prev_verbose) };

  croak "Expected $num_cols columns" unless length($letters) == $num_cols;

  check_titles $letters;

  apply {
    return if $rx < $title_rx;  # omit header rows
    check_currow_data($letters)
  };
}

sub verif_eval_err($) {
  my ($ln) = @_;
  my $fn = __FILE__;
  croak "expected error did not occur at line $ln\n" unless $@;

  my $errmsg = "";
  if ($@ !~ / at $fn line $ln\.?(?:$|\n)/s) {
    $errmsg .= "Got UN-expected err (did not point to file $fn line $ln)\n";
  }
  if ($@ =~ /Text::CSV/ && !$Carp::Verbose) {
    $errmsg .= "Error traceback mentions internal package:\n"
  }
  if ($errmsg) {
    croak $errmsg, $@;
  } else {
    dprint "Got expected err ",vis($@),"\n";
  }
}

# Verify that a column title, alias, etc. is NOT defined
sub check_colspec_is_undef(@) {
  foreach(@_) {
    bug "Colspec ".vis($_)." is unexpectedly defined" 
      if defined $colx{$_};
  }
}

# Verify %colx entries, e.g. aliases.  Arguments are any mixture of
# [ $ident, $CxorABC] or "Ident_CxorABC".
sub check_colx(@) {
  my $colx = sheet()->colx;
  foreach (@_) {
    my ($ident, $cx_or_abc);
    if (ref) {
      ($ident, $cx_or_abc) = @$_
    } else {
      ($ident, $cx_or_abc) = (/^(\w+)_(.*)$/);
    }
    my $cx = $cx_or_abc =~ /\d/ ? $cx_or_abc : let2cx($cx_or_abc);
    my $actual_cx = $colx->{$ident};
    outercroak "colx{$ident}=",vis($actual_cx),", expecting $cx (arg=",vis($_),")\n"
      unless u($cx) eq u($actual_cx);
    die "bug" unless sheet()->[2]->{$ident} eq cx2let($cx)."2";
  }
}

####### MAIN ######

#our ($A, $B, $C, $D, $E, $F, $Dalias, $Ealias, $Falias, $Atitle, $Gtitle);
#  Declaration unnecessary because vars tied in BEGIN{...}
BEGIN {
  die "current sheet unexpected" if defined sheet();
  die "current sheet unexpected" if defined sheet();

  # auto-detect title row (on alias)
  autodetect_title_rx;
  read_spreadsheet $infile;
  options silent => (!$debug), verbose => $debug ;  # auto-creates empty sheet
  die "bug1" unless 1 == alias "_dummy" => "Btitle";
  die "bug2" unless title_rx == 1;
  
  # auto-detect title row (on magic-row-hash deref)
  sheet(undef);  # forget previous sheet
  autodetect_title_rx;
  read_spreadsheet $infile;
  options silent => (!$debug), verbose => $debug;
  die "bug1" unless sheet()->[2]->{Btitle} eq "B2";
  die "bug2" unless title_rx == 1;

  # auto-detect title row (on tied variable reference)
  package autodetect_test3 { 
    use Text::CSV::Edit qw(:DEFAULT :STDVARS);
    sheet(undef);  
    our $Btitle;
    tie_column_vars qw($Btitle);
    autodetect_title_rx;
    read_spreadsheet $infile;
    options silent => (!$debug), verbose => $debug;
    apply_torx { die "bug1" unless $Btitle eq "B2" } 2;
    die "bug2" unless title_rx == 1;
  }

  # Do it manually
  sheet(undef);  # forget previous sheet
  tie_column_vars;  # auto-tie to columns whenever they become defined
  options silent => (!$debug);  # auto-creates empty sheet
  read_spreadsheet $infile;
  eval { $_ = alias "_dummy" => "Btitle" }; verif_eval_err(__LINE__);

  { my $s=sheet(); dprint dvis('After reading $infile\n   $$s->{rows}\n   $$s->{colx_desc}\n'); }

  # Aliases without title-row
  alias Aalias => '^';
  alias Aalia2 => 0;
  alias Dalias => 'D';
  alias Ealias => 'E';
  alias Falias => 'F';
  alias Falia2 => 5;
  alias Galias => 'G';
  alias Halias => 'H';
  alias Halia2 => '$';

  check_colx qw(Aalias_0 Aalia2_0 Dalias_D Ealias_E Falias_F
                Falia2_F Galias_G Halias_H Halia2_H);

  # This must be in the BEGIN block so that titles will be auto-exported
  title_rx 1;
}

# "H" is now a title for cx 4, so it maskes the ABC code "H".
# Pre-existing aliases remain pointing to their original columns.
alias Halia3 => 'H';

die "Halia3 gave wrong val"  unless sheet()->[2]->{Halia3} eq "E2";
die "Halias stopped working" unless sheet()->[2]->{Halias} eq "H2";
die "Halia2 stopped working" unless sheet()->[2]->{Halias} eq "H2";
die "Falias stopped working" unless sheet()->[2]->{Falias} eq "F2";

# "F" is also a now title for cx 5, but is the same as the ABC code
alias Falia3 => 'F';
die "Falia3 gave wrong val"  unless sheet()->[2]->{Falia3} eq "F2";
die "Falias stopped working" unless sheet()->[2]->{Falias} eq "F2";

# An alias created with a regexp matches titles
my $Halia3cx = alias Halia4 => qr/^H$/;
die "wrong alias result" unless $Halia3cx == 4;
die "Halia4 gave wrong val"  unless sheet()->[2]->{Halia4} eq "E2";

# Create user alias "A" to another column.  This succeeds because
# ABC codes are hidden by user aliases
alias A => 2;
die "alias 'A' gave wrong val" unless sheet()->[2]->{A} eq "C2";
alias Aalia2 => "A";
die "Aalia2 gave wrong val" unless sheet()->[2]->{Aalia2} eq "C2";
unalias 'A';
die "unaliased 'A' is wrong" unless sheet()->[2]->{A} eq "A2";
die "Aalia2 stopped working" unless sheet()->[2]->{Aalia2} eq "C2";

alias A => 'C';
die "alias 'A' gave wrong val" unless sheet()->[2]->{A} eq "C2";

unalias 'A';
die "'A' after unalias gave wrong val" unless sheet()->[2]->{A} eq "A2";
alias Aalia2 => "A";
die "Aalia2 wrong val" unless sheet()->[2]->{Aalia2} eq "A2";

unalias "Aalia2";
die "unaliased 'A' is wrong" unless sheet()->[2]->{A} eq "A2";

# Try to access the now-undefined alias in a magicrow
eval { $_ = sheet()->[2]->{Aalia2} }; verif_eval_err(__LINE__);

# Try to create a new alias "H" to another column.  This FAILS because
# "H" is a Title, and Titles are always valid.
eval { alias H => 0 }; verif_eval_err(__LINE__);

# Be sure no detritus was left behind when exception was thrown
eval { alias H => 0 } && die "expected exception did not occur";

die "Aalias gave wrong val" unless sheet()->[2]->{Aalias} eq "A2";
die "Dalias gave wrong val" unless sheet()->[2]->{Dalias} eq "D2";
die "Ealias gave wrong val" unless sheet()->[2]->{Ealias} eq "E2";
die "Falias gave wrong val" unless sheet()->[2]->{Falias} eq "F2";
die "Falia2 gave wrong val" unless sheet()->[2]->{Falia2} eq "F2";
die "Galias gave wrong val" unless sheet()->[2]->{Galias} eq "G2";
die "Halias gave wrong val" unless sheet()->[2]->{Halias} eq "H2";
die "Halia2 gave wrong val" unless sheet()->[2]->{Halia2} eq "H2";

# Atitle,Btitle,Multi Word Title C,,,F,Gtitle,Z
check_both('ABCDEFGH');

# Verify error checks
foreach ([f => 0], [flt => 0, f => 1], [lt => $#rows]) {
  my @pairs = @$_; 
  my @saved = ($first_data_rx, $last_data_rx, $title_rx);
  scope_guard {
    first_data_rx $saved[0];
    last_data_rx  $saved[1];
    title_rx      $saved[2];
  };
  while (@pairs) {
    my ($key,$val) = @pairs[0,1]; @pairs = @pairs[2..$#pairs];
    if ($key =~ /f/) {
      first_data_rx $val;
      die 'bug:first_data_rx as getter' unless u(first_data_rx) eq u($val);
    }
    if ($key =~ /l/) {
      last_data_rx $val;
      die 'bug:last_data_rx as getter' unless u(last_data_rx) eq u($val);
    }
    if ($key =~ /l/) {
      title_rx $val;
      die 'bug:title_rx as getter' unless u(title_rx) eq u($val);
    }

    # rx out of range
    eval { apply_torx {  } [0..$#rows+1]; }; verif_eval_err(__LINE__);
    eval { apply_torx {  } [-1..$#rows]; }; verif_eval_err(__LINE__);
    eval { apply_exceptrx {  } [0..$#rows+1]; }; verif_eval_err(__LINE__);
    eval { apply_exceptrx {  } [-1..$#rows]; }; verif_eval_err(__LINE__);

    # Attempt to modify read-only sheet variables
    eval { $num_cols = 33 }; verif_eval_err(__LINE__);
    eval { $title_rx = 33 }; verif_eval_err(__LINE__);

    # Access apply-related sheet vars outside apply
    eval { my $i = $rx }; verif_eval_err(__LINE__);
    eval { my $i = $row }; verif_eval_err(__LINE__);
    eval { my $i = $linenum }; verif_eval_err(__LINE__);
    eval { my $i = $row->{A} }; verif_eval_err(__LINE__);
  }
}

# Flavors of apply
    my %visited;
    sub ck_apply(@) {
      my %actual = map{ $_ => 1 } @_;
      my $visited_str = join ",", sort { $a <=> $b } grep{$visited{$_}} keys %visited;
      foreach(@_){ 
        confess "ck_apply:FAILED TO VISIT $_ (visited $visited_str)" unless $visited{$_}; 
      }
      foreach(keys %visited){ 
        confess "ck_apply:WRONGLY VISITED $_" unless $actual{$_}; 
      }
      while (my($rx,$count) = each %visited) {
        confess "ck_apply:MULTIPLE VISITS TO $rx" if $count != 1;
      }
      %visited = ();
    }
    sub ck_applyargs($$) {
      my ($count, $uargs) = @_;
      die "ck_coldata:WRONG ARG COUNT" unless @$uargs == $count;
      return if $rx <= $title_rx;
      my $L = 'A';
      for my $cx (0..$count-1) {
        my $expval = "${L}$rx";
        confess "ck_coldata:WRONG COL rx=$rx cx=$cx exp=$expval act=$uargs->[$cx]"
          unless $expval eq $uargs->[$cx];
        $L++;
      }
    }
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(2..6);

    first_data_rx 3;
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(3..6);
    first_data_rx undef;
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(2..6);

    last_data_rx 4;
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(2..4);
    last_data_rx undef;
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(2..6);

    first_data_rx 0;  # no-op for apply() because <= title_rx
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(2..6);
    last_data_rx 4;
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(2..4);
    apply_all { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(0..6);
    first_data_rx undef;
    last_data_rx undef;
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(2..6);

    last_data_rx 0; # less than title_rx+1
    apply { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply();
    last_data_rx undef;

    apply_all { $visited{$rx}++; ck_applyargs(0,\@_); } ; ck_apply(0..6);
    foreach my $i (0..6) {
      apply_torx { $visited{$rx}++; ck_applyargs(1,\@_); } $i, 0 ; ck_apply($i);
      apply_torx { $visited{$rx}++; ck_applyargs(2,\@_); } [$i],"Atitle",1 ; ck_apply($i);
      apply_exceptrx { $visited{$rx}++; ck_applyargs(0,\@_); } $i ; ck_apply(0..$i-1,$i+1..6);
      apply_exceptrx { $visited{$rx}++; ck_applyargs(2,\@_); } $i,0,"Btitle" ; ck_apply(0..$i-1,$i+1..6);
      apply_exceptrx { $visited{$rx}++; } [$i] ; ck_apply(0..$i-1,$i+1..6);
    }
    apply_torx { $visited{$rx}++; } [0..6] ; ck_apply(0..6);
    apply_exceptrx { $visited{$rx}++; } [0..6] ; ck_apply();
    apply_exceptrx { $visited{$rx}++; } [0..5] ; ck_apply(6);

# Change title_rx
    title_rx 3;
      bug unless $title_row->[0] eq "A3";
      apply { $visited{$rx}++; } ; ck_apply(4..6);
    title_rx 4;
      bug unless $title_row->[0] eq "A4";
      bug unless $rows[$title_rx]->[1] eq "B4";
      apply { $visited{$rx}++; } ; ck_apply(5..6);
    title_rx undef; #forget_title_rx;
      apply { $visited{$rx}++; } ; ck_apply(0..6);
    title_rx 0;
      apply { $visited{$rx}++; } ; ck_apply(1..6);

    title_rx 1;  # the correct title row
      apply { $visited{$rx}++; } ; ck_apply(2..6);

# Add and drop rows
    new_rows 3,4;
    delete_rows 3,4,5,6;
    check_both('ABCDEFGH');

    new_rows 0,3;      # insert 3 rows at the top

    delete_rows 0..2;  # take them back out
    bug if $title_row->[5] ne 'F';
    check_both('ABCDEFGH');

# Append a new column
    our $Ktitle;  # will be tied
    new_cols '>$', "Ktitle";
    apply {
      $Ktitle = "K$rx";
    };
    check_both('ABCDEFGHK');

# Insert two new columns before that one
    our ($Ititle, $Jtitle); # will be tied
    new_cols '<Ktitle', qw(Ititle Jtitle);
    apply {
      bug "rx=$rx" if $rx <= $title_rx;
      $Ititle = "I$rx"; $Jtitle = "J$rx";
      bug unless $Ktitle eq "K$rx";
    };
    check_both('ABCDEFGHIJK');

# Swap A <-> K

    move_cols ">K", "A";
    check_both('BCDEFGHIJKA');

    move_cols '0', "Ktitle";
    check_both('KBCDEFGHIJA');

# And back and forth

    move_cols "<11", qw(A);  # 'A' means cx 0, i.e. Ktitle
    check_both("BCDEFGHIJAK");

    move_cols "^", "Atitle";
    check_both("ABCDEFGHIJK");

    move_cols '>$', "Multi Word Title C";
    check_both("ABDEFGHIJKC");

    move_cols '>B', '$';
    check_both("ABCDEFGHIJK");

# Delete columns

    apply { bug unless $Gtitle eq "G$rx" };
    delete_cols 'G';
    apply { check_colspec_is_undef('Gtitle') };
    check_both('ABCDEFHIJK');

    delete_cols '^', 'Dalias', '$';
    # "H" (the title of original col E) is no longer a valid ABC code
    # because there are now only 5 columns; so the title "H" can now
    # be used normally, e.g. unquoted
    check_both('BCEFHIJ');


# Put them back


    new_cols '<^', "Atitle" ; apply { $Atitle = "A$rx" };
    check_both('ABCEFHIJ');

    apply_all { return unless $rx==0; $row->[0] = "Restored initial stuff" };

    new_cols '>C',""; apply { $row->[3] = "D$rx" };
    check_both('ABCDEFHIJ');

    new_cols '>F', qw(Gtitle); apply { $Gtitle = "G$rx" };
    check_both('ABCDEFGHIJ');
    apply { bug unless $Gtitle eq "G$rx" };

    new_cols '>$', qw(Ktitle); apply { $Ktitle = "K$rx" };
    check_both('ABCDEFGHIJK');
    apply { bug unless $Gtitle eq "G$rx" };

# only_cols

    only_cols qw(A B C D E F G Z I J K);   # (no-op)
    check_both('ABCDEFGHIJK');
    apply { bug unless $Gtitle eq "G$rx" };

    only_cols qw(A B C D E F Z I J K);  # (deletes G)
    check_both('ABCDEFHIJK');
    apply { check_colspec_is_undef('Gtitle') };

    # Restore col G
    new_cols '>F', "Gtitle" ; apply { $Gtitle = "G$rx" };
    check_both('ABCDEFGHIJK');
    apply { bug unless $Gtitle eq "G$rx" };


# Reverse

    reverse_cols;
    check_both('KJIHGFEDCBA');
    apply { bug unless $Gtitle eq "G$rx" };

    reverse_cols;
    check_both('ABCDEFGHIJK');
    apply { bug unless $Gtitle eq "G$rx" };

# Rename

    rename_cols Atitle => "AAAtitle";
    bug unless $title_row->[0] eq 'AAAtitle';

    our $AAAtitle;  # will be tied, but not imported at start
    apply { bug unless $AAAtitle eq "A$rx" };
    check_both('ABCDEFGHIJK');

    rename_cols AAAtitle => "Atitle";
    check_both('ABCDEFGHIJK');
    apply { bug unless $Gtitle eq "G$rx" };
    check_other_package();

# switch sheet

    my $sheet1 = sheet();
    my $p = sheet();
    bug unless defined($p) && $p == $sheet1;
    bug unless $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__} == $sheet1;
    check_other_package();

    # replace with no sheet
    $p = sheet(undef);
    bug unless $p == $sheet1;
    bug if defined $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__};
    bug if defined eval { my $x = $num_cols; } ; # expect undef or croak
    bug if defined eval { my $x = $Atitle;   } ; # expect undef or croak
    bug if defined $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__};
    $p = sheet();
    bug if defined $p;
    bug if defined $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__};
    bug if defined sheet();
    check_other_package();

    # put back the first sheet
    check_other_package();
    $p = sheet($sheet1);
    bug if defined $p;
    bug unless $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__} == $sheet1;
    apply { bug unless $Gtitle eq "G$rx" };
    check_both('ABCDEFGHIJK');

    # switch to a different sheet
    new_sheet { silent => (! $debug) };
    my $sheet2 = $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__};
    read_spreadsheet $infile2;
    bug unless sheet() == $sheet2;
    apply { check_colspec_is_undef('Gtitle') };
    title_rx 0;
    apply { check_currow_data('PQRSTU*'); };
    apply{ our $TitleP; bug if defined $TitleP; };
    tie_column_vars qr/^Title/;
    apply { our $TitleP; bug unless $TitleP eq "P$rx"; 
            check_colspec_is_undef('Gtitle');
          };
    apply { check_currow_data('PQRSTU*'); };

    # switch back to original sheet
    $p = sheet($sheet1);
    bug unless $p == $sheet2;
    bug unless $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__} == $sheet1;
    apply { our $TitleP; bug unless $Gtitle eq "G$rx"; 
            check_colspec_is_undef('TitleP');
          };
    check_both('ABCDEFGHIJK');

    # and back and forth
    sheet($sheet2);
    apply { our $TitleP; bug unless $TitleP eq "P$rx";
            check_colspec_is_undef('Gtitle');
          };
    sheet($sheet1);
    apply { our $TitleP; bug unless $Gtitle eq "G$rx"; 
            check_colspec_is_undef('TitleP');
          };

    # Verify that the OO api does not do anything to the "current package"
    sheet(undef);
    { my $obj = Text::CSV::Edit->new();
      bug if defined $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__};
    }
    bug if defined sheet();
    check_other_package();

    # Test attaching to another package's sheet
    sheet($sheet1);
    { my $tmp;
      our $Gtitle;
      apply_torx { die "bug($Gtitle)" unless $Gtitle eq "G2" } [2];
      sheet_from_package("Other");
      apply_torx { 
        die "bug($Gtitle)" if defined eval{ $Gtitle };
      } [2];
      eval { my $i = defined $Gtitle }; verif_eval_err(__LINE__);
      apply_torx { bug unless $Other::OtitleA == 314 } [2];
      bug unless $Other::Gtitle eq "non-tied-Gtitle-in-Other";
      bug unless $num_cols == 3;
      bug unless @rows==4 && $rows[2]->[0]==314;
      bug unless @Other::rows==4 && $Other::rows[2]->[1]==159;
      check_other_package();
      sheet(undef);
      bug if defined $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__};
      eval { sheet_from_package(__PACKAGE__) }; bug unless $@ =~ /has no active sheet/;
      bug if defined $Text::CSV::Edit::OO::pkg2currsheet{"".__PACKAGE__};
      bug if defined sheet();
    }

    # Create an empty sheet with defined num_cols, then add new rows
    my $old_num_cols = $sheet1->num_cols;
    my $old_rows = $sheet1->rows;
    my $old_num_rows = scalar @$old_rows;
    new_sheet(num_cols => $old_num_cols, silent => (!$debug));
      bug unless $num_cols == $old_num_cols;
      bug unless @rows == 0;
    new_rows 0, $old_num_rows;
      bug unless @rows == $old_num_rows;
    foreach (0..$old_num_rows-1) {
      $rows[$_] = $old_rows->[$_];
    }
    title_rx 1;
    check_both('ABCDEFGHIJK');

    # Create a sheet from existing data
    new_sheet(rows => $old_rows, silent => (!$debug));
    title_rx 1;
    check_both('ABCDEFGHIJK');

    # Put back sheet1
    sheet($sheet1);

# User-defined attributes
    { my $hash = attributes;
      expect1(ref($hash), "HASH");
      expect1(scalar(keys %$hash),0);
      attributes->{key1} = "val1";
      expect1(ref($hash), "HASH");
      expect1(scalar(keys %$hash),1);
      expect1($hash->{key1}, "val1");
      expect1(scalar(keys %{ attributes() }),1);
    }

# invert

    if ($debug) { print "Before invert:\n"; write_csv "/dev/fd/1" }
    invert;
    die if defined eval('$title_rx');
    if ($debug) { print "After  invert:\n"; write_csv "/dev/fd/1" }

    invert;
    die if defined eval('$title_rx');
    apply {
      return if $rx < 2;  # omit header rows
      check_currow_data('ABCDEFGHIJK');
    };
    title_rx 1;
    check_both('ABCDEFGHIJK');

#FIXME: Add tests of more error conditions, e.g.
#  magic $cellvar throws if column was deleted (and not if not)

# Get rid of changes

    delete_cols qw(I J K);

# write_csv

    my $outfile = "/tmp/output.csv";
    write_csv "$outfile";

    { local $/ = undef; # slurp
      open(CHK, "<:crlf", $outfile) || die "Could not open $outfile : $!";
      my $finaldata = <CHK>;
      close CHK;
      $finaldata =~ s/"//g;
      $finaldata =~ s/^[^\n]*//;
      $testdata =~ s/^[^\n]*//;
      die dvis('\nWRONG DATA WRITTEN\n\n$finaldata\n\n $testdata\n').fmtsheet()
        unless $finaldata eq $testdata;
    }
    check_other_package();

# sort
{   package Other;
    dprint "> Running sort_rows test\n";

    sort_rows { my ($p,$q)=@_; $rows[$p]->[$colx{OtitleA}] <=> $rows[$q]->[$colx{OtitleA}] };
    #dprint dvis('### AFTER SORT:\n  @rows\n   @linenums\n');
    die "rows wrong after sort"
      unless main::arrays_eq [map{$_->[0]} @rows], ["OtitleA",314,777,999];
    die "linenums wrong after sort"
      unless main::arrays_eq \@linenums, [1,3,4,2];
    my @Bs;
    apply { push @Bs, $OtitleB };
    die "apply broken after sort" unless main::arrays_eq \@Bs, [159, 888, 000];
}

if ($debug) {
  ok(1, "With --debug option (run internally or manually)");
} else {
  ok(1, "The whole shebang (without verbose & debug)");
  # Repeat, this time with debug output to a temp file.  Only if it fails
  # do we dump the debug output.
  my ($tfh, $tpath) = tempfile();
  my $pid = fork;
  if ($pid == 0) {
    # CHILD
    close STDERR; open STDERR, ">>", $tpath or die "$tpath : $!";
    close STDOUT; open STDOUT, ">>", $tpath or die "$tpath : $!";
    exec $^X, $0, "--debug";
    die
  }
  die "waitpid bork" unless waitpid($pid,0)==$pid;
  if ($?) {
    warn sprintf "** SUB-PROCESS WAIT STATUS = 0x%04X\n", $?;
    seek $tfh,0,0;
    while (<$tfh>) { print STDERR $_ }
    die "\n*** TEST FAILED when run with debug";
  } else {
    ok(1, "The whole shebang (with verbose & debug)");
  }
} 

exit 0;

