# Copyright © Jim Avera 2012-2021.  This document contains code extracted from
# Data::Dumper and JSON:PP, as noted in adjacent comments, and those extracts
# remain subject to the licenses of their respective modules.  All other
# portions of this document have been released into the Public Domain by
# the copyright owner (jim.avera AT gmail dot com) where legally permitted,
# and otherwise subject to the Creative Commons CC0 license
# (https://creativecommons.org/publicdomain/zero/1.0/)
use strict; use warnings FATAL => 'all'; use 5.010;

use utf8;
# This file contains UTF-8 characters in debug-output strings (e.g. « and »).
# But to see them correctly on a non-Latin1 terminal (e.g. utf-8), your
# program has to say
#   use open ':std' => ':locale';
# or otherwise set the STDERR encoding to match what your terminal expects
# (Perl assumes Latin-1 by default).

package Vis;
use version 0.77; our $VERSION = version->declare(sprintf "v%s", q$Revision: 1.148 $ =~ /(\d[.\d]+)/);

# *** Documentation is at the end ***

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

  ($Vis::pkg) = (caller(1))[0];  # pkg containig user's call
  #Carp::cluck "###Vis::pkg=$Vis::pkg evalarg=$Vis::evalarg\n";

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
    $Vis::evalstr = "package $Vis::pkg;"
                   .' $@=$Vis::saved[0];'
                   .' @Vis::result='.$Vis::evalarg.';';
    eval $Vis::evalstr
  };

  package Vis;

  our (@saved, $evalarg, @result);

  my $at = $@;

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
    push @DB::CARP_NOT, 'Vis';  # Is this necessary?
#    { ###TEMP
#      require PadWalker;
#      for (my $level=1; defined(caller($level-1)); $level++) {
#        my ($pkg,$fn,$line) = caller($level-1);
#        my (undef,undef,undef,$subr) = caller($level-1+1);
#        $subr //= "";
#        my $my_h = PadWalker::peek_my($level);
#        print "--- level $level ${fn}:$line ($pkg)($subr) ---\n";
#        while (my ($vn, $vr) = each %$my_h) {
#          print "  my $vn = ", debugvis($vr), "\n";
#        }
#        my $our_h = PadWalker::peek_our($level);
#        while (my ($vn, $vr) = each %$our_h) {
#          print "  our $vn = ", debugvis($vr), "\n";
#        }
#      }
#    }
    #Carp::croak
    Carp::confess(
      $Vis::error_prefix,
      #(index($at,$evalarg) >= 0 ? "" : "Error interpolating '$evalarg', "),
      "Error interpolating '$evalarg' ",
      "$at (package $Vis::pkg (then ",scalar(caller),"))\n");
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
use Scalar::Util qw(blessed reftype refaddr looks_like_number);
use List::Util qw(any);
use overload ();

use Terminalsize qw(get_terminal_columns);

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

sub debugvis($) {  # for our internal debug messages
  local $/ = "\n";
  confess "should call debugavis" if @_ != 1;
  my $s = Data::Dumper->new([shift])->Useqq(1)->Terse(1)->Indent(0)->Maxdepth(2)->Sortkeys(\&_sortkeys)->Dump;
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
our @EXPORT    = qw(vis  avis  lvis  svis  dvis  hvis  hlvis
                    visq avisq lvisq svisq dvisq hvisq hlvisq
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
$Stringify      = 1           unless defined $Stringify;

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
#
sub u(_) {
  # Show caller's location if arg is tied and the handler dies
  # TODO: Generalize this to other arg references?
  local $@;
  my $result = eval { $_[0] // "undef" };
  if (my $at = $@) {
    $at =~ s/ at (?:\(eval \d+\)|\S+) line \d+\.?\n?\z//s;
    croak $at." (caught in Vis::u)\n";
  }
  $result
}
#sub u(_) { $_[0] // "undef" }

sub vis(_)    { return __PACKAGE__->vnew(@_)->Dump1; }
sub visq(_)   { return __PACKAGE__->vnew(@_)->Useqq(0)->Dump1; }
sub avis(@)   { return __PACKAGE__->anew(@_)->Dump1; }
sub avisq(@)  { return __PACKAGE__->anew(@_)->Useqq(0)->Dump1; }
sub lvis(@)   { return __PACKAGE__->lnew(@_)->Dump1; }
sub lvisq(@)  { return __PACKAGE__->lnew(@_)->Useqq(0)->Dump1; }
sub hvis(@)   { return __PACKAGE__->hnew(@_)->Dump; }
sub hvisq(@)  { return __PACKAGE__->hnew(@_)->Useqq(0)->Dump1; }
sub hlvis(@)  { return __PACKAGE__->hlnew(@_)->Dump; }
sub hlvisq(@) { return __PACKAGE__->hlnew(@_)->Useqq(0)->Dump1; }

# trampolines
#   The interpolation code for svis, etc. must live in package DB and
#   it's immediate caller must be the user's context.
sub svis($)  { @_ = (Vis->snew(@_)); goto &DB::DB_Vis_Interpolate; }
sub dvis($)  { @_ = (Vis->dnew(@_)); goto &DB::DB_Vis_Interpolate; }
sub svisq($) { @_ = (Vis->snew(@_)->Useqq(0)); goto &DB::DB_Vis_Interpolate; }
sub dvisq($) { @_ = (Vis->dnew(@_)->Useqq(0)); goto &DB::DB_Vis_Interpolate; }

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
     || $^O eq 'darwin'
     || $^O eq 'cygwin'
    )
    && -w "/dev/null";
  $result;
}

sub _config_defaults {
  my $self = shift;

  ### Set double-quote style, but preserve any special
  ### $Data::Dumper::Useqq setting, e.g. 'utf8'
  $self->Useqq(1) unless $self->Useqq();

  if (! defined $Maxwidth) {
    # BUG HERE: The special _ filehandle saved by stat gets corrupted
    local *_; # Does not work, see https://github.com/Perl/perl5/issues/19142
    $Maxwidth = get_terminal_columns(reset=>1,debug => $self->{VisDebug})//80
  }

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
  my $obj = &anew;
  $obj->{VisType} = 'l';  # return bare "list,of,items"
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
sub hlnew {
  my $obj = &hnew;
  $obj->{VisType} = 'l';  # return bare "key => value, ..." sans parens
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
    ->Terse($Data::Dumper::Terse)  # normally 0
    # Should other settings be taken from Data::Dumper ???
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

# Reformat output in $_
sub __reformat_dumper_output($$) {
  my ($maxwidth, $debug) = @_;

  print "===== RAW =====\n${_}---------------\n" if $debug;
  #do{ $|=1; require Carp; print Carp::longmess("===== RAW =====\n${_}---------------") } if $debug;

  #WRONG! Maxwidth==0 means no wrap, but we should still condense!
  #return $_ if $maxwidth == 0; # no condensation

  # Split into logical lines.  Basically, split after newlines except
  # for newlines inside quoted strings, or inside sub{...} definitions
  # (these are kept together regardless of maxwidth because the parse code
  # below could not deal with them if spanning multiple lines).
  my @lines;
  die "oops" if defined pos;
  while (/\G(?=.)/cgs) {
    my $startpos = pos;
    while (
      /\G"(?:[^"\\]++|\\.)*+"/cg
      ||
      /\G'(?:[^'\\]++|\\.)*+'/cgs
      ||
      /\Gsub\s*
          \{  # {block}
          (
            [^{}"']++
            |
            "(?:[^"\\]++|\\.)*+"
            |
            '(?:[^'\\]++|\\.)*+'
            |
            \{ (?-1) \}
            |
            \( (?-1) \)
            |
            \[ (?-1) \]
          )*
          \}/cgsx
      ||
      /\G[^s"'\n]+/gsc  # eat until just before possible sub or string
      ||
      /\Gs/gsc          # eat an 's' which was not "sub..."
    )
    {
      #print "### consumed ", debugvis(substr($_,0,pos)), "\n";
    }
    /\G\n/gsc or confess "oops:".debugvis(substr($_,pos));
    push @lines, substr($_, $startpos, pos()-$startpos);
    $startpos = pos();
  }
  undef $_;

  #print "===== ",scalar(@lines)," lines: =====",(map{ "\n  ".debugvis($_) } @lines),"\n" if $debug;

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
                die "VisBUG: Would remove non-indent chars adj=$adj len=",length($1)
                  if length($1) < (-$adj);
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
  unshift @lines, __PACKAGE__." VisBUG: $@" if $@; # show the error, but keep the Data::Dumper output

  $_ = join "", @lines;
  print "===== REFORMATTED =====\n${_}---------------\n" if $debug;
  $_
} # __reformat_dumper_output
sub __postprocess_qquote($) {
  local $_ = shift;
  # "double-quoted string" : Replace \x{ABCD} with actual char if printable
  s/\G(?:[^\\]++
       | \\(?: [^x]| x[^\{] | x\{([0-9A-Fa-f]+)\}
                              # hex escape we don't want to convert?
                              (?(?{do{ local $_ = $^N; length>8 || chr(hex($_)) =~ m{\P{XPosixGraph}} }})|(*FAIL))
                              # not XPosixPrint so we see non-ASCII whitespace
                              # as escapes
           ) )*+
    \K
    \\x\{([0-9A-Fa-f]+)\}
   / chr(hex($^N)) /exsg;
  $_
}
sub __postprocess_squote($) {
  local $_ = shift;
  die "bug" unless index($_,"'")==0;
  # Convert to double-quoted
  # Replace \' with just ', \\ with itself, and double all other backslashes
  #say "##psA «",u($_),"»";
  s/\G [^\\]*+ \K (\\.) / do{
          #say "##SUBST ($1) pos=",pos()-1;
          $1 eq "\\'" ? "'" : $1 eq "\\\\" ? "\\\\" : "\\$1"
                                   } /xesg;
  #say "##psB «",u($_),"»";
  # Backslash $ and @ and "
  pos=0; s/[^\$\@"]*\K([\$\@"])/\\$1/sg;
  # Convert controls to \n etc or \x{} escapes
  # Backslash $ and @
  s/(\P{XPosixPrint})/ $esc{$1} or sprintf("\\x{%X}", ord($1)) /exsg;
  substr($_,0,1) = '"';
  substr($_,-1,1) = '"';
  __postprocess_qquote($_);
}

# Walk an arbitrary structure calling &coderef on each item, stopping
# prematurely if &$coderef returns false, and skipping any reference
# which was visited previously.  Members of containers are visited after
# processing the container item itself, and containerness is checked
# after &$coderef returns so that &$coderef may transform the
# item (by reference through $_[0]) e.g. to replace a container with a scalar.
# RETURNS: The final $&coderef return val (i.e. TRUE unless terminated early)
sub _walk($$;$);
sub _walk($$;$) {  # (coderef, item [, seenhash])
  no warnings 'recursion';
  my $seen = $_[2] // {};
  # Test for recursion both before and after calling the coderef, in case the
  # code unconditionally clones or otherwise replaces the item with new data.
  #my $orig_item = $_[1];
  #return 0 if (reftype($orig_item) && $seen->{ refaddr($orig_item) }++);
  if (reftype($_[1])) {
    my $refaddr0 = refaddr($_[1]);
    return 1 if $seen->{$refaddr0}; # increment only below
  }
  # Now call the coderef and re-check the item
  my $r = &{ $_[0] }($_[1]);
  return $r unless (my $reftype = reftype($_[1]));
  my $refaddr1 = refaddr($_[1]);
  return $r if $seen->{$refaddr1}++;
  return 0 unless $r;
  if ($reftype eq 'ARRAY') {
    foreach (@{$_[1]}) {
      return 0 unless _walk($_[0], $_, $seen);
    }
  }
  elsif ($reftype eq 'HASH') {
    foreach (values %{$_[1]}) {
      return 0 unless _walk($_[0], $_, $seen);
    }
  }
  1
}

# Edit Values: _walk() is called with the specified subref on the
# array of Values in the object.  The Values are cloned first to
# avoid corrupting the user's data structure.
sub _Modify_Values { #internal method
  my ($self, $coderef) = @_;
  my @values = $self->Values;
  unless ($self->{VisCloned}++) {
    require Clone;
    @values = map{ Clone::clone($_) } @values;
  }
  _walk($coderef, \@values);
  $self->Values(\@values);
}

# Overload Data::Dumper::Values() so we can clear VisCloned if Values change
sub Values {
  my $self = shift;
  delete $self->{VisCloned} if @_ > 0;
  $self->SUPER::Values(@_);
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

# Dump1 is like Dump except:
#   1. The user's frame is up one level.
#   2. The immediate caller is in package DB
# For dvis etc., this is called from the interpolation code in package DB
# For user-called Dump(), this is called via a thunk in package DB
sub Dump1 {
  local $_;

  my $self = $_[0];

# print "##Dump1 caller(0)=",Vis::debugavis((caller(0))[0,1,2])," VT=",Vis::debugvis($self->{VisType}),"\n";
# print "##Dump1 caller(1)=",Vis::debugavis((caller(1))[0,1,2])," VT=",Vis::debugvis($self->{VisType}),"\n";

  if (($self->{VisType}//"") =~ /^[sd]/) {
    @_ = ($_[0]);
    goto &DB::DB_Vis_Interpolate;
  }

  my ($debug, $maxwidth, $maxstringwidth)
    = @$self{qw/VisDebug Maxwidth MaxStringwidth/};

  my $cloned;

  if (my $stringify = $self->Stringify()) { # not undef or "0"
    $stringify = [ $stringify ] unless ref($stringify) eq 'ARRAY';
    my %hits;
    _walk(
      sub {
        if (my $class = blessed($_[0])) {
          if (overload::Method($class,'""')) { # implements operator stringify
            my $shouldwe;
            foreach my $mod (@$stringify) {
              # Stringify may be class name, a Regexp matching class name,
              # or a non-zero number (i.e. "true")
              $shouldwe=1, last if
                ref($mod) eq "Regexp"
                  ? $class =~ /$mod/
                  : ($class eq $mod || ($mod && $mod =~ /^\d+$/))
            }
            $hits{ref $_[0]} = $class if $shouldwe;
          }
        }
        1
      }, [$self->Values]
    );
    if (%hits) {
      $self->_Modify_Values(sub {
        if (my $class = $hits{ref $_[0]}) {
          $_[0] = "($class)".$_[0]
        }
        1;
      });
    }
  }

  if (($maxstringwidth//0) > 0) {
    my $truncsuf = $self->{Truncsuffix};
    my $maxwid = $maxstringwidth + length($truncsuf);
    if (! _walk(sub{
                 ! (ref($_[0]) eq "" && length($_[0]//"undef") > $maxwid)
                }, [$self->Values]))
    {
      $self->_Modify_Values(
        sub {
          if (ref($_[0]) eq "") { # a defined scalar
            if (length($_[0]) > $maxwid) {
              $_[0] = substr($_[0],0,$maxstringwidth).$truncsuf;
            }
          }
          1
        }
      );
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
    $self->Pad("");
    #$self->Pad(" ");
  }

  # 2/21/22: Previously we relied on Data::Dumper to produce double-quoted
  # strings, using a repaired qquote sub which allowed unicode chars to appear
  # unmolested with Useqq("utf8") (the "repair" was to make some undocumented
  # code in Data::Dumper which seems to have been intended to make Unicode
  # characters appear as themselves in double-quoted strings actually work.)
  #
  # But now another deficiency has been discovered; Data::Dumper will
  # format scalars set to strings like "6" as numbers rather than strings
  # *except* with Useqq(false) and Useperl(false).
  #
  # So now we always call Data::Dumper (as SUPER) in single-quote mode,
  # and hand-convert the result to double-quoted if Useqq was requested.
  # The "repaired" qquote sub has been removed.
  my $useqq = $self->Useqq();
  $self->Useqq(0);
  {
    confess "bug:defined:$_" if defined($_);
    my @values = $self->Values;
    # Data::Dumper always quotes floats to avoid inter-platform dependencies.
    # We don't want that; we can catch the simple case here of a single value;
    # however floats in structures, e.g. [42, 3.14] will not be handled and
    # so will come out as quoted strings.  Sigh.
    my $val0 = $values[0];
    if (defined($val0) && ref($val0) eq "" && looks_like_number($val0)) {
      # pad, quotes and newline mimic Data::Dumper::Dump output
      $_ = $pad
          . (show_as_number($val0) ? $val0 : "'$val0'")
          . "\n";
    } else {
      # Hack -- save/restore punctuation variables corrupted by Data::Dumper
      my ($sAt, $sQ) = ($@, $?);

      # HERE is where Data::Dumper formats the value(s)
      $_ = $self->SUPER::Dump;

      ($@, $?) = ($sAt, $sQ);
    }
  }
  $self->Useqq($useqq);

  s/^ *// if $maxwidth==0;  # remove initial forced pad from Indent(0)

  if ($pad ne ""){
    # Remove initial pad; will be put back later
    s/^\Q${pad}\E//mg || $maxwidth==0 || confess "bug: pad='${pad}' maxwidth=$maxwidth '$_'";
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
    __reformat_dumper_output($maxwidth, $debug);
  }
  # else: one long line, produced using Indent(0) above;

  # As noted above, Data::Dumper is always called in single-quoted mode
  # to avoid various bugs.  Hand-convert to double-quoted if Useqq was
  # in effect and convert \x{ABCD} escapes to actual characters where possible
  # N.B. Data::Dumper 2.174 forces double-quoted output regardless of Useqq
  # if certain (Unicode?) chars are present.
  { my $newstr = "";
    while ((pos()//0) < length()) {
      if (/\G([^"'\\]++)/sgc) {
        $newstr .= $1;
      }
      elsif (/\G(\\+ (?: # ref to a [ref to a ...] scalar
                        [^"'\\]++             # some kind of number, or undef
                      | "(?:[^"\\]++|\\.)*+"  # "dq string"
                      | '(?:[^'\\]++|\\.)*+'  # 'sq string'
                     ))/xsgc) {
        $newstr .= $1;
      }
      elsif (/\G("(?:[^"\\]++|\\.)*+")/sgc) {
        $newstr .= __postprocess_qquote($1);
      }
      elsif (/\G('(?:[^'\\]++|\\.)*+')/sgc) {
        $newstr .= $useqq ? __postprocess_squote($1) : $1;
      }
      else {
        confess "VisBUG: Did not parse ",Vis::debugvis(substr($_,pos()//0))," pos=",Vis::debugvis(pos), " \$_=",Vis::debugvis($_),"\n";
die;
      }
    }
    $_ = $newstr;
  }

  if ($self->{VisType}) {
    s/\s+\z//s;  # omit final newline except when emulating Data::Dumper
    if ($self->{VisType} eq 'a') {
      # Convert {...} or [...] to (...)
      s/^( *)[\[{]/$1\(/ or confess "bug1($_)";
      s/[\]}]$/\)/ or confess "bug2($_)";
    }
    elsif ($self->{VisType} eq 'l') {
      s/^( *)[\[{(]/$1/ or confess "bug3($_)"; # convert to "bare,list,of,items"
      s/[\]})]$// or confess "bug4($_)";
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

  local $_ = shift;
  if (ref) {
    # If a ref to an aggregate, convert to a string
    $_ = __PACKAGE__->vnew($_)->Useqq(0)->Dump;
  }

  # Now we have a simple scalar.
  # Don't use Data::Dumper because it will change
  # wide characters to \x{...} escapes.  For qsh() we assume the user
  # will encode the resulting string as needed, so leave wide chars as-is.

  if (/["\$`!\\\x{00}-\x{1F}\x{7F}]/) {
    s/'/'\\''/g; # foo'bar => foo'\''bar
    return "'${_}'";
  } else {
    # Prefer "..." if no shell escapes are needed (assume all > 7F are safe)
    return "\"${_}\"";
  }
}

sub qsh(_) {
  local $_ = shift;
  defined $_
    ? (/[^-=\w_\/:\.,]/ || $_ eq "" || ref) ? forceqsh($_) : $_
    : "undef";
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
    | \#?[^\w\s\{\$]     # $#- $^ $? $\ (regular 'punctuation' variable)
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
    # Multiple values are not possible except via internal constructors.
    local $_ = join "", map {defined($_) ? $_ : '<undef arg>'} $self->Values();
    while (1) {
      #print "### pos()=",Vis::u(pos()),"\n" if $debug;

      # Recognize $ or @ followed by various forms
      my ($sigl, $rhs);
      if (/\G (?!\\)([\$\@\%])/xsgc) {
        ($sigl, $rhs) = ($1, "");
        $rhs = $1 if /\G($variable_re)/sgc; # includes {ref expr}
        for(;;) {
          last unless
            /\G ( ${slice_re} )/xsgc         # {key} or [ix]
            ||
            /\G ( ${scalar_index_re} )/xsgc  # ->{key} or ->[ix]
            ||
            /\G ( ${method_call_re} )/xsgc   # ->method(args)
            ||
            /\G ( \$+ ) /xsgc          # chained de-refs
            ;
          $rhs .= $1
        }
      }
      elsif (/\G (?!\\)(\%)( $variable_re )/xsgc) {
        # %name
        ($sigl, $rhs) = ($1, $2);
      }

      if (defined $rhs) {
        push @actions, ['e',$sigl,$rhs];  # eval $sigl$rhs
        if ($debug) {
          print "### regex match ($_): sigl='${sigl}' rhs='${rhs}'\n";
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
          die "Syntax error (or Vis bug): next:",substr($tmp,pos//0,4),
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
      print "# after Vis_Eval: items=",Vis::debugavis(@items),"\n" if $debug;

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
        print "### extra_padlen=$extra_padlen varlabel='$varlabel' pad='$pad' \$1='$1' result='$result'\n" if $debug;
      }

      my $autopad = $pad.(" " x $extra_padlen);
      # $sigl may be '$$' etc. if there are chained derefs
      if ($sigl =~ /^\$/) {
        $self->Reset()->Pad($autopad)->Values([$items[0]])->{VisType}='v';
      }
      elsif ($sigl eq '@') {
        $self->Reset()->Pad($autopad)->Values([\@items])->{VisType} = 'a';
      }
      elsif ($sigl eq '%') {
        my %hash = @items;
        $self->Reset()->Pad($autopad)->Values([\%hash])->{VisType} = 'a';
      }
      else { die "bug($sigl)" }

      my $s = $self->Dump1;  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<

      print "# # Vis::Indent=$Vis::Indent Indent()=", $self->Indent(),
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

=head2 svis 'string to be interpolated'

Returns the argument with variable references and escapes interpolated
as in in Perl double-quotish strings except that variable values are
formatted using C<vis()> or C<avis()> for $ or @ expressions, respectively.
In addition, C<%name> is interpolated as S<<< C<< (key => value ...) >> >>>, and
C<< $name->method(...) >> is interpolated as a method call (if args are given,
no space is allowed before the open parenthesis).

Multi-line structures are indented to line up with their starting position,
taking into account any preceeding text on the same line.

The argument string should be written SINGLE QUOTED so Perl will not
interpolate it before passing to svis().

=head2 dvis 'string to be interpolated'

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

Objects of the indicated class(es) are allowed to convert themselves to
strings before being dumped.  A "(classname)" prefix will be prepended
to the string provided by the class.

Strinification is done by concatenating "" to the object, and it is
done only if the class implements the "" operator.

The Stringify property value may be:

1) 1 (the default) to enable stringification of all objects which
implement the "" operator.

2) 0, undef, or [] to completely disable the feature.

3) A string giving the name of a class, a qr/regex/ matching class names,
or a ref to an array of those things; objects blessed into
any matching class will be stringified if possible.

When stringification is performed, a _deep copy_ is first made of the
entire data, which will impact performance for large structures.


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

=head2 qsh $word

=head2 qshpath $path_with_tilde_prefix

The "word" ($_ by default) is quoted if necessary for parsing
by /bin/sh, which has different quoting rules than Perl.
"Double quotes" are used when no escapes would be needed,
otherwise 'single quotes'.

An item which contains only "safe" characters is returned unquoted.

References are formatted as with C<vis()> and the resulting string quoted.
Undefined values appear as C<undef> without quotes.

C<qshpath> is like C<qsh> except that an initial ~ or ~username is left
unquoted.  Useful with shells such as bash or csh.

=head2 forceqsh $word

The argument is quoted for /bin/sh even if not necessary.

Unlike C<qsh>, C<forceqsh> requires an explicit argument which must be defined.

=head1 PERFORMANCE

Vis calls Data::Dumper and then condenses the output.
C<svis> and C<dvis> must also parse the string to be interpolated.
For most purposes the extra overhead is not significant.

Single-quote style (C<Useqq(0)>) runs faster because
the underlying Data::Dumper implementation uses XS (C code).

For high-volume applications, such as to serialize a large data set,
C<< Data::Dumper->new() >> may be called directly.

=head1 SEE ALSO

Data::Dumper

=head1 CREDITS

This module contains code snippets copied from Data::Dumper and JSON::PP.

=head1 AUTHOR

Jim Avera  (jim.avera AT gmail dot com)

=cut

#!/usr/bin/perl
# Tester for module Vis.  TODO: Convert to CPAN module-test setup
use strict; use warnings ; use feature qw(state);
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

sub check($$@) {
  my ($code, $expected_arg, @actual) = @_;
  my @expected = ref($expected_arg) eq "ARRAY" ? @$expected_arg : ($expected_arg);
  confess "\nTEST FAILED: $code\n"
         ."Expected ".scalar(@expected)." results, but got ".scalar(@actual).":\n"
         ."expected=(@expected)\n"
         ."actual=(@actual)\n"
    if @expected != @actual;
  foreach my $i (0..$#actual) {
    my $actual = $actual[$i];
    my $expected = $expected[$i];
    if (ref($expected) eq "Regexp") {
      confess "\nTEST FAILED: $code\n"
             ."Expected:${expected}\n"
             ."Got     :".u($actual)."«end»\n"
        unless $actual =~ ($expected // "Never Matched");
    } else {
      confess "\nTEST FAILED: $code\n"
             ."Expected:".u($expected)."«end»\n"
             ."Got     :".u($actual)."«end»\n"
        unless (!defined($actual) && !defined($expected))
               || (defined($actual) && defined($expected) && $actual eq $expected);
    }
  }
}

# Run a variety of tests on an item which is a string or
# a stringified object, with code evaluated in the caller's context
# so that "use bignum" or similar may be in effect.
sub checkstringy(&$$) {
  my ($doeval, $item, $expected_re) = @_;
  my $expqq_re = "\"${expected_re}\"";
  my $expq_re  = "'${expected_re}'";
  foreach (
    [ 'Vis->vnew($_[1])->Dump',  '_Q_' ],
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
    local $Vis::Maxwidth = 0;  # disable wrapping
    check $code, qr/$exp/, $doeval->($code, $item) ;
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
die "Expected initial Vis::Maxwidth to be undef" if defined $Vis::Maxwidth;
{ local $ENV{COLUMNS} = 66;
  vis(123);
  die "Vis::Maxwidth does not honor ENV{COLUMNS}" unless $Vis::Maxwidth == 66;
  undef $Vis::Maxwidth;  # re-enable auto-detect
}
if (Vis::_unix_compatible_os()) {
  delete local $ENV{COLUMNS};
  my $expected = `tput cols`;  # may default to 80 if no tty
  die "bug: Vis::Maxwidth not undef" if defined($Vis::Maxwidth);
  vis(123);
  die "Vis::Maxwidth ",u($Vis::Maxwidth)," not defaulted correctly, expecting $expected" unless $Vis::Maxwidth == $expected;
  undef $Vis::Maxwidth;  # re-enable auto-detect
}
die "bug: Vis::Maxwidth not undef" if defined($Vis::Maxwidth);
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
  $? = 0;
}

##################################################
# Check Useqq('utf8') support
##################################################
{
  my $vis_outstr = Vis->new([$unicode_str])->Terse(1)->Useqq(1)->Dump;
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
if (! ref Vis->new([1])->Useqq(undef)) {
  warn "WARNING: Data::Dumper methods do not recognize undef boolean args as 'false'.\n";
  $undef_as_false = 0;
}

# Basic test of OO interfaces
{ my $code="Vis->vnew('foo')->Dump;"; check $code, '"foo"',   eval $code }
{ my $code="Vis->anew('foo')->Dump;"; check $code, '("foo")', eval $code }
{ my $code="Vis->hnew(k=>'v')->Dump;"; check $code,'(k => "v")',eval $code}
{ my $code="Vis->dnew('foo')->Dump;"; check $code, 'foo',     eval $code }
{ my $code="Vis->snew('foo')->Dump;"; check $code, 'foo',     eval $code }

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
    foreach my $ctor (qw(vnew anew snew dnew hnew)) {
      {
        my $v = eval "{ local \$Vis::$name = \$value;
                        Vis->\$ctor('k',[\"test \$testval\"])->$name();
                      }";
        die "bug:$@ " if $@;
        die "\$Vis::$name value is not preserved by Vis->$ctor\n",
            "(Set \$Vis::$name=",u($value)," but $name() returned ",u($v),")\n"
         unless (! defined $v and ! defined $value) || ($v eq $value);
      }
      {
        my $v = eval "{ Vis->\$ctor('k',[\"test \$testval\"])->$name(\$value)->$name() }";
        die "bug:$@ " if $@;
        die "Vis::$name(v) does not set it's value!\n",
            "(called $name(",u($value),") but $name() returned ",u($v),")\n"
         unless (! defined $v && ! defined $value) || ($v eq $value);
      }
    }
  }
}

# ---------- Check formatting or interpolation --------

sub MyClass::meth {
  my $self = shift;
  return @_ ? [ "methargs:", @_ ] : "meth_with_noargs";
}

$Vis::Maxwidth = 72;

@ARGV = ('fake','argv');
$. = 1234;
$ENV{EnvVar} = "Test EnvVar Value";

my %toplex_h = ("" => "Emp", A=>111,"B B"=>222,C=>{d=>888,e=>999},D=>{});
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

our %localized_h = (key => "should never be seen");
our @localized_a = ("should never be seen");
our $localized_ar = \@localized_a;
our $localized_hr = \%localized_h;
our $localized_obj = \%localized_h;

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
# dvis & svis no longer accept multiple args
#{ my $code = 'svis(q($_ con),q(caten),q(ated\n))';
#  check $code, "\"${_}\" concatenated\n", eval $code;
#}
#{ my $code = 'dvis(q($_ con),q(caten),q(ated\n))';
#  check $code, "\$_=\"${_}\" concatenated\n", eval $code;
#}
{ my $code = 'avis(undef)'; check $code, "(undef)", eval $code; }
{ my $code = 'hvis("foo",undef)'; check $code, "(foo => undef)", eval $code; }
{ my $code = 'vis(undef)'; check $code, "undef", eval $code; }
{ my $code = 'svis(undef)'; check $code, "<undef arg>", eval $code; }
{ my $code = 'dvis(undef)'; check $code, "<undef arg>", eval $code; }
{ my $code = 'dvisq(undef)'; check $code, "<undef arg>", eval $code; }
#{ my $code = 'svis("foo",undef)'; check $code, "foo<undef arg>", eval $code; }
#{ my $code = 'dvis("foo",undef)'; check $code, "foo<undef arg>", eval $code; }
#{ my $code = 'dvisq("foo",undef)'; check $code, "foo<undef arg>", eval $code; }

{ my $code = q/my $s; my @a=sort{ $s=dvis('$a $b'); $a<=>$b }(3,2); "@a $s"/ ;
  check $code, '2 3 a=3 b=2', eval $code;
}

# Check that $1 etc. can be passed (this was once a bug...)
{ my $code = '" a~b" =~ / (.*)/ && qsh($1)'; check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)/ && qshpath($1)'; check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)/ && forceqsh($1)'; check $code, '"a~b"', eval $code; }
{ my $code = '" a~b" =~ / (.*)/ && vis($1)'; check $code, '"a~b"', eval $code; }

{ my $code = 'my $vv=123; \' a $vv b\' =~ / (.*)/ && dvis($1)'; check $code, 'a vv=123 b', eval $code; }

# Check Deparse support
{ my $data = eval 'BEGIN{ ${^WARNING_BITS} = 0 } no strict; no feature;
                   sub{ my $x = 42; };';
  { my $code = 'vis($data)'; check $code, "sub { \"DUMMY\" }", eval $code; }
  $Data::Dumper::Deparse = 1;
  { my $code = 'vis($data)'; check $code, "sub {\n    my \$x = 42;\n}", eval $code; }
}

# Floating point values (single values special-cased to show not as 'string')
{ my $code = 'vis(3.14)'; check $code, '3.14', eval $code; }
# But multiple values are sent through Data::Dumper, so...
{ my $code = 'vis([3.14])'; check $code, '["3.14"]', eval $code; }

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
  checkstringy(sub{eval $_[0]}, $bigf, qr/(?:\(Math::BigFloat[^\)]*\))?${bigfstr}/);

  my $bigi = eval $bigistr // die;
  die unless blessed($bigi) =~ /^Math::BigInt/;
  checkstringy(sub{eval $_[0]}, $bigi, qr/(?:\(Math::BigInt[^\)]*\))?${bigistr}/);

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
  checkstringy(sub{eval $_[0]}, $rat, qr/(?:\(Math::BigRat[^\)]*\))?${ratstr}/);
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
    checkstringy(sub{eval $_[0]}, $bigf, qr/(?:\(Math::BigFloat[^\)]*\))?${bigfstr}/);
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
  s/\t/<tab>/sg;
  s/( +)$/"<space>" x length($1)/seg;
  s/\n/<newline>/sg;
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
  my %sublex_h = %toplex_h;
  my @sublex_a = @toplex_a;
  my $sublex_ar = \@sublex_a;
  my $sublex_hr = \%sublex_h;
  my $sublex_obj = $toplex_obj;
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
  local %localized_h = %toplex_h;
  local @localized_a = @toplex_a;
  local $localized_ar = \@toplex_a;
  local $localized_hr = \%localized_h;
  local $localized_obj = $toplex_obj;

  for my $test (
    [ q(aaa\\\\bbb), q(aaa\bbb) ],

    #[ q($unicode_str\n), qq(unicode_str=\" \\x{263a} \\x{263b} \\x{263c} \\x{263d} \\x{263e} \\x{263f} \\x{2640} \\x{2641} \\x{2642} \\x{2643} \\x{2644} \\x{2645} \\x{2646} \\x{2647} \\x{2648} \\x{2649} \\x{264a} \\x{264b} \\x{264c} \\x{264d} \\x{264e} \\x{264f} \\x{2650}\"\n) ],
    [ q($unicode_str\n), qq(unicode_str="${unicode_str}"\n) ],

    # Now Data::Dumper outputs \x{...} escapes instead of octal
    #[ q($byte_str\n), qq(byte_str=\"\\n\\13\\f\\r\\16\\17\\20\\21\\22\\23\\24\\25\\26\\27\\30\\31\\32\\e\\34\\35\\36\"\n) ],
    [ q($byte_str\n), qq(byte_str=\"\\n\\x{B}\\f\\r\\x{E}\\x{F}\\x{10}\\x{11}\\x{12}\\x{13}\\x{14}\\x{15}\\x{16}\\x{17}\\x{18}\\x{19}\\x{1A}\\e\\x{1C}\\x{1D}\\x{1E}\"\n) ],

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
    #[ q($;\n), qq(\$;=\"\\34\"\n) ],
    [ q($;\n), qq(\$;=\"\\x{1C}\"\n) ],
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

        ( $] >= 5.022001 && $] <= 5.022001
            ?  (do{ state $warned = 0;
                    warn "\n\n** obj->method() tests disabled ** due to Perl v5.22.1 segfault!\n\n"
                     unless $warned++; ()
                  },())
            : (
               [ qq(\$${name}_obj->meth ()), qq(${name}_obj->meth="meth_with_noargs" ()) ],
               [ qq(\$${name}_obj->meth(42)), qq(${name}_obj->meth(42)=["methargs:",42]) ],
              )
        ),

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
    #warn "##^^^^^^^^^^^ dvis_input='$dvis_input' expected='$expected'\n";

    { local $@;  # check for bad syntax first, to avoid uncontrolled die later
      # For some reason we can't catch exceptions from inside package DB.
      # undef is returned but $@ is not set
      my $ev = eval { "$dvis_input" };
      die "Bad test string:$dvis_input\nPerl can't interpolate it"
         .($@ ? ":\n  $@" : "\n")
        if $@ or ! defined $ev;
    }

    my sub punctbug($$$) {
      my ($varname, $act, $exp) = @_;
      confess "ERROR (testing '$dvis_input'): $varname was not preserved (expected $exp, got $act)\n"
    }
    my sub checkspunct($$$) {
      my ($varname, $actual, $expecting) = @_;
      check "dvis('$dvis_input') : $varname NOT PRESERVED : ",
            $actual//"<undef>", $expecting//"<undef>" ;
    }
    my sub checknpunct($$$) {
      my ($varname, $actual, $expecting) = @_;
      # N.B. check() compaares as strings
      check "dvis('$dvis_input') : $varname NOT PRESERVED : ",
            defined($actual) ? $actual+0 : "<undef>",
            defined($expecting) ? $expecting+0 : "<undef>" ;
    }

    for my $use_oo (0,1) {
      my $actual;
      { # Verify that special vars are preserved and don't affect Vis
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
          ? Vis->dnew($dvis_input)->Dump   # <<<<<<<<<<<<<<< HERE
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
      }
      unless ($expected eq $actual) {
        confess "\ndvis (oo=$use_oo) test failed: input «",
                show_white($dvis_input),"»\n",
                "Expected:\n",show_white($expected),"«end»\n",
                "Got:\n",show_white($actual),"«end»\n"
      }
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
      my $act = Vis->vnew($input)->Useqq($useqq)->Dump;
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
