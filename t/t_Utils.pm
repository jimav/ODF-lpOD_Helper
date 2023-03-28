package t_Utils;

require Exporter;
use parent 'Exporter';

our @EXPORT = qw/displaystr fmt_codestring timed_run
                 checkeq_literal check
                 @quotes
                /;

use Carp;

# Format a Unicode string in «french quotes» and also with hex escapes
# (so we can still see something useful on non-Unicode platforms).
#our @quotes = ("«", "»");
our @quotes = ("<<", ">>");
sub displaystr($) {
  my ($input) = @_;
  return "undef" if ! defined($input);
  chomp( my $dd = Data::Dumper->new([$input])->Useqq(1)->Terse(1)->Indent(0)->Dump );
  $quotes[0].$input.$quotes[1]."($dd)"
}

sub fmt_codestring($;$) { # returns list of lines
  my ($str, $prefix) = @_;
  $prefix //= "line ";
  my $i; map{ sprintf "%s%2d: %s\n", $prefix,++$i,$_ } (split /\n/,$_[0]);
}

sub timed_run(&$@) {
  my ($code, $maxcpusecs, @codeargs) = @_;

  eval { require Time::HiRes };
  my $getcpu = defined(eval{ &Time::HiRes::clock() })
    ? \&Time::HiRes::clock : sub{ my @t = times; $t[0]+$t[1] };

  my $startclock = &$getcpu();
  my (@result, $result);
  if (wantarray) {@result = &$code(@codeargs)} else {$result = &$code(@codeargs)};
  my $cpusecs = &$getcpu() - $startclock;
  confess "TOOK TOO LONG ($cpusecs CPU seconds vs. limit of $maxcpusecs)\n"
    if $cpusecs > $maxcpusecs;
  if (wantarray) {return @result} else {return $result};
}

# Nicer alternative to check() when 'expected' is a literal string, not regex
sub checkeq_literal($$$) {
  my ($desc, $exp, $act) = @_;
  #confess "'exp' is not plain string in checkeq_literal" if ref($exp); #not re!
  $exp = show_white($exp); # stringifies undef
  $act = show_white($act);
  return unless $exp ne $act;
  my $posn = 0;
  for (0..length($exp)) {
    my $c = substr($exp,$_,1);
    last if $c ne substr($act,$_,1);
    $posn = $c eq "\n" ? 0 : ($posn + 1);
  }
  @_ = ( "\n**************************************\n"
        ."${desc}\n"
        ."Expected:\n".displaystr($exp)."\n"
        ."Actual:\n".displaystr($act)."\n"
        # + for opening « or << in the displayed str
        .(" " x ($posn+length($quotes[0])))."^\n"
        .visFoldwidth()."\n" ) ;
  #goto &Carp::confess;
  Carp::confess(@_);
}

# Convert a literal "expected" string which contains things which are
# represented differently among versions of Perl and/or Data::Dumper
# into a regex which works with all versions.
# As of 1/1/23 the input string is expected to be what Perl v5.34 produces.
our $bs = '\\';  # a single backslash
sub expstr2re($) {
  local $_ = shift;
  confess "bug" if ref($_);
  unless (m#qr/|"::#) {
    return $_; # doesn't contain variable-representation items
  }
  # In \Q *string* \E the *string* may not end in a backslash because
  # it would be parsed as (\\)(E) instead of (\)(\E).
  # So change them to a unique token and later replace problematic
  # instances with ${bs} variable references.
  s/\\/<BS>/g;
  $_ = '\Q' . $_ . '\E';
  s#([\$\@\%])#\\E\\$1\\Q#g;

  if (m#qr/#) {
    # Canonical: qr/STUFF/MODIFIERS
    # Alternate: qr/STUFF/uMODIFIERS
    # Alternate: qr/(?^MODIFIERS:STUFF)/
    # Alternate: qr/(?^uMODIFIERS:STUFF)/
#say "#XX qr BEFORE: $_"; 
    s#qr/([^\/]+)/([msixpodualngcer]*)
     #\\E\(\\Qqr/$1/\\Eu?\\Q$2\\E|\\Qqr/(?^\\Eu?\\Q$2:$1)/\\E\)\\Q#xg
      or confess "Problem with qr/.../ in input string: $_";
#say "#XX qr AFTER : $_"; 
  }
  if (m#\{"([\w:]+).*"\}#) {
    # Canonical: fh=\*{"::\$fh"}  or  fh=\*{"Some::Pkg::\$fh"}
    #   which will be encoded above like ...\Qfh=<BS>*{"::<BS>\E\$\Qfh"}
    # Alt1     : fh=\*{"main::\$fh"}
    # Alt2     : fh=\*{'main::$fh'}  or  fh=\*{'main::$fh'} etc.
#say "#XX fh BEFORE: $_"; 
    s{(\w+)=<BS>\*\{"(::)<BS>([^"]+)"\}}
     {$1=<BS>*{\\E(?x: "(?:main::|::) \\Q<BS>$3"\\E | '(?:main::|::) \\Q$3'\\E )\\Q}}xg
    |
    s{(\w+)=<BS>\*\{"(\w[\w:]*::)<BS>([^"]+)"\}}
     {$1=<BS>*{\\E(?x: "\\Q$2<BS>$3"\\E | '\\Q$2$3'\\E )\\Q}}xg
    or
      confess "Problem with filehandle in input string <<$_>>";
#say "#XX fh AFTER : $_"; 
  }
  s/<BS>\\/\${bs}\\/g;
  s/<BS>/\\/g;
#say "#XX    FINAL : $_"; 

  my $saved_dollarat = $@;
  my $re = eval "qr{${_}}"; die "$@ " if $@;
  $@ = $saved_dollarat;
  $re
}

# check $test_desc, string_or_regex, result
sub check($$@) {
  my ($desc, $expected_arg, @actual) = @_;
  local $_;  # preserve $1 etc. for caller
  my @expected = ref($expected_arg) eq "ARRAY" ? @$expected_arg : ($expected_arg);
  confess "zero 'actual' results" if @actual==0;
  confess "ARE WE USING THIS FEATURE? (@actual)" if @actual != 1;
  confess "ARE WE USING THIS FEATURE? (@expected)" if @expected != 1;
  confess "\nTESTa FAILED: $desc\n"
         ."Expected ".scalar(@expected)." results, but got ".scalar(@actual).":\n"
         ."expected=(@expected)\n"
         ."actual=(@actual)\n"
         ."\$@=$@\n"
    if @expected != @actual;
  foreach my $i (0..$#actual) {
    my $actual = $actual[$i];
    my $expected = $expected[$i];
    if (!ref($expected)) {
      # Work around different Perl versions stringifying regexes differently
      $expected = expstr2re($expected);
    }
    if (ref($expected) eq "Regexp") {
      unless ($actual =~ $expected) {
        @_ = ( "\n**************************************\n"
              ."TESTb FAILED: ".$desc."\n"
              ."Expected (Regexp):\n".${expected}."<<end>>\n"
              ."Got:\n".displaystr($actual)."<<end>>\n"
              .visFoldwidth()."\n" ) ;
        Carp::confess(@_); #goto &Carp::confess;
      }
#say "###ACT $actual";
#say "###EXP $expected";
    } else {
      unless ($expected eq $actual) {
        @_ = ("TESTc FAILED: $desc", $expected, $actual);
        goto &checkeq_literal
      }
    }
  }
}

1;
