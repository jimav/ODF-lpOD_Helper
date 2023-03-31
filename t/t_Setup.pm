# License: Public Domain or CC0
# See https://creativecommons.org/publicdomain/zero/1.0/
# The author, Jim Avera (jim.avera at gmail) has waived all copyright and 
# related or neighboring rights to the content of this file.  
# Attribution is requested but is not required.

use strict; use warnings  FATAL => 'all'; use feature qw/say/;
use v5.16; # must have PerlIO for in-memory files for 'silent';

package t_Setup;

require Exporter;
use parent 'Exporter';
our @EXPORT = qw/silent/;

# Do an initial read of $[ so arybase will be autoloaded
# (prevents corrupting $!/ERRNO in subsequent tests)
eval '$[' // die;

# N.B. It appears, experimentally, that output from ok(), like() and friends
# is not written to the test process's STDOUT or STDERR, so we do not need
# to worry about ignoring those normal outputs (somehow everything is
# merged at the right spots, presumably by a supervisory process).
#
# Therefore tests can be simply wrapped in silent{...} or the entire
# program via the ':silent' tag; however any "Silence expected..." diagnostics
# will appear at the end, perhaps long after the specific test case which
# emitted the undesired output.
my ($orig_stdOUT, $orig_stdERR);
my ($inmem_stdOUT, $inmem_stdERR) = ("", "");
my $silent_mode;
use Encode qw/decode FB_WARN FB_PERLQQ FB_CROAK LEAVE_SRC/;
use Carp;
sub _start_silent() {
  confess "nested silent treatments not supported" if $silent_mode;
  $silent_mode = 1;

  my @OUT_layers = grep{ $_ ne "unix" } PerlIO::get_layers(*STDOUT, output=>1);
  open($orig_stdOUT, ">&", \*STDOUT) or die "dup STDOUT: $!";
  close STDOUT;
  open(STDOUT, ">", \$inmem_stdOUT) or die "redir STDOUT: $!";
  binmode(STDOUT); binmode(STDOUT, ":utf8");

  my @ERR_layers = grep{ $_ ne "unix" } PerlIO::get_layers(*STDERR, output=>1);
  open($orig_stdERR, ">&", \*STDERR) or die "dup STDERR: $!";
  close STDERR;
  open(STDERR, ">", \$inmem_stdERR) or die "redir STDERR: $!";
  binmode(STDERR); binmode(STDERR, ":utf8");
}
sub _finish_silent() {
  confess "not in silent mode" unless $silent_mode;
  close STDERR;
  open(STDERR, ">>&", $orig_stdERR) or exit(198);
  close STDOUT;
  open(STDOUT, ">>&", $orig_stdOUT) or die "orig_stdOUT: $!";
  $silent_mode = 0;
  # The in-memory files hold octets; decode them before printing
  # them out (when they will be re-encoded for the user's terminal).
  my $errmsg;
  if ($inmem_stdOUT ne "") {
    print STDOUT "--- saved STDOUT ---\n";
    print STDOUT decode("utf8", $inmem_stdOUT, FB_PERLQQ|LEAVE_SRC);
    $errmsg //= "Silence expected on STDOUT";
  }
  if ($inmem_stdERR ne "") {
    print STDERR "--- saved STDERR ---\n";
    print STDERR decode("utf8", $inmem_stdERR, FB_PERLQQ|LEAVE_SRC);
    $errmsg = $errmsg ? "$errmsg and STDERR" : "Silence expected on STDERR";
  }
  $errmsg
}
sub silent(&) {
  my $wantarray = wantarray;
  my $code = shift;
  _start_silent();
  my @result = do{
    if (defined $wantarray) {
      return( $wantarray ? $code->() : scalar($code->()) );
    }
    $code->();
    my $dummy_result; # so previous call has null context
  };
  my $errmsg = _finish_silent();
  local $Test::Builder::Level = $Test::Builder::Level + 1;
  Test::More::ok(! defined($errmsg), $errmsg);
  wantarray ? @result : $result[0]
}

sub import {
  my $target = caller;
  use Import::Into;

  strict->import::into($target);
  warnings->import::into($target, FATAL => 'all');

  #use 5.010;  # say, state
  use 5.011;  # cpantester gets warning that 5.11 is the minimum acceptable
  use 5.018;  # lexical_subs
  require feature;
  feature->import::into($target, qw/state say current_sub lexical_subs/);
  warnings->unimport::out_of($target, "experimental::lexical_subs");

  # Unicode support
  state $initialized;
  unless ($initialized++) {
    # This Must be done before loading Test::More
    confess "Test::More already loaded!" if defined( &Test::More::ok );
    use open ':std', ':encoding(UTF-8)';
    "open"->import::into($target, ':std', ':encoding(UTF-8)');
  
    # Disable buffering
    STDERR->autoflush(1);
    STDOUT->autoflush(1);
  }
  require utf8;
  utf8->import::into($target);

  # die if obsolete or dangerous syntax is used
  require indirect;
  indirect->unimport::out_of($target);

  require multidimensional;
  multidimensional->unimport::out_of($target);

  require autovivification;
  autovivification->unimport::out_of($target,
                warn => qw/fetch store exists delete/);

  require Test::More; 
  Test::More->VERSION('0.98'); # see UNIVERSAL
  Test::More->import::into($target);

  # import things I always or often use into the test case
  require Carp;
  Carp->import::into($target);

  require File::Basename;
  File::Basename->import::into($target, qw/basename dirname/);

  require List::Util;
  List::Util->import::into($target, qw/reduce min max first/);

  require File::Spec;
 
  require Scalar::Util;
  Scalar::Util->import::into($target, qw/blessed reftype looks_like_number
                                         weaken isweak refaddr/
                            );

  require Cwd;
  Cwd->import::into($target, qw/getcwd abs_path/);

  # Avoid regex performance penalty in Perl <= 5.18
  # if $PREMATCH $MATCH or $POSTMATCH are imported
  # (fixed in perl 5.20).
  require English;
  English->import::into($target, '-no_match_vars' ); 

  unless (Cwd::abs_path(__FILE__) =~ /Data-Dumper-Interp/) {
    # unless we are being used to test Data::Dumper::Interp ...
    require Data::Dumper::Interp;
    Data::Dumper::Interp->import::into($target);
    $Data::Dumper::Interp::Useqq = 'unicode'; # omit 'controlpic' to get \t etc.
  }

  if (grep{ $_ eq ':silent' } @_) {
    @_ = grep{ $_ ne ':silent' } @_;
    _start_silent();
  }

  # chain to Exporter to export any other importable items
  goto &Exporter::import
}

END{
  if ($silent_mode) {
    my $errmsg = _finish_silent();
    die $errmsg if $errmsg;
  }
}

1;
