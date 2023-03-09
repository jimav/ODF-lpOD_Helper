use strict; use warnings  FATAL => 'all'; use feature qw/say/;
use v5.16; # must have PerlIO for in-memory strings, see ':silent' below

package t_Setup;

require Exporter;
use parent 'Exporter';
our @EXPORT = qw/bug silent/;

# N.B. It appears, experimentally, that output from ok(), like() and friends 
# is not written to the test process's STDOUT or STDERR, so we do not need
# to worry about ignoring those normal outputs.
# Somehow the test infrastructure output gets merged into the final STDOUT/ERR
# streams at the right points.
#
# The upshot is that an entire set of tests can be wrapped in a single silent {...}
# however the "Silence expected..." diagnostics will occur at the end of the block,
# possibly long after the specific test which caused the undesired output.
my ($orig_stdOUT, $orig_stdERR);
my ($inmem_stdOUT, $inmem_stdERR) = ("", "");
my $silent_mode;
use Encode qw/decode FB_CROAK LEAVE_SRC/;
use Carp;
sub _start_silent() {
  confess "nested silent treatments not supported" if $silent_mode;
  open($orig_stdOUT, ">&", \*STDOUT) or die "dup STDOUT: $!";
  close STDOUT;
  open(STDOUT, ">", \$inmem_stdOUT) or die "redir STDOUT: $!";
  open($orig_stdERR, ">&", \*STDERR) or die "dup STDERR: $!";
  close STDERR;
  open(STDERR, ">", \$inmem_stdERR) or die "redir STDERR: $!";
  $silent_mode = 1;
}
sub _finish_silent() {
  confess "not in silent mode" unless $silent_mode;
  close STDOUT;
  open(STDOUT, ">>&", $orig_stdOUT) or do{ warn "return to STDOUT: $!"; exit 198; };
  close STDERR;
  open(STDERR, ">>&", $orig_stdERR) or do{ warn "return to STDERR: $!"; exit 198; };
  $silent_mode = 0;
  # The in-memory files are stored as octets, so decode before writing to
  # the STD* files (which will re-encode).
  print STDOUT decode("utf8", $inmem_stdOUT, FB_CROAK|LEAVE_SRC);
  print STDERR decode("utf8", $inmem_stdERR, FB_CROAK|LEAVE_SRC);
  # Can we call Test::More::ok(0, "..."); here?
  # Just dying produces "END failed--call queue aborted."
  die "Silence was expected on STDOUT\n" if $inmem_stdOUT ne "";
  die "Silence was expected on STDERR\n" if $inmem_stdERR ne "";
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
  _silent_finish();
  wantarray ? @result : $result[0]
}

sub import {
  my $target = caller;
  use Import::Into;

  strict->import::into($target);
  warnings->import::into($target, FATAL => 'all');
  feature->import::into($target, qw/say state/);
  
  # Unicode support
  # Must be done before loading Test::More
  confess "too late" if defined( &Test::More::ok );
  use open ':std', ':encoding(UTF-8)';
  "open"->import::into($target, ':std', ':encoding(UTF-8)');
  use utf8;
  utf8->import::into($target);

  # Disable buffering
  STDERR->autoflush(1);
  STDOUT->autoflush(1);

  # die if obsolete or dangerous syntax is used
  require indirect; 
  indirect->unimport::out_of($target); 

  require multidimensional;
  multidimensional->unimport::out_of($target);

  require autovivification;
  autovivification->unimport::out_of($target);
  
  require Carp; 
  Carp->import::into($target);

  require Test::More; Test::More->VERSION('0.98'); # see UNIVERSAL
  Test::More->import::into($target);

  if (grep{ $_ eq ':silent' } @_) {
    @_ = grep{ $_ ne ':silent' } @_;
    Carp::confess("multiple uses?") if $silent_mode;
    _start_silent();
  }

  # chain to Exporter to export our local definitions
  goto &Exporter::import
}

END{
  _finish_silent() if $silent_mode;
}

1;

use Capture::Tiny qw/capture/;
use Carp;

sub bug(@) { @_=("BUG:",@_); goto &Carp::confess }

our $num_silence_violations = 0;

END {
  die "$num_silence_violations silence violations occurred\n"
    if $num_silence_violations;
}

1;
