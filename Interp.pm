package Jima::Vis;
use strict; use warnings;
our $VERSION = sprintf "%d.%03d", q$Id: Interp.pm,v 1.1 2012/05/07 04:00:29 jima Exp $ =~ /(\d+)/g;

=head1 NAME

Jima::Vis -- stringify arbitrary Perl data structures for display.

Note: These are similar to Data::Dumper::Dumper, but with better 
formatting for human consumption.  This does not handle self-referential
data structures etc. like Data::Dumper does.

TODO: Maybe use Data::Dumper internally and filter it's output to collapse
unnecessary multi-line composites, etc. (don't re-invent the wheel).

  print vis($foo, \%hash), "\n";
  printf "mv %s %s\n", quo($path1), quo($path2);

=cut

our @ISA       = qw(Exporter);
our @EXPORT    = qw(vis quo forcequo);
our @EXPORT_OK = qw();

use Carp;

#------------------------Begin "vis" Library---------------------
sub vis(@);  # stringify any perl expression
sub quo(@);  # stringify any perl expression, 'quoting' if needed for /bin/sh

sub VIS_MAXLEN() { 72 }
sub forcequo($) {
  my $str = shift;
  confess "quo:undef" unless defined $str;
  $str =~ s/'/'\\''/g;
  return "'${str}'";
}
sub quo(;$) {
  my $str = (@_==0 ? $_ : $_[0]);
  return (($str =~ /[^-\w_\/:\.]/ || $str eq "") ? forcequo($str) : $str);
}
sub _vis_append_last($@);
sub _vis_append_last($@) { # append string(s) to the last scalar
  my $top = shift;
  if (ref $top->[$#$top]) {
    my $last = $top->[$#$top];
    my $prevlen = $last->[0];
    _vis_append_last($last, @_);
    $top->[0] += ($last->[0] - $prevlen);
  } else {
    foreach (@_) {
      $top->[0] += length($_);
      $top->[$#$top] .= $_;
    }
  }
}
sub _vis_list($$$) {
  my ($left, $elems, $right) = @_;
  my $result = [length($left)+length($right), $left];
  for my $ix (0..$#$elems) {
    my $item = _vis($elems->[$ix]);
    if (ref($item)) {
      $result->[0] += $item->[0];
    } else {
      $result->[0] += length($item);
    }
    push @$result, $item;
    _vis_append_last($result, ref($item) ? ", ":",") unless $ix==$#$elems;
  }
  push @$result, $right;
  return $result;
}
sub _vis(@);
sub _vis(@) { # returns scalar or [totlen,items...]
  if (@_ > 1) {
    return _vis_list("(", \@_, ")");
  } else {
    my $arg = shift;
    if (ref($arg) eq 'ARRAY') {
      return _vis_list("[", $arg, "]");
    }
    elsif (ref($arg) eq 'HASH') {
      my $result = [2,"{"];
      my @keys = sort keys %$arg;
      for my $ix (0..$#keys) {
        my $key = $keys[$ix];
        my $qkey = ($key =~ /^[A-Za-z]\w*$/ ? $key : forcequo($key));
        $result->[0] += length($qkey)+2;
        push @$result, "${qkey}=>";
        my $value = _vis($arg->{$key});
        if (ref($value)) {
          $result->[0] += $value->[0];
        } else {
          $result->[0] += length($value);
        }
        push @$result, $value;
        _vis_append_last($result, ", ") unless $ix==$#keys;
      }
      push @$result, "}";
      return $result;
    }
    else {
      return defined($arg) ? quo($arg) : "<undef>";
    }
  }
}
my ($_vis_buf, $_vis_linelen);
my $_vis_indent = 0;
sub _vis_expand($);
sub _vis_expand($) {
  my $top = shift;
  if (! ref $top) {
    $top = [ length($top), $top ];
  }
  if ($_vis_linelen+$top->[0] > VIS_MAXLEN && $_vis_linelen > $_vis_indent) {
    # warn "### break--group\n";
    $_vis_buf .= "\n" . (" " x $_vis_indent);
    $_vis_linelen = $_vis_indent;
  }
  my $indent_delta = 0;
  foreach my $ix (1..$#$top) {
    my $item = $top->[$ix];
    if (ref($item)) {
      $_vis_indent += 2;
      _vis_expand($item);
      $_vis_indent -= 2;
    } else {
      if ($_vis_linelen+length($item) > VIS_MAXLEN 
                                          && $_vis_linelen > $_vis_indent) {
        # warn "### break--scalar\n";
        $_vis_buf .= "\n" . (" " x $_vis_indent);
        $_vis_linelen = $_vis_indent;
      }
      $_vis_buf .= $item;
      $_vis_linelen += length($item);
      if ($ix == 1) {
        $indent_delta = length($item); # e.g. 1 for "[" or "{"
        $_vis_indent += $indent_delta;
      }
    }
    if ($ix == $#$top) { $_vis_indent -= $indent_delta; }
  }
}
sub vis(@) {
  $_vis_buf = "";
  $_vis_linelen = 0;
  my $r = &_vis; # tree of printable strings
  _vis_expand($r);
  confess "Unbalance bug" unless $_vis_indent==0;
  return $_vis_buf;
}
#die "\n\n\n\nTEST CASE:\n",
#"1234567891         2         3         4         51234567896123456789712\n", 
#vis([], 33,"fourty four", [0..10, {}, [],
#     ["a".."z"],
#     { "key one" => "val1", "k2" => "v 2222222222222222222222222222",
#       key3 => "val 33333333333333333333"},
#    ["A".."CZ"],
#    sub { 123 },
#    11..50
#    ]),"\n";
#------------------------End "vis" Library---------------------

1;
