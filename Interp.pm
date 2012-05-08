use strict; use warnings; 

# Copyright © Jim Avera 2012.  Released into the Public Domain May 7, 2012.
# (james_avera AT ya (ho)o dot youknowwhat) 
# Please retain the preceeding attribution in any copies or derivitives.

our $VERSION = sprintf "%d.%03d", q$Revision: 1.4 $ =~ /(\d+)/g; 

=head1 SYNOPSIS

  use Vis;

  my $struct = { complicated => [stuff] };
  print "struct:", vis($struct), "\n";

  foreach ($ENV{HOME}, "/dir/safe", "Uck!", "My Folder", "Qu'ote", 'Qu"ote') {
    system( "set -x; /bin/ls -ld ".quo($_) );
  }

=head1 DESCRIPTION

vis() is a wrapper for Data::Dumper which provides a simplified interface
and more compact output, suitable for error/diagnostic messages.

  The argument(s) are simply items to format.  If more than one argument
  is passed, the result looks like a list: (arg1,arg2,arg3,...)

  List and hash members are shown all on the same line, subject to
  a maximum line length given by $Vis::VisMaxWidth.

  There is no final newline.

$Vis::VisMaxWidth is not exported by default.


quo() and forcequo() 'single-quote' a string suitable for parsing by /bin/sh.
quo() returns the input unchanged if quoting is not necessary.

=cut

package Vis;

use Exporter;
our @ISA       = qw(Exporter);
our @EXPORT    = qw(vis quo forcequo);
our @EXPORT_OK = ('$VisMaxWidth');
our $VisMaxWidth = 72;

use Carp;
use Data::Dumper ();
use POSIX qw(INT_MAX);

sub u($) { defined $_[0] ? $_[0] : "undef" }
sub vis(@);
sub vis(@) {
  if (@_ > 1) {
    return "(" . (join ", ", map { vis($_) } @_) . ")";
  }
  my @lines = split /(?<=\n)/, 
                    Data::Dumper->new([@_])->
                      Quotekeys(0)->Sortkeys(1)->Terse(1)->Indent(1)->
                      Useqq(1)->Dump();

  $lines[$#lines] =~ s/\s+\z//s;  # omit final newline

  #print "---BEFORE---\n"; foreach(@lines) { print; } print "\n-----------\n";
  # Data::Dumper output looks like this, with an indent increment of 2 spaces:
  # [
  #   value,
  #   value,
  #   {
  #     key => [
  #       value,
  #       value
  #     ],
  #     key => {
  #       key => value,
  #       key => value
  #     },
  #   },
  #   value
  # ]
  my $restart = 0;
  while ($restart < $#lines) 
  {
    OUTER_LOOP:
    for (my $I=$restart, my $J=$restart+1, $restart=INT_MAX; 
         $J < $#lines; 
         $I=$J, $J=$I+1) 
    {
      while ($lines[$I] =~/^\s*$/) { next OUTER_LOOP if ++$I >= $J }
      my ($Iind,$Icode) = ($lines[$I] =~ /^(\s*)(.*\S)/);
      $Iind = length($Iind);

      #die "bug" if grep {$_ ne ""} @lines[$I+1..$J-1]; ### TEMP

      while ($lines[$J] =~/^\s*$/) { last OUTER_LOOP if ++$J > $#lines }
      my ($Jind,$Jcode) = ($lines[$J] =~ /^(\s*)(.*\S)/);
      $Jind = length($Jind);

      my $do_join;
      if ($Iind <= $Jind           # I & J at same level
          && $Icode !~ /^[\]\}]/   # I isn't closing an aggregate
          && $Jcode !~ /[\[\{]$/   # J isn't opening an aggregate
         ) {
        $do_join = 1;
      }
      if ($do_join) {
        my $Ilen = $Iind+length($Icode);
        if ($Ilen + $Jind + length($Jcode)+1 <= $VisMaxWidth) {
          substr($lines[$I],$Ilen,1) = " ";
          substr($lines[$I],$Ilen+1) = substr($lines[$J], $Jind);
          $lines[$J] = "";
          $restart = $I if ($I < $restart);
          last if ++$J > $#lines;
          redo;
        } else {
          next;  # won't fit
        }
      }
    }
  }

  return join "",@lines;
}

sub forcequo($) {
  my $str = shift;
  confess "quo:value is undef" unless defined $str;
  $str =~ s/'/'\\''/g;
  return "'${str}'";
}

sub quo(;$) {
  my $str = (@_==0 ? $_ : $_[0]);
  return (($str =~ /[^-\w_\/:\.]/ || $str eq "") ? forcequo($str) : $str);
}

1;
