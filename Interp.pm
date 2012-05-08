use strict; use warnings; 
our $VERSION = sprintf "%d.%03d", q$Revision: 1.6 $ =~ /(\d+)/g; 

# Copyright © Jim Avera 2012.  Released into the Public Domain May 7, 2012.
# (james_avera AT ya (ho)o dot youknowwhat) 
# Please retain the preceeding attribution in any copies or derivitives.

# Documentation is at the end

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
  local $_;  # preserve $1 etc. in caller's context
  if (@_ == 0) {
    return "()";
  }
  elsif (@_ != 1) {
    $_ = vis(\@_);
    s/^\[/\(/ or die "bug($_)"; # convert to "(list,of,args)"
    s/\]$/\)/ or die "bug($_)";
    return $_;
  }

  my @lines = split /(?<=\n)/, 
                    Data::Dumper->new([@_])->
                      Quotekeys(0)->Sortkeys(1)->Terse(1)->Indent(1)->
                      Useqq(1)->Dump();
  $lines[$#lines] =~ s/\s+\z//s;  # omit final newline

  # Data::Dumper output is very structured, with a 2-space indent increment:
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
  
  # Combine appropriate lines to make it more "horizontal"
  my $restart = 0;
  while ($restart < $#lines) 
  {
    OUTER_LOOP:
    for (my $I=$restart, my $J=$restart+1, $restart=INT_MAX; 
         $J <= $#lines; 
         $I=$J, $J=$I+1) 
    {
      while ($lines[$I] =~/^\s*$/) { next OUTER_LOOP if ++$I >= $J }
      my ($Iindent,$Icode) = ($lines[$I] =~ /^(\s*)(.*\S)/);
      $Iindent = length($Iindent);

      while ($lines[$J] =~/^\s*$/) { last OUTER_LOOP if ++$J > $#lines }
      my ($Jindent,$Jcode) = ($lines[$J] =~ /^(\s*)(.*\S)/);
      $Jindent = length($Jindent);

      if ($Iindent <= $Jindent           # I & J at same level
          && $Icode !~ /^[\]\}]/ # I isn't closing an aggregate
          && $Jcode !~ /[\[\{]$/ # J isn't opening an aggregate
         )
      {
        my $Ilen = $Iindent+length($Icode);
        next 
          if $Ilen + 1 + length($Jcode) > $VisMaxWidth;
        substr($lines[$I],$Ilen,1) = " ";
        substr($lines[$I],$Ilen+1) = substr($lines[$J], $Jindent);
        $lines[$J] = "";
        $restart = $I if ($I < $restart);
        last if ++$J > $#lines;
        redo;
      }
    }
  }

  $_ = join "",@lines;
  s/\[ (.*) \]$/\[$1\]/;  # "[ 1, 2, 3 ]" -> "[1, 2, 3]"

  return $_
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
__END__

=head1 SYNOPSIS

  use Vis;

  my $struct = { complicated => ['lengthy','stuff',1..20] };
  print "struct=", vis($struct), "\n";

  foreach ($ENV{HOME}, "/dir/safe", "Uck!", 
           "My Documents", "Qu'ote", 'Qu"ote') 
  {
    system( "set -x; /bin/ls -ld ".quo($_) );
  }

=head1 DESCRIPTION

vis() is a wrapper for Data::Dumper which provides a simplified interface
and much more compact output, suitable for error/diagnostic messages:

=over

=item

There is no final newline.

=item

Argument(s) are all items to format.  If a single argument is passed,
then that object is simply stringified.  Otherwise the result has the
form of a list: "(arg1,arg2,arg3,...)", and no arguments produces "()".
This is nice for showing arrays, 
e.g.  C<print "Myfunc",vis(@_)," was called.\n";> 

=item

Array and hash members are shown all on the same line, subject to
a maximum line length given by $Vis::VisMaxWidth.

=back

$Vis::VisMaxWidth is not exported by default.

The above example produces the followint output:
  struct={
    complicated => [ "lengthy", "stuff", 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
    ]
  }

quo() and forcequo() 'single-quote' a string suitable for parsing by /bin/sh.

quo() returns the input unchanged if quoting is not necessary.

=cut

