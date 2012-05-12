use strict; use warnings; 
our $VERSION = sprintf "%d.%03d", q$Revision: 1.8 $ =~ /(\d+)/g; 

# Copyright © Jim Avera 2012.  Released into the Public Domain May 7, 2012.
# (james_avera AT ya (ho)o dot youknowwhat) 
# Please retain the preceeding attribution in any copies or derivitives.

# Documentation is at the end

package Vis;

use Exporter;
our @ISA       = qw(Exporter);
our @EXPORT    = qw(vis avis svis quo forcequo);
our @EXPORT_OK = ('$VisMaxWidth', '$VisUseqq');

our $VisMaxWidth = 72;
our $VisUseqq = 1;

use Carp;
use Data::Dumper ();
use POSIX qw(INT_MAX);

sub vis1($) {

  local $_ = Data::Dumper->new([shift])->
               Quotekeys(0)->Sortkeys(1)->Terse(1)->Indent(1)->
               Useqq($VisUseqq)->Dump();

  #print "===== RAW =====\n${_}---------------\n";

  # Split into logical lines, being careful to ignore newlines inside strings. 
  # The "delimiter" is the whole (logical) line, including final newline,
  # which is returned because it is in a (capture group).
  my @lines = (grep {defined} 
               ($VisUseqq 
                 ? split(/( (?: "(?: [^"]++ |\\" )*" | [^"\n]+)* \n )/xo, $_)
                 : split(/( (?: '(?: [^']++ |\\' )*' | [^'\n]+)* \n )/xo, $_)
               )
              );

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
      while ($lines[$I] =~/\A\s*$/s) { next OUTER_LOOP if ++$I >= $J }
      my ($Iindent,$Icode) = ($lines[$I] =~ /^(\s*)(.*\S)/s);
      $Iindent = length($Iindent);

      while ($lines[$J] =~/\A\s*$/s) { last OUTER_LOOP if ++$J > $#lines }
      my ($Jindent,$Jcode) = ($lines[$J] =~ /\A(\s*)(.*\S)/s);
      $Jindent = length($Jindent);

#      print "===== I=$I Iind=$Iindent, J=$J Jind=$Jindent =====\n";
#      for my $ix(0..$#lines) {
#        next if $lines[$ix] eq "" && $ix != $I && $ix != $J;
#        printf "[%2d] ", $ix;
#        print($ix==$I ? "I" : " ");
#        print($ix==$J ? "J" : " ");
#        print ":", ($lines[$ix] eq "" ? "(empty)" : "«$lines[$ix]»"), "\n";
#      }
#      print "--------------------\n";

      if ($Iindent <= $Jindent
          && $Icode !~ /^[\]\}]/s # I isn't closing an aggregate
          && $Jcode !~ /[\[\{]$/s # J isn't opening an aggregate
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
sub vis(@) {
  local $_;  # preserve $1 etc. in caller's context
  return join "\n", map { vis1($_) } @_;
}
sub avis(@) {
  local $_ = vis(\@_);
  s/^\[/\(/ or die "bug($_)"; # convert to "(list,of,args)"
  s/\]$/\)/ or die "bug($_)";
  return $_;
}
sub svis { 
  goto &DB::_svis;
}


sub forcequo($) {
  local $_ = shift;
  croak "quo:value is undef" unless defined $_;
  s/'/'\\''/g;
  return "'${_}'";
}

sub quo(;$) {
  local $_ = (@_==0 ? $_ : $_[0]);
  return ((/[^-\w_\/:\.]/ || $_ eq "") ? forcequo($_) : $_);
}

sub svis { goto &DB::_svis }

package DB; # Makes 'eval' see caller's context

sub _svis(@) {
  local $_;
  join("", map{
    local $_ = $_;
    s/(?<!\\)\$/\{ VisDollar \}/xgs;
    # Interpolate \n, unicode escapes, etc.
    $_ = eval qq(<<"V i s E O F"
${_}
V i s E O F
);
    s/\{ VisDollar \}/\$/g;
    s/(?<!\\)  (?| \$(\w+)\b | \$\{(\w+)\} )/
              do{
                my $varname = $1;
                my $value = eval "\$$varname";
                $value;
              }/xegs;
    $_;
  } @_);
}

1;
__END__

=head1 SYNOPSIS

  use Vis;

  my $struct = { complicated => ['lengthy','stuff',1..20] };
  print "struct=", vis($struct), "\n";

  print "My args are ", avis(@_), "\n";

  foreach ($ENV{HOME}, "/dir/safe", "Uck!", 
           "My Documents", "Qu'ote", 'Qu"ote') 
  {
    system( "set -x; /bin/ls -ld ".quo($_) );
  }

=head1 DESCRIPTION

=head2 vis($item ...)

Returns a printable representation of arbitrary Perl data structure(s).

vis() is a wrapper for Data::Dumper which provide
a simplified interface and much more compact output, 
suitable for error/diagnostic messages:

=over

=item

There is no final newline.

=item

Multiple array and hash members are shown on the same line, subject to
a maximum line length given by $Vis::VisMaxWidth
$Vis::VisMaxWidth is not exported by default.

The example shown above produces the following output:

  struct={
    complicated => [ "lengthy", "stuff", 1, 2, 3, 4, 5, 6, 7,
      8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
    ]
  }

=item

Although vis() is usually is called with a single argument, multiple 
args are allowed and are formatted separately, appearing on separate lines
(there is still no final newline).

=back

=head2 avis(@array)

avis() formats an array or list in parenthesis: C<(arg1,arg2,arg3,...)> .
Zero arguments produces C<()>.   
This allows @arrays to be shown without taking a reference.


=head2 quo($string) 

The string is 'single-quoted' if necessary for parsing by /bin/sh, otherwise
the string is returned unchanged.

=head2 forcequo($string)

The string is 'single-quoted' for /bin/sh if even if not necessary.

=cut

