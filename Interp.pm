use strict; use warnings; 

# Copyright © Jim Avera 2012.  Released into the Public Domain May 7, 2012.
# (james_avera AT ya (ho)o dot youknowwhat) 
# Please retain the preceeding attribution in any copies or derivitives.

our $VERSION = sprintf "%d.%03d", q$Revision: 1.3 $ =~ /(\d+)/g; 

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

sub u($) { defined $_[0] ? $_[0] : "undef" }
sub vis(@);
sub vis(@) { 
  if (@_ > 1) {
    return "(" . (join ", ", map { vis($_) } @_) . ")";
  }
  local $_ = Data::Dumper->new([@_])->
              Quotekeys(0)->Sortkeys(1)->Terse(1)->Indent(1)->
              Useqq(1)->Dump();

  s/\s+\z//s;       # omit final newline

  # Make the output more horizontal
  use feature 'state';
  state $item_re = qr/  (?:
                            [^",\\\[\]\{\}]*[^",\\\[\]\{\}\s]    
                            | \\.
                            | "(?:[^"\\]+|\\.)*" 
                        )+
                      /xs; 

   #warn "### ORIGINAL:\n$_\n----\n";
   s/^(\ *) 
     (
       (?|  # reuse same group numbers in each alternation
         (\[) \s* ( (?:\s*${item_re},)* (?:\s*${item_re})? ) \s* (\]) \s*
         |
         (\{) \s* ( (?:\s*${item_re},)* (?:\s*${item_re})? ) \s* (\}) \s*
       )
     )
    /
     do{ my ($indent, $lb, $content, $rb) = ($1,$3,$4,$5);
         #warn "### lb=",u($lb)," content=",u($content)," rb=",u($rb),"\n";
         local $_;
         my $new = "${indent}${lb}";
         my $llen = length($indent)+1;
         my $comma;
         foreach my $piece (split m#\s*(${item_re})\s*#,$content) {
           #warn "### piece=$piece\n";
           if ($piece =~ m#^,?$#) { #delimiter or initial empty field
             $comma = $piece;
             next;
           }
           $new .= $comma; $llen += length($comma); 
           if ($llen + 1 + length($piece) + 1 > $VisMaxWidth) {
             $new .= "\n${indent} ";
             $llen = length($indent)+1;
           }
           $new .= " $piece";
           $llen += (1 + length($piece));
         }
         $new .= " ${rb}";
         $new;
     }/xemsg;
  $_;
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
