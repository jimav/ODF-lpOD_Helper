use strict; use warnings; 
our $VERSION = sprintf "%d.%03d", q$Revision: 1.11 $ =~ /(\d+)/g; 

# Copyright © Jim Avera 2012.  Released into the Public Domain 
# by the copyright owner.  (james_avera AT yahoo đøţ ¢ÔḾ) 
# Please retain the preceeding attribution in any copies or derivitives.

# Documentation is at the end

package Vis;

use Exporter;
use Data::Dumper ();
use Carp;
use POSIX qw(INT_MAX);

our @ISA       = qw(Exporter Data::Dumper);
our @EXPORT    = qw(vis avis svis dvis Dumper quo forcequo);
our @EXPORT_OK = ('$VisMaxwidth', '$VisUseqq');

# Used by non-oo functions, and initial settings for oo constructors
our ($VisMaxwidth, $VisUseqq);
$VisMaxwidth = 72  unless defined $VisMaxwidth;
$VisUseqq    = 1   unless defined $VisUseqq;

# Functional (non-oo) APIs
sub vis(@)    { return __PACKAGE__->vnew(@_)->Dump; }
sub avis(@)   { return __PACKAGE__->anew(@_)->Dump; }
sub svis(@)   { @_ = (__PACKAGE__->snew(@_)); goto &DB::Vis_DB_Dump }
sub dvis(@)   { @_ = (__PACKAGE__->dnew(@_)); goto &DB::Vis_DB_Dump }

# Provide Data::Dumper non-oo APIs 
sub Dumper(@) { return __PACKAGE__->Dump([@_]); }
sub Dumpf(@)  { return __PACKAGE__->Dump([@_]); }
sub Dumpp(@)  { print __PACKAGE__->Dump([@_]); }

# Note: All Data::Dumper methods can be called on Vis objects

sub _config_defaults {
  my $self = shift;
  $self->Quotekeys(0)->Sortkeys(1)->Terse(1)->Indent(1)->
           Useqq($VisUseqq)->Maxwidth($VisMaxwidth)
}

# vnew(items...)                
# anew(items...)              
# snew(strings...)             
# dnew(strings...)             
# new([items...],[names...])  --with trailing \n like Data::Dumper::new 
sub new {
  my $class = shift;
  bless($class->SUPER::new(@_), $class)->_config_defaults()->Terse(0);
}
sub vnew {
  my $class = shift;
  my $obj = (bless($class->SUPER::new(\@_), $class))->_config_defaults();
  $obj->{VisType} = 'v';
  $obj;
}
sub anew {
  my $class = shift;
  my $obj = (bless($class->SUPER::new([\@_]), $class))->_config_defaults();
  $obj->{VisType} = 'a';
  $obj;
}
sub snew {
  my $class = shift;
  # The string we pass here to the Dumper constructor (which, by the way,
  # the user can get or replace via the inherited Value method) will not 
  # actually be formatted as-is.  Instead, our overload Dump() method
  # will parse the string and then re-use the dumper object to format 
  # each interpolated $varname etc.  separately, thereby using any 
  # configurations (Useqq etc.) set by the user.
  #
  # Useqq(0) by default so that newline appear as such, rather than '\n'
  # so   «print svis 'The answer is $answer\n';»   just works.
  my $obj = (bless($class->SUPER::new([@_]), $class))->_config_defaults()->Useqq(0);
  $obj->{VisType} = 's';
  $obj;
}
sub dnew {
  my $obj = snew(@_);
  $obj->{VisType} = 'd';
  $obj;
}

sub Debug {
  my($s, $v) = @_;
  defined($v) ? (($s->{VisDebug} = $v), return $s) : $s->{VisDebug};
}
sub Maxwidth {
  my($s, $v) = @_;
  defined($v) ? (($s->{VisMaxwidth} = $v), return $s) : $s->{VisMaxwidth};
}

sub Dump {
  my ($self) = @_;

  $self = $self->new(@_) unless ref $self;

  if (($self->{VisType}//"") =~ /[sd]/) {
    goto &DB::Vis_DB_Dump;
  }

  my $debug = $self->{VisDebug};
  local $_ = $self->SUPER::Dump;

  print "===== RAW =====\n${_}---------------\n" if $debug;

  # Split into logical lines, being careful to ignore newlines inside strings. 
  # The "delimiter" is the whole (logical) line, including final newline,
  # which is returned because it is in a (capture group).
  my @lines = (grep {defined} 
               ($VisUseqq 
                 ? split(/( (?: "(?: [^"]++ |\\" )*" | [^"\n]+)* \n )/xo, $_)
                 : split(/( (?: '(?: [^']++ |\\' )*' | [^'\n]+)* \n )/xo, $_)
               )
              );

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

      if ($debug) {
        print "===== I=$I Iind=$Iindent, J=$J Jind=$Jindent =====\n";
        for my $ix(0..$#lines) {
          next if $lines[$ix] eq "" && $ix != $I && $ix != $J;
          printf "[%2d] ", $ix;
          print($ix==$I ? "I" : " ");
          print($ix==$J ? "J" : " ");
          print ":", ($lines[$ix] eq "" ? "(empty)" : "«$lines[$ix]»"), "\n";
        }
        print "--------------------\n";
      }

      if ($Iindent <= $Jindent
          && $Icode !~ /^[\]\}]/s # I isn't closing an aggregate
          && $Jcode !~ /[\[\{]$/s # J isn't opening an aggregate
          && ($Icode =~ /(?:,|[\[\{])$/ || $Jcode =~ /^[\]\}]/)
         )
      {
        my $Ilen = $Iindent+length($Icode);
        next 
          if $Ilen + 1 + length($Jcode) > $self->{VisMaxwidth};
        substr($lines[$I],$Ilen,1) = " ";
        substr($lines[$I],$Ilen+1) = substr($lines[$J], $Jindent);
        $lines[$J] = "";
        $restart = $I if ($I < $restart);
        last if ++$J > $#lines;
        redo;
      } else {
        print "NOT JOINED.\n" if $debug;
      }
    }
  }

  foreach (@lines) {
    s/\[ (.*) \]$/\[$1\]/;  # "[ 1, 2, 3 ]" -> "[1, 2, 3]"
    s/\{ (.*) \}$/\{$1\}/;  # "{ key => value }" -> "{key => value}"
  }

  $_ = join "", @lines;

  if (($self->{VisType}//"") eq 'a') {
    s/^\[/\(/ or confess "bug($_)"; # convert to "(list,of,args)"
    s/\]$/\)/ or confess "bug($_)";
  }
  if (exists $self->{VisType}) {
    s/\s+\z//s;  # omit final newline except when emulating Data::Dumper
  }

  return $_;
}

sub forcequo($) {
  local $_ = vnew(shift)->Useqq(0)->Dump;
  $_ = vnew($_)->Useqq(0)->Dump unless /^'/;  # re-quote [aggregate]
  s/\\'/'\\''/g;  # 'foo\'bar'  =>  'foo'\''bar' for /bin/sh
  return $_;
}

sub quo(;$) {
  local $_ = (@_==0 ? $_ : $_[0]);
  return ((/[^-\w_\/:\.]/ || $_ eq "") ? forcequo($_) : $_);
}

package DB;

# Implement Dump() method for svis and dvis styles
# This must be in package DB so that eval "" uses the caller's context
sub Vis_DB_Dump {
  my ($self) = @_;

  use feature 'state';
  state $qstr_re = qr{" ( [^\"\\]+ | \\. ) " |' ( [^\'\\]+ | \\. ) ' }x;
  state $expr_re = qr{ 
                      (
                          [^\{\}\[\]\(\)\'\"]+  # backslash okay
                        | \{ (?-1) \}
                        | \[ (?-1) \]
                        | \( (?-1) \)
                        | $qstr_re
                      )*
                     }x;
  
  # This allows @_ to be interpolated
  # See https://rt.perl.org/rt3//Public/Bug/Display.html?id=112896
  () = caller 1;
  local *_ = \@DB::args;

  my $debug = $self->{VisDebug};
  my $display_mode = $self->{VisType} eq 'd';

  local $_ = join "", $self->Values(); 

  my @parts;
  while (1) {
    if (
        # Sigh.  \G does not work with (?|...) in Perl 5.12.4
        # https://rt.perl.org/rt3//Public/Bug/Display.html?id=112894

        # $? and other 'punctuation variables'
        /\G (?!\\)([\$\@])( [^\s\{\w] )/xsgc 
        ||
        # $name $name[expr] $name->{expr} etc.
        /\G (?!\\)([\$\@])( \w+ (?:->)? 
            (?: \[ $expr_re \] | \{ $expr_re \} )? )/xsgc
        ||
        # ${?} etc. (loosing the curlies)
        /\G (?!\\)([\$\@]) \{ ( [^\s\{\w] ) \}/xsgc 
        ||
        # ${name} (loosing the curlies)
        /\G (?!\\)([\$\@]) \{ ( \w+ ) \}/xsgc 
        ||
        # ${refexpr} or ${^SPECIALVARNAME}
        /\G (?!\\)([\$\@]) ( \{ $expr_re \} )/xsgc 
       )
    {
      my ($sigl, $rhs) = ($1,$2);
      # $sigl$rhs is a Perl expression giving the desired value.  
      # Note that the curlies were dropped from ${name} in the string
      # (because braces can not be used that way in expressions, only strings)
      print "### EVAL $sigl$rhs\n" if $debug;
      my @items = eval "$sigl$rhs";
      Carp::confess "($sigl$rhs)$@" if $@ && $debug;
      Carp::croak($@) if $@ =~ s/ at \(eval.*//;
      push @parts, "$sigl$rhs=" if $display_mode;
      if ($sigl eq '$') {
        $self->Reset()->Values([$items[0]])->{VisType} = 'v';
        push @parts, $self->Dump;
      } else {
        $self->Reset()->Values([\@items])->{VisType} = 'a';
        push @parts, $self->Dump;
      }
    }
    elsif (/\G ( (?: [^\$\@\\]+ | \\. )* ) /xsgc)  # plain text
    {
      # Interpolate \n etc.
      print "### PLAIN $1\n" if $debug;
      chomp (my $value = eval qq{<<" V i s EOF"
${1}
 V i s EOF
});
      Carp::confess "Unexpected eval faulre ($1): $@" if $@;
      push @parts, $value;  # plain text
    }
    else {
      die "bug pos=",pos," in:\n$_\n".(" "x pos)."^\n" if /\G./;
      last;
    }
  }
  return join "",@parts;
}

1;
__END__

=head1 NAME 

Vis - Format arbitrary Perl data structures for printing

=head1 SYNOPSIS

  use Vis;

  my $struct = { complicated => ['lengthy','stuff',1..20] };

  print "struct=", vis($struct), "\n";
  print "ARGV=", avis(@ARGV), "\n";
  print svis 'struct=$struct\nARGV=@ARGV\n'; # SINGLE quoted!
  print dvis 'Display of variables: $struct @ARGV\n'; 

  print "struct=", Vis->vnew($struct)->Useqq(0)->Dump, "\n";
  print "ARGV is ", Vis->anew(@ARGV)->Useqq(0)->Dump, "\n";
  print Vis->snew('struct=$struct\nARGV=@ARGV\n')->Useqq(1)->Dump;
  print Vis->dnew('$struct\n@ARGV\n')->Useqq(1)->Dump;

  foreach ($ENV{HOME}, "/dir/safe", "Uck!", 
           "My Documents", "Qu'ote", 'Qu"ote') 
  {
    system( "set -x; /bin/ls -ld ".quo($_) );
  }

  print Vis->new([items],[names])->Dump;  # same API as Data::Dumper
  print Vis::Dumper(items);

=head1 DESCRIPTION

The Vis package provides additional interfaces to Data::Dumper
which provide more compact output and are especially 
4uitable for error/diagnostic messages:

=over

=item

There is no final newline when using the new interfaces.

=item

Multiple array and hash members are shown on the same line, subject to
a maximum line length given by the Maxwidth() method or $Vis::VisMaxwidth.

The vis() call shown above produces the following output:

  struct={
    complicated => [ "lengthy", "stuff", 1, 2, 3, 4, 5, 6, 7,
      8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
    ]
  }

=item

By default, just the data items are shown, no variable assignments.

=back

Vis is a subclass of Data::Dumper and is a drop-in replacement.  The old
APIs work exactly as before but provide more compact output (a final newline
is included when using the old APIs).

=head2 vis $item,... 

Format an arbitrary Perl data structure for printing, without a final newline.

Multiple arguments are each formatted separately,
separated by newlines.

=head2 avis @array

C<avis> formats an array or list in parenthesis: C<(arg1,arg2,arg3,...)>.
This allows @arrays to be shown without taking a reference.

=head2 svis 'string to be interpolated',...

The string(s) are interpolated similar to Perl double-quoted strings,
except that embedded variable references are replaced by the results 
from vis() or avis().  Multiple strings are concatenated.

The strings should SINGLE QUOTED so Perl will not interpolate variable 
references before passing the string.

=head2 dvis 'string to be interpolated',...

('d' stands for 'debug display').  C<dvis> is identical to C<svis>
except that interpolated variable references are automatically prefixed by
the name of the variable (or the expression).  
For example, dvis('$foo $ary[3]\n') yields '$foo=<value> $ary[3]=<value><newline>'.

=head2 OO interfaces

vnew, anew, snew, and dnew are constructors which work analogously to 
Data::Dumper->new(...).   See SYNOPSIS above. 

The Maxwidth() method sets or gets the maximum characters for formatted lines.

Any of the Data::Dumper methods may also be called on the resulting object.  

Perhaps the most useful is Useqq(), which controls whether strings should be 
rendered in "double quoted" syntax with newlines appearing as "\n" instead 
of actual newlines.

By default "double quoted" style is used.  

=head2 quo($string) 

If necessary, the string is 'single-quoted' suitable for later parsing 
by /bin/sh.  If the string does not contain special characters or embedded
spaces, the original the string is returned unchanged.

If $string is actually a ref, then the stringified data structure is 
substituted (in 'quotes').

=head2 forcequo($string)

The string is 'single-quoted' for /bin/sh if even if not necessary.

=head1 SEE ALSO

Data::Dumper

=cut

