use strict; use warnings; 
our $VERSION = sprintf "%d.%03d", q$Revision: 1.9 $ =~ /(\d+)/g; 

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
our @EXPORT    = qw(vis avis svis Dumper quo forcequo);
our @EXPORT_OK = ('$VisMaxwidth', '$VisUseqq');

# Used by non-oo functions, and initial settings for oo constructors
our ($VisMaxwidth, $VisUseqq);
$VisMaxwidth = 72  unless defined $VisMaxwidth;
$VisUseqq    = 1   unless defined $VisUseqq;

# Functional (non-oo) APIs
sub vis(@)    { return __PACKAGE__->vnew(@_)->Dump; }
sub avis(@)   { return __PACKAGE__->anew(@_)->Dump; }
sub svis(@)   { unshift @_,__PACKAGE__; goto &DB::Vis_DB_svis; }

# Emulate Data::Dumper non-oo APIs except for more compact output
sub Dumper(@) { return __PACKAGE__->Dump([@_]); }
sub Dumpf(@)  { return __PACKAGE__->Dump([@_]); }
sub Dumpp(@)  { print __PACKAGE__->Dump([@_]); }

sub _config_defaults {
  my $self = shift;
  $self->Quotekeys(0)->Sortkeys(1)->Terse(1)->Indent(1)->
           Useqq($VisUseqq)->Maxwidth($VisMaxwidth)
}

# vnew(items...)                
# anew(items...)              
# snew(strings...)             
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
  # The Dumper object is created with no dump items because our Dump()
  # will call it repeatedly to dump each interpolated $varname etc. 
  # using whatever settings the user may have installed by calling methods
  # such as Useqq() etc.
  my $obj = (bless($class->SUPER::new([]), $class))->_config_defaults()->Useqq(0);
  $obj->{VisType} = 's';
  $obj->{VisString} = join "", @_;
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
  my $self = shift;

  $self = $self->new(@_) unless ref $self;

  if (($self->{VisType}//"") eq 's') {
    # This code has to be in package DB so the user's context is visible
    @_ = ($self);
    goto &DB::Vis_DB_sdump;
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

# These must be in package DB so that eval "" uses the caller's context

sub Vis_DB_svis(@) {
  my $package = shift;
  return $package->snew(@_)->Dump;
}

sub Vis_DB_sdump {
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
  
  local $_ = $self->{VisString} // die "bug";

  my @parts;
  while (1) {
    # Sigh.  \G does not work with (?|...) in Perl 5.12.4
    # https://rt.perl.org/rt3//Public/Bug/Display.html?id=112894
    if (
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
      if ($rhs =~ /^_\b/) {
        # Perl limitation (bug?): '@_' does not see caller's context!
        print "### UNSUPP $sigl$rhs\n" if $self->{VisDebug};
        push @parts, "$sigl$rhs"; # don't show $self
        next;
      }
      if ($sigl eq '$') {
        # $name or ${scalar-ref expression}
        print "### SCALAR $sigl$rhs\n" if $self->{VisDebug};
        my $item = eval "$sigl$rhs";
        #Carp::croak($@) if $@ =~ s/ at \(eval.*//;
        Carp::confess "($sigl$rhs)$@" if $@;
        $self->Reset()->Values([$item])->{VisType} = 'v';
        push @parts, $self->Dump;
      } else {
        # @arrayname or @{array-ref expression}
        print "### ARRAY $sigl$rhs\n" if $self->{VisDebug};
        my @items = eval "$sigl$rhs";
        #Carp::croak($@) if $@ =~ s/ at \(eval.*//;
        Carp::confess "($sigl$rhs)$@" if $@;
        $self->Reset()->Values([\@items])->{VisType} = 'a';
        push @parts, $self->Dump;
      }
    }
    elsif (/\G ( (?: [^\$\@\\]+ | \\. )* ) /xsgc)  # plain text
    {
      # Interpolate \n etc.
      print "### PLAIN $1\n" if $self->{VisDebug};
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

=head1 SYNOPSIS

  use Vis;

  my $struct = { complicated => ['lengthy','stuff',1..20] };

  print "struct=", vis($struct), "\n";
  print "My args are ", avis(@_), "\n";
  print svis 'struct=$struct\nMy args are @_\n'; # note SINGLE quotes

  my $obj = Vis->vnew($struct);
  my $obj = Vis->qnew(@array);
  my $obj = Vis->snew($string_to_interpret);

  print "struct=", Vis->vnew($struct)->Useqq(0)->Dump, "\n";

  print "My args are ", Vis->anew(@_)->Useqq(0)->Dump, "\n";

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
a maximum line length given by $Vis::VisMaxwidth
$Vis::VisMaxwidth is not exported by default.

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

