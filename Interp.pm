use strict; use warnings; use utf8;
our $VERSION = sprintf "%d.%03d", q$Revision: 1.18 $ =~ /(\d+)/g; 

# Copyright © Jim Avera 2012.  Released into the Public Domain 
# by the copyright owner.  (james_avera AT yahoo đøţ ¢ÔḾ) 
# Please retain the preceeding attribution in any copies or derivitives.

# Documentation is at the end

package Vis;

use Exporter;
use Data::Dumper ();
use Carp;
use feature qw(switch state);
use POSIX qw(INT_MAX);

our @ISA       = qw(Exporter Data::Dumper);
our @EXPORT    = qw(vis avis svis dvis Dumper qsh forceqsh);
our @EXPORT_OK = ('$Maxwidth', '$Quotestyle', '$Debug');

# Used by non-oo functions, and initial settings for oo constructors
our ($Maxwidth, $Quotestyle, $Debug);
$Maxwidth   = 72   unless defined $Maxwidth;
$Quotestyle = '"'  unless defined $Quotestyle;
$Debug      = 0    unless defined $Debug;

# Functional (non-oo) APIs
sub vis(@)    { return __PACKAGE__->vnew(@_)->Dump; }
sub avis(@)   { return __PACKAGE__->anew(@_)->Dump; }
sub svis(@)   { @_ = (__PACKAGE__->snew(@_)); goto &DB::Vis_DB_DumpInterpolate }
sub dvis(@)   { @_ = (__PACKAGE__->dnew(@_)); goto &DB::Vis_DB_DumpInterpolate }

# Provide Data::Dumper non-oo APIs 
sub Dumper(@) { return __PACKAGE__->Dump([@_]); }
sub Dumpf(@)  { return __PACKAGE__->Dump([@_]); }
sub Dumpp(@)  { print __PACKAGE__->Dump(@_); }

# Note: All Data::Dumper methods can be called on Vis objects

# Split keys into "components" (e.g. 2.16.A) and sort each component
# numerically if both corresponding are numbers, else lexicographically.
sub _sortkeys {
  my $hash = shift;
  return [
    sort { my @a = split /\b/,$a; 
           my @b = split /\b/,$b; 
           for (my $i=0; $i <= $#a; ++$i) {
             return 1 if $i > $#b;  # b is shorter
             my $r = ($a[$i] =~ /^\d+$/ && $b[$i] =~ /^\d+$/) 
                      ? ($a[$i] <=> $b[$i]) : ($a[$i] cmp $b[$i]) ;
             return $r if $r != 0;
           }
           return -1 if $#a < $#b; # a is shorter
           return 0;
         }
         keys %$hash
  ]
}

sub _config_defaults {
  my $self = shift;
  $self->Quotekeys(0)->Sortkeys(\&_sortkeys)->Terse(1)->Indent(1)->
           Debug($Debug)->Maxwidth($Maxwidth)->Quotestyle($Quotestyle);
}

# vnew(items...)                
# anew(items...)              
# snew(strings...)             
# dnew(strings...)             
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
  # each interpolated $varname etc. separately, thereby using any 
  # configurations (such as Quotestyle or Indent) set by the user.
  my $obj = (bless($class->SUPER::new([@_]), $class))->_config_defaults();
  $obj->{VisType} = 's';
  $obj;
}
sub dnew {
  my $obj = snew(@_);
  $obj->{VisType} = 'd';
  $obj;
}

# new([items...],[names...])  
# Functionally compatible with Data::Dumper->new(), including final newline.
# The differences include:
#   * Condensed output format 
#   * Defaults to Deepcopy(1)  
#   * Defaults to Purity(1) 
sub new {
  my $class = shift;
  bless($class->SUPER::new(@_), $class)
    ->_config_defaults()
    ->Deepcopy(1)
    ->Purity(1)
    ;
}

sub Debug {
  my($s, $v) = @_;
  defined($v) ? (($s->{VisDebug} = $v), return $s) : $s->{VisDebug};
}
sub Maxwidth {
  my($s, $v) = @_;
  defined($v) ? (($s->{Maxwidth} = $v), return $s) : $s->{Maxwidth};
}
sub Quotestyle {
  my($s, $v) = @_;
  defined($v)
   ? (($s->{Quotestyle} = $v), 
      return $s->Useqq(($v eq '"' || substr($v,1,1) eq 'q') ? 1:0))
   : $s->{Quotestyle};
}
sub Useqq {
  my($s, $v) = @_;
  local $_;
  if (defined $v) {
    $s->{Quotestyle} =~ /^(q+|"|')(.*)/ or die "bug";
    given ($1) {
      when ("'")  { $s->{Quotestyle} = '"'    if $v;   }
      when ("q")  { $s->{Quotestyle} = "qq$2" if $v;   }
      when ('"')  { $s->{Quotestyle} = "'"    if ! $v; }
      when ("qq") { $s->{Quotestyle} = "q$2"  if ! $v; }
    }
    return $s->SUPER::Useqq($v);
  } else {
    return $s->SUPER::Useqq();
  }
}

sub Dump {
  my ($self) = @_;
  local $_; # preserve caller's $1 etc.

  $self = $self->new(@_[1..$#_]) unless ref $self;

  goto &DB::Vis_DB_DumpInterpolate
    if ($self->{VisType}//"") =~ /[sd]/;

  my $debug = $self->{VisDebug};

  $_ = $self->SUPER::Dump;

  print "===== RAW =====\n${_}---------------\n" if $debug;

  # Split into logical lines, being careful to preserve newlines in strings. 
  # The "delimiter" is the whole (logical) line, including final newline,
  # which is returned because it is in a (capture group).
  my $quotestyle = $self->Quotestyle();
  my $split_re = ($quotestyle eq '"' || substr($quotestyle,1,1) eq "q")
                 ? qr/( (?: "(?: [^"]++ |\\" )*" | [^"\n]+ )* \n )/xo
                 : qr/( (?: '(?: [^']++ |\\' )*' | [^'\n]+ )* \n )/xo ;
  my @lines = (grep {defined} split /$split_re/, $_);

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
        # Omit space between [] or {} and contents 
        #   [1, 2, 3] instead of [ 1, 2, 3 ]
        #   {key => value} insteas of { key => value }
        my $sep = ($Icode =~ /[\[\{]\s*$/ || $Jcode =~ /^[\]\}]/) ? "" : " "; 
        next 
          if $Ilen + length($sep) + length($Jcode) > $self->{Maxwidth};
        substr($lines[$I],$Ilen,1) = $sep;
        substr($lines[$I],$Ilen+length($sep)) = substr($lines[$J], $Jindent);
        $lines[$J] = "";
        $restart = $I if ($I < $restart);
        last if ++$J > $#lines;
        redo;
      } else {
        print "NOT JOINED.\n" if $debug;
      }
    }
  }

  $_ = join "", @lines;

  if (($self->{VisType}//"") eq 'a') {
    s/^[\[{]/\(/ or confess "bug($_)"; # convert to "(list,of,args)"
    s/[\]}]$/\)/ or confess "bug($_)";
  }
  if (exists $self->{VisType}) {
    s/\s+\z//s;  # omit final newline except when emulating Data::Dumper
  }
  if (/^['"]/) {
    if ($quotestyle =~ /^["']$/) {
      # most common cases first.  Ok as-is.
    }
    elsif ($quotestyle =~ /^q([^q])(.)$/) { # q()
      my ($L,$R) = ($1,$2);
      die "bug" if $self->Useqq(); # Should be 'singlequoted'
      $_ = substr($_,1,length($_)-2);
      s/\\'/'/g;
      s/(\Q${L}\E|\Q${R}\E)/\\$1/g;
      $_ = "q${L}${_}${R}";
    }
    elsif ($quotestyle =~ /^qq(.)(.)$/) {  # qq()
      my ($L,$R) = ($1,$2);
      die "bug" unless $self->Useqq();  # Should be "single quoted"
      $_ = substr($_,1,length($_)-2);
      s/\\"/"/g;
      s/(\Q${L}\E|\Q${R}\E)/\\$1/g;
      $_ = "qq${L}${_}${R}";
    }
    else {
      confess "Vis: Quotestyle set to illegal value <<${quotestyle}>>\n";
    }
  }

  return $_;
}

sub forceqsh($) {
  # Unlike Perl, the bourne shell does not recognize backslash escapes 
  # inside '...'.  Therefore the quoting has to be interrupted for any
  # embedded single-quotes so they can be contatenated as \' or "'"
  # 
  # Vis with Quotestyle("'") will format ' and \ as \' and \\ respectively. 

  local $_ = __PACKAGE__->vnew(shift)->Quotestyle("'")->Dump;

  unless (/^'/) {
    # The input was a reference to an aggregate; re-format as 'string'
    $_ = __PACKAGE__->vnew($_)->Quotestyle("'")->Dump;
  }

  s/\\\\/\\/g;   # 'foo\\bar\\\'baz' -> 'foo\bar\\'baz'
  s/\\'/'\\''/g; # 'foo\bar\'baz'    -> 'foo\bar'\''baz'

  return $_;
}

sub qsh(;@) {
  @_ = ($_) if @_==0;  # format $_ if no args
  join " ",
       map {
         (/[^-\w_\/:\.]/ || $_ eq "") ? forceqsh($_) : $_
       } 
       @_;
}

package DB;

# These are aliases for forced-globals, so we have to do this for them
# to be visible in eval "..." in package DB. 
use English qw( -no_match_vars ) ;

# Implement Dump() method for svis and dvis styles
# This must be in package DB so that eval "" uses the caller's context
sub Vis_DB_DumpInterpolate {
  my ($self) = @_;

  # This allows the caller's @_ to be interpolated
  # See https://rt.perl.org/rt3//Public/Bug/Display.html?id=112896
  () = caller 1;
  local *_ = \@DB::args;

  my $debug = $self->{VisDebug};
  my $display_mode = $self->{VisType} eq 'd';

  state $qstr_re = qr{ " ( [^"\\]+ | \\. ) " | ' ( [^'\\]+ | \\. ) ' }x;
  state $expr_re = qr{ 
    (
        [^"'{}()\[\]]+
      | $qstr_re
      | \{ (?-1) \}
      | \( (?-1) \)
      | \[ (?-1) \]
    )*
  }x;
  state $scalar_index_re = qr{ 
    ( (?:->)? (?: \{ $expr_re \} | \[ $expr_re \] ) )+ 
  }x;
  state $slice_re = qr{ 
    (?: \{ $expr_re \} | \[ $expr_re \] ) 
  }x;
  state $variable_re = qr/   # sigl could be $ or @
      \#?\w+(?:::\w+)*   # @name $pkg::name $#name $1
    | \#?\$\w+(?:::\w+)* # $$ref $#$ref
    | \^\w               # $^N   (control-character 'punctuation' variable)
    | [^\w\s\{\$]        # $^ $? (regular 'punctuation' variable)
    | \{ $expr_re \}     # ${ref expression} or ${^SPECIALNAME}
  /x;

  my @actions;
  {
    # Localize $_ while running regexprs to preserve the caller's $1 etc.
    # N.B. The evals are executed after $_ is restored.
    local $_ = join "", $self->Values();
    while (1) {
      if (
        # Sigh.  \G does not work with (?|...) in Perl 5.12.4
        # https://rt.perl.org/rt3//Public/Bug/Display.html?id=112894

        # $name $name[expr] $name->{expr} ${refexpr}->[expr] etc.
        /\G (?!\\)(\$)( $variable_re ${scalar_index_re}? )/xsgc
        ||
        # ${name} ${name->[expr]} etc. (loosing the curlies)
        /\G (?!\\)(\$) \{ ( $variable_re ${scalar_index_re}? ) \}/xsgc
        ||
        # @name @name[slice] @name{slice} @{refexpr}[slice] etc.
        /\G (?!\\)(\@)( $variable_re ${slice_re}? )/xsgc
        ||
        # @{name} @{name[slice]} etc. (loosing the curlies)
        /\G (?!\\)(\@) \{ ( $variable_re ${slice_re}? ) \}/xsgc
        ||
        # %name 
        /\G (?!\\)(\%)( $variable_re )/xsgc
       )
      {
        push @actions, ['e',$1,$2];  # eval $1$2
        if ($debug) {
          print "### regex match ($_):";
          for my $i (1..$#+) {
            eval "print \" $i=«\$$i»\" if defined \$$i;"; die "bug" if $@; 
          }
          print "\n";
        }
      }
      elsif (/\G ( (?: [^\$\@%\\]+ | \\. )+ ) /xsgc) {
        push @actions, ['t',$1];  # interpolate \n etc. in plain text
      }
      else {
        if (/\G./) {
          die "Vis bug: next:",substr($_,pos,4),
              "... pos=",pos," in:\n$_\n".(" "x pos)."^\n "
        }
        last;
      }
    }
  }

  # $_ and $1 etc. have now been restored to the caller's values
  # DO NOT use regex with (capture groups) between here and the eval below!

  my $result = "";
  foreach my $action (@actions) {
    my $act = $action->[0];
    if ($act eq 'e') {
      my ($sigl, $rhs) = @$action[1,2];
      # $sigl$rhs is a Perl expression giving the desired value.  
      # Note that the curlies were dropped from ${name} in the string
      # (because braces can not be used that way in expressions, only strings)
      print "### EVAL $sigl$rhs\n" if $debug;
      { local $@ = $@;
        my @items;
        if ($rhs eq '@' && $sigl eq '$') {
          # Special case--we can't see caller's $@ inside another eval!
          @items = ($@);
        } else {
          if (($rhs eq 'PREMATCH' || $rhs eq 'MATCH' || $rhs eq 'POSTMATCH')
               && $sigl eq '$') {
            # we normally don't import these to avoid a performance hit
            local $_;  # defensive programming
            English->import("$sigl$rhs");
          }
          @items = eval "($sigl$rhs);";
          Carp::confess "($sigl$rhs)$@" if $@ && $debug;
          Carp::croak("Error interpolating '$sigl$rhs': $@") 
            if $@ =~ s/ at \(eval.*//;
        }
        #my $prefix = $display_mode ? ($sigl eq '$' ? "":$sigl)."$rhs=" : "";
        my $prefix = $display_mode ? "$sigl$rhs=" : "";
        if ($sigl eq '$') {
          $self->Reset()->Values([$items[0]])->{VisType} = 'v';
        }
        elsif ($sigl eq '@') {
          $self->Reset()->Values([\@items])->{VisType} = 'a';
        }
        elsif ($sigl eq '%') { ### TODO: DEBUG THIS
          my %hash = @items;
          $self->Reset()->Values([\%hash])->{VisType} = 'a';
        }
        else { die "bug" }
        $result .= $prefix.$self->Dump;
      }
    }
    elsif ($act eq 't') {
      # Interpolate \n etc.
      my $text = $action->[1];
      if ($text =~ /\b((?:ARRAY|HASH)\(0x[^\)]*\))/) {
        state $warned=0;
        Carp::carp "Warning: String passed to svis or dvis may have been interpolated by Perl\n(use 'single quotes' to avoid this)\n" unless $warned++;
      }
      print "### PLAIN «$text»\n" if $debug;
      { local $@;
        chomp (my $value = eval qq{<<"ViSEoF"
$text
ViSEoF
});
        Carp::confess "Unexpected eval faulre ($text): $@" if $@;
        $result .= $value;  # plain text
      }
    } else {
      die "bug";
    }
  }
  return $result;
}

1;
__END__

=head1 NAME 

Vis - Improve Data::Dumper to format arbitrary Perl data in messages

=head1 SYNOPSIS

  use Vis;

  my %hash = ( complicated => ['lengthy','stuff',1..20] );
  my $href = \%hash;

  print svis 'href=$href\n hash=%hash\n ARGV=@ARGV\n'; # SINGLE quoted!
  print dvis 'Display of variables: $href %hash @ARGV\n'; 
  print "href=", vis($href), "\n";
  print "ARGV=", avis(@ARGV), "\n";

  print "href=", Vis->vnew($href)->Quotestyle("'")->Maxlength(110)->Dump, "\n";
  print "ARGV=", Vis->anew(@ARGV)->Dump, "\n";
  print Vis->snew('href=$href\n')->Dump;
  print Vis->dnew('$href\n')->Dump;

  foreach ($ENV{HOME}, "/dir/safe", "Uck!", 
           "My Documents", "Qu'ote", 'Qu"ote') 
  {
    system( "set -x; /bin/ls -ld ".qsh($_) );
  }

  print Vis->new([items],[names])->Dump,"\n";  # same API as Data::Dumper
  print Vis::Dumper(items),"\n";

=head1 DESCRIPTION

The Vis package provides convenience interfaces to Data::Dumper
to generate error/diagnostic messages for human consumption.
Also, aggregate data structures are formatted much more compactly.

=over

=item

There is no final newline when using the new interfaces.

=item

Multiple array and hash members are shown on the same line, subject to
a maximum line length given by $Vis::Maxwidth or the Maxwidth() method.

The vis() call shown above produces the following output:

  href={
    complicated => [ "lengthy", "stuff", 1, 2, 3, 4, 5, 6, 7,
      8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
    ]
  }

=item

By default, just the data items are shown, no variable assignments.

=back

Vis is a subclass of Data::Dumper and is a drop-in replacement.  The old
APIs work exactly as before but provide more compact output at the cost
of slight additional overhead for re-formatting (Data::Dumper can still
be called directly to access the original implementation).  
A final newline is included when using the old APIs.  

=head2 vis $item, ... 

Format an arbitrary Perl data structure for printing, without a final newline.

Multiple arguments are each formatted separately,
separated by newlines.

=head2 avis @array

C<avis> formats an array or list in parenthesis: C<(arg1,arg2,arg3,...)>.
This allows @arrays to be shown without taking a reference.

=head2 svis 'string to be interpolated',...

Variables and escapes in the string(s) are interpolated as 
in Perl double-quotish strings except that expression values 
are formatted using vis() or avis() (for $ or @ expressions, respectively).
String values appear 'quoted' and complex data structures are shown in full.
Multiple arguments are concatenated.

The input string(s) should written SINGLE QUOTED so Perl will not 
interpolate them before passing to svis().

In additional to all the forms recognized by Perl in double-quotish strings,
vis() interpolates C<%name> into S<<< C<< (key => value ...) >> >>>

=head2 dvis 'string to be interpolated',...

('d' is for 'debug display').  C<dvis> is identical to C<svis>
except that interpolated variables are automatically prefixed by
the name of the variable (or the expression).  
For example, S<dvis('$foo $i $ary[$i]\n')> 
yields S<< "$foo=<value> $i=<value> $ary[$i]=<value><newline>". >>

=head2 OO interfaces

C<vnew>, C<anew>, C<snew>, and C<dnew> 
are constructors which work analogously to 
S<< Data::Dumper->new(...). >>   See SYNOPSIS above. 

=head2 Configuration Variables or Methods

These work similarly to methods of Data::Dumper (which are also 
available):

=over 4

=item $Vis::Maxwidth  I<or>  I<$OBJ>->Maxwidth(I<[NEWVAL]>) 

Sets or gets the maximum number of characters for formatted lines.

=item $Vis::Quotestyle I<or>  I<$OBJ>->Quotestyle(I<[NEWVAL]>) 

This may be set to B<"'"> B<'"'> B<'q()'> or B<'qq()'> to indicate the style of
quotes used for strings (any pair of delimiters may be specified for q or qq).

Double-quotish styles (" or qq) use I<\n \t \r> escapes, whereas in 
single-quotish styles, newlines appear as themselves, etc.
The default is '"'.

=item $Vis::Useqq I<or>  I<$OBJ>->Useqq(I<[NEWVAL]>) 

Use C<Quotestyle> instead.  C<Useqq> is provided for compatibiliy 
with Data::Dumper.  Calling either method will automatically set the
other value to correspond (Useqq will change a previously-set 
Quotestyle between C<q()> and C<qq()> while preserving delimiters).

=back

Changes to global variables should usually be localized, e.g.

 { local $Vis::Quotestyle = "'";
   ...code which uses vis() or avis()...
 }

=head2 qsh 

=head2 qsh $data, ...

The data items ($_ by default) are formatted and 'quoted' for /bin/sh 
if necessary, and concatenated with a single space between each item.

In other words, if an item is a non-empty string containing 
only "safe" characters, then that item is returned unchanged, otherwise
it will be 'quoted'.  
References become something like S<<< C<< '[42, {key => 123}]' >>. >>>

Note that /bin/sh 'quoting' rules differ from Perl's.

=head2 forceqsh $data

The string is 'quoted' for /bin/sh if even if not necessary.

Unlike C<qsh>, C<forceqsh> requires exactly one argument.

=head1 SEE ALSO

Data::Dumper

=cut

#!/usr/bin/perl
# Tester for module Vis.  TODO: Convert to CPAN module-test setup
use strict; use warnings;
use Carp;
use English;
#use lib "$ENV{HOME}/lib/perl";
use Vis;
binmode STDOUT, 'utf8';
binmode STDERR, 'utf8';
select STDERR; $|=1; select STDOUT; $|=1;

sub check($$$) {
  my ($code, $expected, $eval_result) = @_;
  confess "BUG(EVAL ERR): $@" if $@;
  $eval_result //= '<undef>';
  local $_;
  print $eval_result;
  print "\n" unless $eval_result =~ /\n$/;
  confess "\nTEST FAILED: $code\n"
         ."Expected «${expected}»\n"
         ."     Got «${eval_result}»\n"
    unless $expected eq $eval_result;
}

@ARGV = ('fake','argv');
$. = 1234;
$ENV{EnvVar} = "Test EnvVar Value";
$_ = "GroupA.GroupB";
my %tophash = (A=>111,B=>222,C=>{d=>888,e=>999},D=>{});
my @toplex_a = (0,1,2,\%tophash,[],[0..9]);
my $toplex_r = \@toplex_a;
my $byte_str = join "",map { chr $_ } 10..30;
my $unicode_str = join "",map {
        eval sprintf "\" \\N{U+%04X}\"",$_} (0x263A..0x2650);
print "unicode_str=$unicode_str\n";

{ my $code='avis(@_)'; check $code, '()', eval $code; }

sub f {
  my $zero = 0;
  my $one = 1;
  my $two = 2;
  my $EnvVarName = 'EnvVar';
  my $flex = 'Lexical in sub f';
  my $flex_ref = \$flex;
  my $ARGV_ref = \@ARGV;
  /(.*)\W(.*)/p or die "nomatch"; # set $1 and $2 

  { my $code = q( "vis(\$_):".vis($_) );
    check $code, 'vis($_):"GroupA.GroupB"', eval $code;
  }

  { my $code = q( svis(' svis:$_ $flex con','caten','ated\n') );
    check $code, ' svis:"GroupA.GroupB" "Lexical in sub f" concatenated'."\n", eval $code;
  }
  eval { die "FAKE DEATH\n" };  # set $@

  for my $test (
    [ q(aaa\\\\bbb), q(aaa\bbb) ],
    [ q($unicode_str\n), qq(\$unicode_str=\" \\x{263a} \\x{263b} \\x{263c} \\x{263d} \\x{263e} \\x{263f} \\x{2640} \\x{2641} \\x{2642} \\x{2643} \\x{2644} \\x{2645} \\x{2646} \\x{2647} \\x{2648} \\x{2649} \\x{264a} \\x{264b} \\x{264c} \\x{264d} \\x{264e} \\x{264f} \\x{2650}\"\n) ],
    [ q($byte_str\n), qq(\$byte_str=\"\\n\\13\\f\\r\\16\\17\\20\\21\\22\\23\\24\\25\\26\\27\\30\\31\\32\\e\\34\\35\\36\"\n) ],
    [ q($_\n), qq(\$_=\"GroupA.GroupB\"\n) ],
    [ q($flex\n), qq(\$flex=\"Lexical in sub f\"\n) ],
    [ q($$flex_ref\n), qq(\$\$flex_ref=\"Lexical in sub f\"\n) ],
    [ q($.\n), qq(\$.=1234\n) ],
    [ q($NR\n), qq(\$NR=1234\n) ],
    [ q($/\n), qq(\$/=\"\\n\"\n) ],
    [ q($\\\n), qq(\$\\=undef\n) ],
    [ q($"\n), qq(\$\"=\" \"\n) ],
    [ q($~\n), qq(\$~=\"STDOUT\"\n) ],
    [ q($^\n), qq(\$^=\"STDOUT_TOP\"\n) ],
    [ q($:\n), qq(\$:=\" \\n-\"\n) ],
    [ q($^L\n), qq(\$^L=\"\\f\"\n) ],
    [ q($?\n), qq(\$?=0\n) ],
    [ q($[\n), qq(\$[=0\n) ],
    [ q($^N\n), qq(\$^N=\"GroupB\"\n) ],
    [ q($+\n), qq(\$+=\"GroupB\"\n) ],
    [ q(@+\n), qq(\@+=(13, 6, 13)\n) ],
    [ q(@-\n), qq(\@-=(0, 0, 7)\n) ],
    [ q($;\n), qq(\$;=\"\\34\"\n) ],
    [ q($1\n), qq(\$1=\"GroupA\"\n) ],
    [ q($2\n), qq(\$2=\"GroupB\"\n) ],
    [ q($3\n), qq(\$3=undef\n) ],
    [ q(${^MATCH}\n), qq(\${^MATCH}=\"GroupA.GroupB\"\n) ],
    [ q(@ARGV\n), qq(\@ARGV=(\"fake\", \"argv\")\n) ],
    [ q($ENV{EnvVar}\n), qq(\$ENV{EnvVar}=\"Test EnvVar Value\"\n) ],
    [ q($ENV{$EnvVarName}\n), qq(\$ENV{\$EnvVarName}=\"Test EnvVar Value\"\n) ],
    [ q(%tophash\n), qq(\%tophash=(A => 111, B => 222, C => {d => 888, e => 999}, D => {})\n) ],
    [ q(@toplex_a\n), qq(\@toplex_a=(0, 1, 2, {A => 111, B => 222, C => {d => 888, e => 999}, D => {}}, [],\n  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n)\n) ],
    [ q(@$toplex_r\n), qq(\@\$toplex_r=(0, 1, 2, {A => 111, B => 222, C => {d => 888, e => 999}, D => {}}, [],\n  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n)\n) ],
    [ q($toplex_r->[3]{C}->{e}\n), qq(\$toplex_r->[3]{C}->{e}=999\n) ],
    [ q($@\n), qq(\$\@=\"FAKE DEATH\\n\"\n) ],
    [ q(@_\n), qq(\@_=(42, [0, 1, 2, {A => 111, B => 222, C => {d => 888, e => 999}, D => {}},\n    [], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n  ]\n)\n) ],
    [ q($#_\n), qq(\$#_=1\n) ],
    [ q($#toplex_a), qq(\$#toplex_a=5) ],
    [ q($#toplex_a\n), qq(\$#toplex_a=5\n) ],
    [ q(@$toplex_r[$zero,$one]\n), qq(\@\$toplex_r[\$zero,\$one]=(0, 1)\n) ],
    [ q(@toplex_a[$zero,$one]\n), qq(\@toplex_a[\$zero,\$one]=(0, 1)\n) ],
  )
  {
    my ($dvis_input, $expected) = @$test;
    my $actual = dvis $dvis_input;
    print $actual;
    { local $_; print "<no newline>\n" unless $actual =~ /\n\z/s; }
    confess "\ndvis test failed: input «${dvis_input}»\n"
         ."Expected «${expected}»\n"
         ."     Got «${actual}»\n"
    unless $expected eq $actual;

    # Check Quotestyle options
    for my $qs (qw{ ' " q() qq() }) {
      my $input = $expected.$dvis_input.'qqq@_(\(\))){\{\}\""'."'"; # gnarly

      my $exp = $input;
      use feature 'switch';
      given($qs) {
        when ("'") { 
          $exp =~ s/([\\'])/\\$1/gs;
          $exp = "'${exp}'";
        }
        when ('q()') { 
          $exp =~ s/([\\()])/\\$1/gs;
          $exp = "q(${exp})";
        }
        when ('"') { 
          $exp =~ s/([\$\@"\\])/\\$1/gs;
          $exp =~ s/\n/\\n/gs;
          $exp =~ s/\t/\\t/gs;
          $exp = "\"${exp}\"";
        }
        when ('qq()') { 
          $exp =~ s/([\$\@\\()])/\\$1/gs;
          $exp =~ s/\n/\\n/gs;
          $exp =~ s/\t/\\t/gs;
          $exp = "qq(${exp})";
        }
      }
      my $act =  Vis->vnew($input)->Quotestyle($qs)->Dump;
      die "\n\nQuotestyle $qs bug:\n"
         ."   Input   «${input}»\n"
         ."  Expected «${exp}»\n"
         ."       Got «${act}»\n"
        unless $exp eq $act;
    }
  }
}
sub g($) {
  local $_ = 'SHOULD NEVER SEE THIS';
  goto &f;
}
&g(42,$toplex_r);
print "Tests passed.\n";
exit 0;
# End Tester
