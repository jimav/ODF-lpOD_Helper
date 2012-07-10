use strict; use warnings; use utf8;
our $VERSION = sprintf "%d.%03d", q$Revision: 1.31 $ =~ /(\d+)/g; 

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
sub debugvis($) 
{local $/="\n"; chomp(my $s = Data::Dumper->new([shift])->Useqq('utf8')->Terse(1)->Indent(0)->Dump); $s;}

our @ISA       = qw(Exporter Data::Dumper);
our @EXPORT    = qw(vis avis svis dvis Dumper qsh forceqsh);
our @EXPORT_OK = qw(u 
                    $Maxwidth $Debug $Useqq $Quotekeys $Sortkeys $Terse $Indent);

# Used by non-oo functions, and initial settings for oo constructors.
our ($Maxwidth, $Debug, $Useqq, $Quotekeys, $Sortkeys, $Terse, $Indent);

$Maxwidth   = undef       unless defined $Maxwidth; # undef to auto-detect tty width
$Debug      = 0           unless defined $Debug;

# The following Vis defaults override Data::Dumper defaults
$Useqq      = 1           unless defined $Useqq;
$Quotekeys  = 0           unless defined $Quotekeys;
$Sortkeys   = \&_sortkeys unless defined $Sortkeys;
$Terse      = 1           unless defined $Terse;
$Indent     = 1           unless defined $Indent;

# Functional (non-oo) APIs
sub u(@);
sub u(@)      { @_ == 1 
                  ? defined($_[0]) ? $_[0] : "undef"
                  : (map { u($_) } @_)
              }
#sub u($)      { defined($_[0]) ? $_[0] : "undef" }
sub vis(@)    { return __PACKAGE__->vnew(@_)->Dump; }
sub avis(@)   { return __PACKAGE__->anew(@_)->Dump; }
sub svis(@)   { @_ = (__PACKAGE__->snew(@_)); goto &DB::Vis_DB_DumpInterpolate }
sub dvis(@)   { @_ = (__PACKAGE__->dnew(@_)); goto &DB::Vis_DB_DumpInterpolate }

# Provide Data::Dumper non-oo APIs 
sub Dumper(@) { return __PACKAGE__->Dump([@_]); }
sub Dumpf(@)  { return __PACKAGE__->Dump([@_]); }
sub Dumpp(@)  { print __PACKAGE__->Dump(@_); }

# Note: All Data::Dumper methods can be called on Vis objects

# Split keys into "components" (e.g. 2_16.A has 3 components) and sort each 
# component numerically if the corresponding items are both numbers.
sub _sortkeys {
  my $hash = shift;
  return [
    sort { my @a = split /([_\W])/,$a; 
           my @b = split /([_\W])/,$b; 
           for (my $i=0; $i <= $#a; ++$i) {
             return 1 if $i > $#b;  # a is longer
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

sub _unix_compatible_os() {
  # There must be a better way...
  (($^O !~ /win|dos/i && $^O =~ /ix$|ux$|bsd|svr|uni|osf|sv$/)
   || $^O eq 'drawin'
   || $^O eq 'cygwin'
  )
  && -w "/dev/null"
}
sub _get_default_width() {
  local $_;
  my $r;
  if (_unix_compatible_os) {
    if (-t STDERR) {
      no warnings;
      ($r = qx'tput cols') # N.B. linux tput prints 80 even if no tty
      ||
      (($r) = ((qx'stty -a'//"") =~ /.*; columns (\d+);/))
      ;
      { local $/ = "\n"; chomp $r if $r; }
      print "## Vis detected terminal width is ",u($r),"\n" if $Debug;
    }
  }
  # elsif(...) { ... }
  else { 
    warn "(fixme) Unrecognized OS $^O or no /dev/null" 
  }
  return $r || 80;
}

sub _config_defaults {
  my $self = shift;

  # Set double-quote style, but preserve any special 
  # $Data::Dumper::Useqq setting, e.g. 'utf8'
  $self->Useqq(1) unless $self->Useqq();

  $Maxwidth = _get_default_width() if ! defined $Maxwidth;

  $self
    ->Quotekeys($Quotekeys)
    ->Sortkeys($Sortkeys)
    ->Terse($Terse)
    ->Indent($Indent)
    ->Debug($Debug)
    ->Useqq($Useqq)
    ->Maxwidth($Maxwidth);
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
  # configurations (such as Useqq or Indent) set by the user.
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
#   * Defaults to Purity(1) 
sub new {
  my $class = shift;
  bless($class->SUPER::new(@_), $class)
    ->_config_defaults()
    ->Terse(0)
    ->Deepcopy(0)
    ->Purity(1)
    ;
}

sub Debug {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{VisDebug} = $v), return $s) : $s->{VisDebug};
}
sub Maxwidth {
  my($s, $v) = @_;
  @_ >= 2 ? (($s->{Maxwidth} = $v), return $s) : $s->{Maxwidth};
}

my $qstr_re = qr{ " (?: [^"\\]++ | \\. )* " | ' (?: [^'\\]++ | \\. )* ' }x;
my $key_re  = qr{ \w+ | $qstr_re }x;

sub Dump {
  my ($self) = @_;
  local $_; # preserve caller's $1 etc.

  $self = $self->new(@_[1..$#_]) unless ref $self;

  goto &DB::Vis_DB_DumpInterpolate
    if ($self->{VisType}//"") =~ /[sd]/;

  $_ = $self->SUPER::Dump;

  my $maxwidth = $self->{Maxwidth};

  #return $_ if $maxwidth == 0; # no condensation

  my $debug = $self->{VisDebug};

  print "===== RAW =====\n${_}---------------\n" if $debug;

  # Split into logical lines, being careful to preserve newlines in strings. 
  # The "delimiter" is the whole (logical) line, including final newline,
  # which is returned because it is in a (capture group).
  my $split_re = $self->Useqq()
                 ? qr/( (?: "(?: [^"]++ |\\" )*+" | [^"\n]++ )* \n )/xo
                 : qr/( (?: '(?: [^']++ |\\' )*+' | [^'\n]++ )* \n )/xo ;
  my @lines = (grep {defined} split /$split_re/, $_);

  # Data::Dumper output with Indent(1) is very structured, with a 
  # fixed 2-space indent increment:
  # [
  #   value,
  #   value,
  #   {
  #     key => [
  #       value,
  #       value
  #     ],
  #     key => {
  #       key => value
  #       longerkey => value,
  #     },
  #   },
  #   value
  # ]
  
  # Combine appropriate lines to make it more "horizontal"
  my $restart = 0;
  while ($restart < $#lines) 
  {
    print "RESTART at $restart \n" if $debug;
    LOOP:
    for (my $I=$restart, my $J=$restart+1, $restart=INT_MAX-1;
         $J <= $#lines; 
         $I=$J, $J=$I+1) 
    {
      # Find next pair of lines which haven't been "deleted" yet
      while ($lines[$J] eq "") { next LOOP if ++$J > $#lines; }
      while ($lines[$I] eq "") { next LOOP if ++$I >= $J; }

      my ($Iprefix,$Icode) = ($lines[$I] =~ /^(\s*)(.*)/);
      my $Iindent = length($Iprefix);

      my ($Jprefix,$Jcode) = ($lines[$J] =~ /^(\s*)(.*)/);
      my $Jindent = length($Jprefix);

      if ($debug) {
        print "===== I=$I Iind=$Iindent, J=$J Jind=$Jindent restart=$restart\n";
        my $nd = @lines <= 9 ? 1 : @lines <= 99 ? 2 : 3;
        for my $ix(0..$#lines) {
          next if $lines[$ix] eq "" && $ix != $I && $ix != $J;
          printf "[%${nd}d] ", $ix;
          print($ix==$I ? "I" : " ");
          print($ix==$J ? "J" : " ");
          print ":",($lines[$ix] eq "" ? "(empty)":debugvis($lines[$ix])),"\n";
        }
        print "--------------------\n";
      }

      if ($Iindent <= $Jindent
          && $Jcode !~ /[\[\{]$/s # J isn't opening a new aggregate
          && $Icode !~ /^[\]\}]/s # I isn't closing an aggregate
          && ($Icode =~ /(?:,|[\[\{])$/ || $Jcode =~ /^[\]\}]/)
         )
      {
        # The lines are elegible to be joined, if there is enough space

        my $Ilen = $Iindent + length($Icode);

        my $squish = (($Icode =~ /^\[ / && $Jcode =~ /^\]/) ||
                      ($Icode =~ /^\{ / && $Jcode =~ /^\}/) ||
                      ($Icode =~ /^${key_re} => \{ / && $Jcode =~ /^\}/)
                     ) ? 2:0;     

        print "Ilen=$Ilen squish=$squish len(Jcode)=",length($Jcode),
              " calc=",
              ($Ilen + length($Jcode) + 1 - $squish),
              " mw=$maxwidth\n" if $debug;

        if ($Ilen + length($Jcode) + 1 - $squish <= $maxwidth) {
          # Join the lines
          substr($lines[$I],$Ilen  ,1) = ' ';
          substr($lines[$I],$Ilen+1  ) = substr($lines[$J], $Jindent);
          if ($squish) {
            # Change [ 1, 2 ] -> [1, 2]  and  { K => val } -> {K => val}
            # This is very imperfect.  Only simple cases are squished.
            die "bug" if substr($lines[$I],$Ilen,1) ne " ";
            substr($lines[$I],$Ilen,1)="";
            $lines[$I] =~ s/^( *(?:[\[\(]|(?:${key_re} => )?\{)) /$1/ or "die";
          }
          $lines[$J] = "";

          #$restart = $I if $I < $restart;
          $restart = 0 if $I < $restart;
          last LOOP if ++$J > $#lines;
          redo LOOP;
        } else {
          # Not joined, but could have if the line wasn't too long.
          print "NO ROOM\n" if $debug;

          #if ($Jcode =~ /^(?:${key_re} => )?[\{\[]/) {
          #  indent rest of this block by $Ilen+length($sep)-$Jindent;
          #}
        }
      } else {
        print "NOT ELEGIBLE\n" if $debug;
      }
    }
  }

  $_ = join "", @lines;

  if ($self->{VisType}) {
    s/\s+\z//s;  # omit final newline except when emulating Data::Dumper
    if ($self->{VisType} eq 'a') {
      s/^[\[{]/\(/ or confess "bug($_)"; # convert to "(list,of,args)"
      s/[\]}]$/\)/ or confess "bug($_)";
    }
  }

  return $_;
}

sub forceqsh($) {
  # Unlike Perl, the bourne shell does not recognize backslash escapes 
  # inside '...'.  Therefore the quoting has to be interrupted for any
  # embedded single-quotes so they can be contatenated as \' or "'"
  # 
  # Vis with Useqq(0) will format ' and \ as \' and \\ respectively. 

  local $_ = __PACKAGE__->vnew(shift)->Useqq(0)->Dump;

  unless (/^'/) {
    # The input was a reference to an aggregate; re-format as 'string'
    $_ = __PACKAGE__->vnew($_)->Useqq(0)->Dump;
  }

  s/\\\\/\\/g;   # 'foo\\bar\\\'baz' -> 'foo\bar\\'baz'
  s/\\'/'\\''/g; # 'foo\bar\'baz'    -> 'foo\bar'\''baz'

  return $_;
}

sub qsh(;@) {
  @_ = ($_) if @_==0;  # format $_ if no args
  join " ",
       map {
         defined $_ 
           ? (/[^-\w_\/:\.]/ || $_ eq "") ? forceqsh($_) : $_
           : "undef";
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

  state $interior_re = qr{ 
    (
        [^"'{}()\[\]]+
      | $qstr_re
      | \{ (?-1) \}
      | \( (?-1) \)
      | \[ (?-1) \]
    )*
  }x;
  state $scalar_index_re = qr{ 
    ( (?:->)? (?: \{ $interior_re \} | \[ $interior_re \] ) )+ 
  }x;
  state $slice_re = qr{ 
    (?: \{ $interior_re \} | \[ $interior_re \] ) 
  }x;
  state $variable_re = qr/   # sigl could be $ or @
      \#?\w+(?:::\w+)*   # @name $pkg::name $#name $1
    | \#?\$\w+(?:::\w+)* # $$ref $#$ref
    | \^\w               # $^N   (control-character 'punctuation' variable)
    | \#?[^\w\s\{\$]     # $#- $^ $? (regular 'punctuation' variable)
    | \{ $interior_re \} # ${ref expression} or ${^SPECIALNAME}
  /x;

  my @actions;
  {
    # Localize $_ while running regexprs to preserve the caller's $1 etc.
    # N.B. The evals are executed after $_ is restored.
    local $_ = join "", map {defined($_) ? $_ : '<undef arg>'} $self->Values();
    while (1) {
      if (
        # \G does not work with (?|...) in Perl 5.12.4
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
        # Don't show initial $ if it's a non-special scalar variable name
        my $prefix = ($display_mode 
                       ? (($sigl eq '$' && $rhs =~ /^[A-Za-z]/) ? "" : $sigl).$rhs."="
                       : ""
                     );
        if ($sigl eq '$') {
          $self->Reset()->Values([$items[0]])->{VisType} = 'v';
        }
        elsif ($sigl eq '@') {
          $self->Reset()->Values([\@items])->{VisType} = 'a';
        }
        elsif ($sigl eq '%') {
          my %hash = @items;
          $self->Reset()->Values([\%hash])->{VisType} = 'a';
        }
        else { die "bug" }
        $result .= $prefix . $self->Dump;
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
        my $value = eval qq{<<"ViSEoF"
$text
ViSEoF
};
        Carp::confess "Unexpected eval faulre ($text): $@" if $@;
        $value =~ s/\n\z//;  # don't use chomp(): $/ may have been changed
        $result .= $value;  # plain text
      }
    } else {
      die "bug";
    }
    print "### After act '$act' : result=«${result}»\n" if $Debug;
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

  print Vis->snew('href=$href\n')->Useqq(0)->Dump;
  print Vis->dnew('$href\n')->Dump;
  print "href=", Vis->vnew($href)->Maxlength(110)->Dump, "\n";
  print "ARGV=", Vis->anew(@ARGV)->Dump, "\n";

  print Dumper($href);                      # Data::Dumper API
  print Vis->new([$href],['$href'])->Dump;  # Data::Dumper API

  use Vis qw(u);
  $value = undef;
  print "Value is ",u($value),"\n";

  foreach ($ENV{HOME}, "/dir/safe", "Uck!", 
           "My Documents", "Qu'ote", 'Qu"ote') 
  {
    system( "set -x; /bin/ls -ld ".qsh($_) );
  }

=head1 DESCRIPTION

The Vis package provides additional interfaces to Data::Dumper
which may be more convenient for error/debug messages 
(and a few related utilities).
A condensed format is used for aggregate data.

=over

=item

A final newline is not automatically included when using the new interfaces.

=item

Multiple array and hash members are shown on the same line, subject to
a maximum line length given by $Vis::Maxwidth or the Maxwidth() method
(defaults to terminal width or 80).

The first print statement above produces the following output:

  href={
    complicated => [ "lengthy", "stuff", 1, 2, 3, 4, 5, 6, 7,
      8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
    ]
  }


=item

Different Data::Dumper defaults are used:

=over

B<Quotekeys(0)> -- Don't quote hash keys unless necessary.

B<Terse(1)> -- Don't assign to variables, just show the data.

B<Sortkeys(smart sorting)>

Numeric "components" in hash keys are auto-detected.
For example: "A.20_999" "A.100_9" "A.100_80" "B" are shown in that order.

=back
  
=back

Vis is a subclass of Data::Dumper and is a drop-in replacement.  The old
APIs are all available but produce condensed output formatting.
A final newline in included when using the old APIs.

=head2 svis 'string to be interpolated',...

Variables and escapes in the string(s) are interpolated as 
in Perl double-quotish strings except that expression values 
are formatted using C<vis()> or C<avis()> (for $ or @ expressions, 
respectively).  
String values appear 'quoted' and complex data structures are shown in full.
Multiple arguments are concatenated.

The input string(s) should written SINGLE QUOTED so Perl will not 
interpolate them before passing to svis().

In addition to all the forms recognized by Perl in double-quotish strings,
C<svis> interpolates C<< %name >> into S<<< C<< (key => value ...) >> >>>

=head2 dvis 'string to be interpolated',...

('d' is for 'debug display').  C<dvis> is identical to C<svis>
except that interpolated expressions are prefixed by
the name of the variable (or the expression).  
For example, S<dvis('$foo $i $ary[$i] @ary %hash\n')> 
yields S<< "foo=<value> i=<value> ary[i]=<value> @ary=(<value>,...) %hash=(<key> => <value>,...)<newline>". >>

=head2 vis $item, ... 

Format arbitrary Perl data for printing, without a final newline.

Multiple arguments are each formatted separately,
separated by newlines.

=head2 avis @array

C<avis> formats an array or list in parenthesis: C<(arg1,arg2,arg3,...)>.
This allows @arrays to be shown without taking a reference.

=head2 OO interfaces

OO interfaces allow setting Configuration options on a case-by-case basis.

B<< Vis->snew >>, B<< Vis->dnew >>, B<< Vis->vnew >> and B<< Vis->anew >>
are constructors corresponding to the functions
B<svis>, B<dvis>, B<vis> and B<avis>, respectively.  See SYNOPSIS above. 

Additionally, B<< Vis->new >> provides the same API as Data::Dumper.

=head2 Configuration Variables or Methods

=over 4

=item $Vis::Maxwidth  I<or>  I<$OBJ>->Maxwidth(I<[NEWVAL]>) 

Sets or gets the maximum number of characters for formatted lines.
If Maxwidth=0 produces output similar to Data::Dumper 
(but still sans final newline).

=back

The following Configuration Methods have the same meaning as in Data::Dumper

=over 4

=item $Vis::Useqq  I<or>  I<$OBJ>->Useqq(I<[NEWVAL]>) 

=item $Vis::Quotekeys  I<or>  I<$OBJ>->Quotekeys(I<[NEWVAL]>) 

=item $Vis::Sortkeys  I<or>  I<$OBJ>->Sortkeys(I<[NEWVAL]>) 

=item $Vis::Terse  I<or>  I<$OBJ>->Terse(I<[NEWVAL]>) 

=item $Vis::Indent  I<or>  I<$OBJ>->Indent(I<[NEWVAL]>) 

=back

Data::Dumper Configuration variables (or methods) other than the above
may also be set.

Changes to global variables should generally be localized, e.g.

 { local $Vis::Maxwidth = 100;
   ...code which uses vis() or avis()...
 }

=head2 u $data, ...

The argument(s) are returned unchanged, except that undefined argument(s)
are replaced by the string "undef".  Refs are not stringified.
C<u()> is not exported by default.

=head2 qsh 

=head2 qsh $data, ...

The data items ($_ by default) are 'quoted' if necessary for parsing 
by /bin/sh (note that /bin/sh quoting rules differ from Perl's).  
Multiple items are concatenated, separated by spaces.

Strings which contain only "safe" characters are returned as-is, 
omitting the 'quotes'.  

References are formatted as with C<vis()> and the resulting string quoted.
Undefined values appear as C<undef> without quotes.

=head2 forceqsh $data

The argument is 'quoted' for /bin/sh if even if quotes are not necessary.

Unlike C<qsh>, C<forceqsh> requires exactly one argument.

=head1 PERFORMANCE

Vis calls Data::Dumper and then condenses the output. 
C<svis> and C<dvis> must also parse the strings to be interpolated.  
For most puposes performance is of no concern.

Single-quote style (C<Useqq(0)>) runs faster because 
the underlying Data::Dumper implementation uses XS (C code).

For high-volume applications, such as to serialize a large data set,
C<< Data::Dumper->new() >> may be called directly.

=head1 SEE ALSO

Data::Dumper

=head1 AUTHOR

Jim Avera (james_avera AT yahoo daht komm)

=cut

#!/usr/bin/perl
# Tester for module Vis.  TODO: Convert to CPAN module-test setup
use utf8; use strict; use warnings;
use feature qw(state switch);
use Carp;
use English qw( -no_match_vars );;
#use lib "$ENV{HOME}/lib/perl";
use Vis;

sub tf($) { $_[0] ? "true" : "false" }
sub u($)  { defined $_[0] ? $_[0] : "undef" }

binmode STDOUT, 'utf8';
binmode STDERR, 'utf8';
select STDERR; $|=1; select STDOUT; $|=1;

# ---------- Check stuff other than formatting or interpolation --------

for my $varname (qw(PREMATCH MATCH POSTMATCH)) {
  $_ = "test"; /(\w+)/;
  no strict 'vars';
  die "Vis imports high-overhead English ($varname)"
    if eval "defined \$Vis::$varname";
  die "EVAL ERR: $@ " if $@;
}

my $byte_str = join "",map { chr $_ } 10..30;
my $unicode_str = join "",map {
        eval sprintf "\" \\N{U+%04X}\"",$_} (0x263A..0x2650);

die "Expected initial Vis::Maxwidth to be undef" if defined $Vis::Maxwidth;
if (Vis::_unix_compatible_os()) {
  # We can't test auto-detection of terminal size because that will vary.
  # However we can test the default width when there is no tty.
  my $pid = fork();
  if ($pid==0) {
    require POSIX;
    die "bug" unless POSIX::setsid()==$$;
    POSIX::close 0;
    POSIX::close 1;
    POSIX::close 2;
    vis(123); 
    exit( $Vis::Maxwidth==80 ? 0 : 1 );
  }
  waitpid($pid,0);
  die "Vis::Maxwidth did not default to 80 with no tty" unless $? == 0;
}
$Vis::Maxwidth = 72;

my $undef_as_false = undef;
if (! ref Vis->new([1])->Useqq(undef)) {
  warn "WARNING: Your Data::Dumper has not been fixed to accept undef boolean args.\n";
  $undef_as_false = 0;
}

print "         unicode_str:$unicode_str\n";
{ my $s = Vis->vnew($unicode_str)->Useqq('utf8')->Dump;
  $s =~ s/^"(.*)"\z/$1/ or die "bug";
  print "Vis with Useqq(utf8):$s\n";
  warn "WARNING: Useqq('utf8') is broken in your Data::Dumper.\n"
    unless $s eq $unicode_str;
}
foreach ( ['Quotekeys',0,1],
          ['Sortkeys',0,1,sub{return sort keys %{shift @_}}],
          ['Terse',0,1],
          ['Indent',0,1,2,3],
          ['Useqq',0,1,'utf8'],
          ['Maxwidth',0,1,80,9999],
          ['Debug',undef,0,1],
        )
{
  my ($name, @values) = @$_;
  foreach my $value (@values) {
    foreach my $ctor (qw(vnew anew snew dnew)) {
      my $v = eval "{ local \$Vis::$name = \$value;
                      Vis->\$ctor([\"test\"])->$name();
                    }";
      die "bug:$@ " if $@;
      die "Vis::$name value is not preserved by Vis->$ctor\n",
          "(Set \$Vis::$name = ",u($value)," but $name() returned ",u($v),")\n"
       unless (! defined $v && ! defined $value) || ($v eq $value);
    }
  }
}

# ---------- Check formatting or interpolation --------

sub check($$$) {
  my ($code, $expected, $actual) = @_;
  confess "BUG(EVAL ERR): $@" if $@;
  $actual //= '<undef>';
  local $_;
  #print $actual;{ local $_;print "<no newline>\n" unless $actual =~ /\n\z/s; }
  confess "\nTEST FAILED: $code\n"
         ."Expected:\n${expected}«end»\n"
         ."Got:\n${actual}«end»\n"
    unless $actual eq $expected;
}

@ARGV = ('fake','argv');
$. = 1234;
$ENV{EnvVar} = "Test EnvVar Value";

my %toplex_h = ("" => "Emp", A=>111,"B B"=>222,C=>{d=>888,e=>999},D=>{});
my @toplex_a = (0,1,"C",\%toplex_h,[],[0..9]);
my $toplex_ar = \@toplex_a;
my $toplex_hr = \%toplex_h;

our %global_h = %toplex_h;
our @global_a = @toplex_a;
our $global_ar = \@global_a;
our $global_hr = \%global_h;

our %maskedglobal_h = (key => "should never be seen");
our @maskedglobal_a = ("should never be seen");
our $maskedglobal_ar = \@maskedglobal_a;
our $maskedglobal_hr = \%maskedglobal_h;

our %localized_h = (key => "should never be seen");
our @localized_a = ("should never be seen");
our $localized_ar = \@localized_a;
our $localized_hr = \%localized_h;

package A::B::C;
our %ABC_h = %main::global_h;
our @ABC_a = @main::global_a;
our $ABC_ar = \@ABC_a;
our $ABC_hr = \%ABC_h;

package main;

$_ = "GroupA.GroupB";
/(.*)\W(.*)/p or die "nomatch"; # set $1 and $2

{ my $code = 'vis($_)'; check $code, "\"${_}\"", eval $code; }
{ my $code = 'avis($_,1,2,3)'; check $code, "(\"${_}\", 1, 2, 3)", eval $code; }
{ my $code = 'avis(@_)'; check $code, '()', eval $code; }
{ my $code = 'svis(q($_ con),q(caten),q(ated\n))';
  check $code, "\"${_}\" concatenated\n", eval $code;
}
{ my $code = 'dvis(q($_ con),q(caten),q(ated\n))';
  check $code, "\$_=\"${_}\" concatenated\n", eval $code;
}
{ my $code = 'avis(undef)'; check $code, "(undef)", eval $code; }
{ my $code = 'vis(undef)'; check $code, "undef", eval $code; }
{ my $code = 'svis("foo",undef)'; check $code, "foo<undef arg>", eval $code; }
{ my $code = 'dvis("foo",undef)'; check $code, "foo<undef arg>", eval $code; }

sub doquoting($$) {
  my ($input, $useqq) = @_;
  my $quoted = $input;
  if ($useqq) {
    $quoted =~ s/([\$\@"\\])/\\$1/gs;
    $quoted =~ s/\n/\\n/gs;
    $quoted =~ s/\t/\\t/gs;
    $quoted = "\"${quoted}\"";
  } else {
    $quoted =~ s/([\\'])/\\$1/gs;
    $quoted = "'${quoted}'";
  }
  return $quoted;
}

sub f {
  my $zero = 0;
  my $one = 1;
  my $two = 2;
  my $EnvVarName = 'EnvVar';
  my $flex = 'Lexical in sub f';
  my $flex_ref = \$flex;
  my $ARGV_ref = \@ARGV;
  eval { die "FAKE DEATH\n" };  # set $@
  my %sublex_h = %toplex_h;
  my @sublex_a = @toplex_a;
  my $sublex_ar = \@sublex_a;
  my $sublex_hr = \%sublex_h;
  our %subglobal_h = %toplex_h;
  our @subglobal_a = @toplex_a;
  our $subglobal_ar = \@subglobal_a;
  our $subglobal_hr = \%subglobal_h;
  our %maskedglobal_h = %toplex_h;
  our @maskedglobal_a = @toplex_a;
  our $maskedglobal_ar = \@maskedglobal_a;
  our $maskedglobal_hr = \%maskedglobal_h;
  local %localized_h = %toplex_h;
  local @localized_a = @toplex_a;
  local $localized_ar = \@toplex_a;
  local $localized_hr = \%localized_h;

  for my $test (
    [ q(aaa\\\\bbb), q(aaa\bbb) ],
    [ q($unicode_str\n), qq(unicode_str=\" \\x{263a} \\x{263b} \\x{263c} \\x{263d} \\x{263e} \\x{263f} \\x{2640} \\x{2641} \\x{2642} \\x{2643} \\x{2644} \\x{2645} \\x{2646} \\x{2647} \\x{2648} \\x{2649} \\x{264a} \\x{264b} \\x{264c} \\x{264d} \\x{264e} \\x{264f} \\x{2650}\"\n) ],
    [ q($byte_str\n), qq(byte_str=\"\\n\\13\\f\\r\\16\\17\\20\\21\\22\\23\\24\\25\\26\\27\\30\\31\\32\\e\\34\\35\\36\"\n) ],
    [ q($_\n), qq(\$_=\"GroupA.GroupB\"\n) ],
    [ q($flex\n), qq(flex=\"Lexical in sub f\"\n) ],
    [ q($$flex_ref\n), qq(\$\$flex_ref=\"Lexical in sub f\"\n) ],
    [ q($.\n), qq(\$.=1234\n) ],
    [ q($NR\n), qq(NR=1234\n) ],
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
    [ q(@+ $#+\n), qq(\@+=(13, 6, 13) \$#+=2\n) ],
    [ q(@- $#-\n), qq(\@-=(0, 0, 7) \$#-=2\n) ],
    [ q($;\n), qq(\$;=\"\\34\"\n) ],
    [ q($1\n), qq(\$1=\"GroupA\"\n) ],
    [ q($2\n), qq(\$2=\"GroupB\"\n) ],
    [ q($3\n), qq(\$3=undef\n) ],
    [ q(${^MATCH}\n), qq(\${^MATCH}=\"GroupA.GroupB\"\n) ],
    [ q(@ARGV\n), qq(\@ARGV=(\"fake\", \"argv\")\n) ],
    [ q($ENV{EnvVar}\n), qq(ENV{EnvVar}=\"Test EnvVar Value\"\n) ],
    [ q($ENV{$EnvVarName}\n), qq(ENV{\$EnvVarName}=\"Test EnvVar Value\"\n) ],
    [ q($@\n), qq(\$\@=\"FAKE DEATH\\n\"\n) ],
    [ q(@_\n), qq(\@_=( 42, [ 0, 1, "C", { "" => "Emp", A => 111, "B B" => 222,\n      C => {d => 888, e => 999}, D => {}\n    },\n    [], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n  ]\n)\n) ],
    [ q($#_\n), qq(\$#_=1\n) ],
    map({
      my ($LQ,$RQ) = (/^(.*)(.)$/) or die "bug";
      map({ 
        my $name = $_;
        map({ 
          my ($dollar, $r) = @$_;
          my $dolname_scalar = ($dollar ? "\$$dollar" : "").$name;
          [ qq(%${dollar}${name}_h${r}\\n),
            qq(\%${dollar}${name}_h${r}=( "" => "Emp", A => 111, "B B" => 222,\n  C => {d => 888, e => 999}, D => {}\n)\n)
          ],
          [ qq(\@${dollar}${name}_a${r}\\n),
            qq(\@${dollar}${name}_a${r}=( 0, 1, "C", { "" => "Emp", A => 111, "B B" => 222,\n    C => {d => 888, e => 999}, D => {}\n  },\n  [], [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n)\n)
          ],
          [ qq(\$#${dollar}${name}_a${r}),    qq(\$#${dollar}${name}_a${r}=5)   ],
          [ qq(\$#${dollar}${name}_a${r}\\n), qq(\$#${dollar}${name}_a${r}=5\n) ],
          [ qq(\$${dollar}${name}_a${r}[3]{C}{e}\\n),
            qq(${dolname_scalar}_a${r}[3]{C}{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]{C}{e}\\n),
            qq(${dolname_scalar}_a${r}[3]{C}{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]->{A}\\n),
            qq(${dolname_scalar}_a${r}[3]->{A}=111\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]->{$LQ$RQ}\\n),
            qq(${dolname_scalar}_a${r}[3]->{$LQ$RQ}="Emp"\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]{C}->{e}\\n),
            qq(${dolname_scalar}_a${r}[3]{C}->{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}[3]->{C}->{e}\\n),
            qq(${dolname_scalar}_a${r}[3]->{C}->{e}=999\n)
          ],
          [ qq(\@${dollar}${name}_a${r}[\$zero,\$one]\\n),
            qq(\@${dollar}${name}_a${r}[\$zero,\$one]=(0, 1)\n)
          ],
          [ qq(\@${dollar}${name}_h${r}{${LQ}A${RQ},${LQ}B B${RQ}}\\n),
            qq(\@${dollar}${name}_h${r}{${LQ}A${RQ},${LQ}B B${RQ}}=(111, 222)\n)
          ],
        } (['',''], ['$','r'])
        ), #map [$dollar,$r]
        map({ 
          my ($dollar, $r, $arrow) = @$_;
          my $dolname_scalar = ($dollar ? "\$$dollar" : "").$name;
          [ qq(\$${dollar}${name}_h${r}${arrow}{\$${name}_a[\$two]}{e}\\n),
            qq(${dolname_scalar}_h${r}${arrow}{\$${name}_a[\$two]}{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}${arrow}[3]{C}{e}\\n),
            qq(${dolname_scalar}_a${r}${arrow}[3]{C}{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_a${r}${arrow}[3]{C}->{e}\\n),
            qq(${dolname_scalar}_a${r}${arrow}[3]{C}->{e}=999\n)
          ],
          [ qq(\$${dollar}${name}_h${r}${arrow}{A}\\n),
            qq(${dolname_scalar}_h${r}${arrow}{A}=111\n)
          ],
        } (['$','r',''], ['','r','->'])
        ), #map [$dollar,$r,$arrow]
        }
        qw(sublex toplex global subglobal maskedglobal localized
           A::B::C::ABC)
      ), #map $name
      } ('""', "''")
    ), #map ($LQ,$RQ)
  )
  {
    my ($dvis_input, $expected) = @$test;

    #print "### $dvis_input\n";
    { local $@;
      my $ev = eval { "$dvis_input" };
      die "Bad test string:$dvis_input\nPerl can't interpolate it: $@" if $@;
    }

    # For some reason we can't catch exceptions from inside package DB (dvis)
    # (undef is returned but $@ is not set!)
    my $actual = dvis $dvis_input;
    confess "\ndvis test failed: input «${dvis_input}»\n"
         ."Expected:\n${expected}«end»\n"
         ."Got:\n${actual}«end»\n"
    unless $expected eq $actual;

    # Check Useqq 
    for my $useqq (0, 1) {
      my $input = $expected.$dvis_input.'qqq@_(\(\))){\{\}\""'."'"; # gnarly
      my $exp = doquoting($input, $useqq);
      my $act =  Vis->vnew($input)->Useqq($useqq)->Dump;
      die "\n\nUseqq ",u($useqq)," bug:\n"
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
&g(42,$toplex_ar);
print "Tests passed.\n";
exit 0;
# End Tester

