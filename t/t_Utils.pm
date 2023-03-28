# License: Public Domain or CC0
# See https://creativecommons.org/publicdomain/zero/1.0/
# The author, Jim Avera (jim.avera at gmail) has waived all copyright and 
# related or neighboring rights to the content of this file.  
# Attribution is requested but is not required.

use strict; use warnings  FATAL => 'all'; use feature qw/say/;
use feature qw/say state current_sub/;
use utf8;

package t_Utils;

require Exporter;
use parent 'Exporter';

our @EXPORT = qw/check_match check_nomatch ok_with_lineno like_with_lineno 
                 bug oops mydump/;

use Test::More; ##??? can this be used even in non-tests like dumpskel.pm  ???

use ODF::lpOD_Helper qw/:DEFAULT :chars fmt_node fmt_tree fmt_match/;
use Carp;

sub bug(@) { @_=("BUG:",@_); goto &Carp::confess }
sub oops(@) { @_=("Internal oops! ",@_); goto &Carp::confess }

sub _check_match($$$) {
  my ($m, $test_label, $exp) = @_;
  my $err;
  if (!$m) {
    $err = "match FAILED (undef result)\n"
  } else {
    {
      $err = "Key 'segments is' undef or missing in match result:\n", last
        unless defined $m->{segments};
      foreach my $key (keys %$exp) {
        confess "test bug" unless defined $exp->{$key};
        if ($key eq "num_segs") {
          $err = "Expecting $exp->{num_segs} segments in match result:\n", last
            unless @{ $m->{segments} } == $exp->{num_segs};
        }
        else {
          $err = "Key '$key' undef or missing in match result:\n", last
            unless defined($m->{$key});
          $err = "Expecting $key = $exp->{$key} (not $m->{$key}) in match result:\n", last
            unless $m->{$key} eq $exp->{$key};
        }
      }
    }
    if ($err) {
      $err .= fmt_match($m)."\n";
      #$err .= "\nparagraph:\n".fmt_tree($m->{paragraph}) if $m->{paragraph};
    }
  } 
  $err
}
sub _check_nomatch($$) {
  my ($m, $test_label) = @_;
  my $err = 0;
  if ($m) {
    $err = "Expected no-match, but got:\n".fmt_match($m);
  }
  $err
}
sub ok_with_lineno($;$) {
  my ($isok, $test_label) = @_;
  my $lno = (caller)[2];
  $test_label = ($test_label//"") . " (line $lno)";
  @_ = ( $isok, $test_label );
  goto &Test::More::ok;  # show caller's line number
}
sub like_with_lineno($$;$) {
  my ($got, $exp, $test_label) = @_;
  my $lno = (caller)[2];
  $test_label = ($test_label//"") . " (line $lno)";
  @_ = ( $got, $exp, $test_label );
  goto &Test::More::like;  # show caller's line number
}
sub _check_end($$$) {
  my ($errmsg, $test_label, $ok_only_if_failed) = @_;
  return 
    if $ok_only_if_failed && !$errmsg;
  my $lno = (caller)[2];
  diag "**********\n${errmsg}***********\n" if $errmsg;
  @_ = ( !$errmsg, $test_label );
  goto &ok_with_lineno;
}
sub check_match($$$;$) {
  my (undef, $test_label, undef, $ok_only_if_failed) = @_;
  @_ = ( &_check_match, $test_label, $ok_only_if_failed );
  goto &_check_end;
}
sub check_nomatch($$;$) {
  my (undef, $test_label, $ok_only_if_failed) = @_;
  @_ = ( &_check_nomatch, $test_label, $ok_only_if_failed );
  goto &_check_end;
}

################################ mydump alternatives ###################
sub mydump_old($;@) {
  my $top = shift;
  my $msg = "";
  my $off = 0;
  my @paragraphs = $top->descendants_or_self(qr/text:(p|h)/);
  foreach my $para (@paragraphs) {
    my $pmsg = "";
    my $pilcrow = "¶";
    foreach my $seg ($para->descendants_or_self(
                       qr/^(#PCDATA|text:tab|text:line-break|text:s)$/)) {
      #local $_ = $seg->get_text();
      local $_ = ODF::lpOD_Helper::__element2vtext($seg);
      while (/\G[^\n]*\n/gsc || /\G[^\n]+/gsc) {
        my $len = length($&);
        my $s = $&;
        $s =~ s/\t/\\t/g;
        $s =~ s/\n/\\n/g;
        $pmsg .= sprintf " %d:", $off;
        $pmsg .= $pilcrow; $pilcrow = "";
        $pmsg .= "[$s]";
        $pmsg .= "\n" if substr($s,-2) eq "\\n";
        $off += $len;
      }
    }
    #$pmsg .= "(no newline)" unless substr($pmsg,-2) eq "\\n";
    if ($pmsg) {
      $msg .= $pilcrow.$pmsg."\n";
    } else {
      $msg .= $pilcrow."[empty paragraph]\n";
    }
  }
  $msg
}
sub mydump_new($;@) {
  my $top = shift;
  my $msg = "";
  my $off = 0;
  my $level_discount;
  my sub append($$) {
    my ($e, $start_flag) = @_;
    my sub wrap($) {
      my $e = shift;
      $msg .= "\n" if $msg;
      $msg .= ("  " x ($e->level - $level_discount));
    }
    my $tag = $e->get_tag;
    if ($e->is_elt) {  # a "real" element (not text)
      my $show;
      if ($tag =~ /^text:[ph]$/) {
        $start_flag = 'p';
        $show = 1;
        wrap($e);
      }
      elsif ($tag =~ /^(?:text:tab|text:line-break)$/) {
        $off += 1;
        $show = 1;
      }
      elsif ($tag eq 'text:s') {
        my $c = $e->att("text:c") // 1;
        $off += $c;
        $show = 1;
      }
      elsif ($tag eq "text:span") {
        $start_flag = 's';
        $show = 1;
        wrap($e);
      }
      else {
        oops if $tag =~ /CDATA/;
      }
      if ($show) {
        $msg .= "<".($e->gi);
        if (my @atts = $e->att_names) {
          $msg .= ' '.join(' ', map { 
                                      (my $name = $_) =~ s/^text://;
                                      $name.'="'.$e->att($_).'"' 
                                    } @atts);
        }
        $msg .= ">";
        $msg .= "\n" if $tag eq 'text:line-break';
      }
      for (my $c=$e->first_child; $c; $c=$c->next_sibling) {
        __SUB__->($c, $start_flag);
        $start_flag = '';
      }
    }
    elsif ($tag eq '#PCDATA') {
      local $_ = $e->get_text;
      wrap($e) if $start_flag;
      $msg .= " " if substr($msg,-1) ne "\n";
      $msg .= sprintf "%d:", $off;
      $off += length($_);
      $msg .= "¶" if $start_flag eq "p";
      s/\t/\\t/g; s/\n/\\n\n/g;
      $msg .= "[$_]";
    } else {
      $msg .= "\n<??? $tag ???>";
    }
  }
  $level_discount = $top->level;
  append($top, '');
  $msg
}
sub mydump_twig_dump($;@) {
  my $top = shift;
  $top->_dump();
}
sub mydump_twig_print($;@) {
  my $top = shift;
  my $pretty = $_[0] // "wrapped";
  my $doc = $top->document // die "doc oops";
  # Derived from ODF::lpOD::Document
  foreach my $part_name ($doc->loaded_xmlparts) {
    next unless $part_name;
    my $part = $doc->{$part_name}  or next;
    my $twig = $part->{twig} // die "twig oops";
    $twig->output_encoding(undef);
    $twig->set_pretty_print($pretty);
  }
  $top->sprint();
}
sub mydump_fmt_tree($;@) {
  my $top = shift;
  fmt_tree($top);
}

our $DumpKind = 5;  # the default

# Dump an object tree, printing via 'note' unless with option 'return_only'
sub mydump($;$@) {
  my ($obj, $opts, @other_args) = @_;
  croak "opts must be a {hashref}" unless ref($opts) eq "HASH";
  my $dump_kind = $opts->{dump_kind} // $DumpKind // oops;
  $dump_kind =~ s/^old.*/1/i;
  $dump_kind =~ s/^new.*/2/i;
  $dump_kind =~ s/^t\w*dump.*/3/i;  # e.g. twigdump
  $dump_kind =~ s/^t\w*print.*/5/i;

  my $result;
  if    ($dump_kind == 1) { $result = mydump_old(       $obj, @other_args) }
  elsif ($dump_kind == 2) { $result = mydump_new(       $obj, @other_args) }
  elsif ($dump_kind == 3) { $result = mydump_twig_dump( $obj, @other_args) }
  elsif ($dump_kind == 4) { $result = mydump_twig_print($obj, @other_args) }
  elsif ($dump_kind == 5) { $result = mydump_fmt_tree(  $obj, @other_args) }
  else { die "mydump: Unknown dump_kind '$dump_kind'\n"; }

  note($result) unless $opts->{return_only};
  $result;
}
###################################################

done_testing();
1; 
