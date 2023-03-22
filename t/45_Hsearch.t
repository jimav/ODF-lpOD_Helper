#!/usr/bin/perl
use FindBin qw($Bin);
use lib $Bin;
#use t_Setup qw/bug :silent/; # strict, warnings, Test::More, Carp etc.
use t_Setup qw/bug /; # strict, warnings, Test::More, Carp etc.

use Data::Dumper::Interp qw/visnew ivis dvis vis hvis avis u/;
$Data::Dumper::Interp::Useqq = 'unicode'; # omit 'controlpic' to get \t etc.

use ODF::lpOD;
use ODF::lpOD_Helper qw/:DEFAULT :chars fmt_node fmt_tree fmt_match/;
use File::Basename qw/basename dirname/;
use List::Util qw/reduce min max first/;

no autovivification qw/fetch store exists delete/;
no autovivification warn => qw/fetch store exists delete/;
use warnings FATAL => 'all';

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
      $err .= "\n".fmt_match($m);
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


my $input_path = "$Bin/../tlib/Skel.odt";
my $doc = odf_get_document($input_path);
my $body = $doc->get_body;
 

check_match( $body->Hsearch(qr/Front/s),
             "Hsearch match start of first paragraph (regex)",
             { num_segs=>1, offset=>0, end=>5, voffset=>0, vend=>5 } );
      
check_match( $body->Hsearch(qr/^Front/s),
             "Hsearch ^anchored match start of first paragraph",
             { num_segs=>1, offset=>0, end=>5, voffset=>0, vend=>5 } );
      
check_nomatch( $body->Hsearch(qr/^Stuff/s),
             "Hsearch ^anchored match middle of first seg fails");

check_match( my $m1 = $body->Hsearch('Stuff'),
             "Hsearch match middle of of first segment (string)",
             { num_segs=>1, offset=>6, end=>11, voffset=>6, vend=>11 } );

check_match( $body->Hsearch(qr/Front Stuff /s),
             "Hsearch match entire first segment",
             { num_segs=>1, offset=>0, end=>12, voffset=>0, vend=>12 } );
      
check_match( $body->Hsearch(qr/Front Stuff o/s),
             "Hsearch match first segment + start of second",
             { num_segs=>2, offset=>0, end=>1, voffset=>0, vend=>13 } );
      
check_match( $body->Hsearch(qr/utside/s),
             "Hsearch match middle of second segment",
             { num_segs=>1, offset=>1, end=>7, voffset=>13, vend=>19 } );
      
check_match( $body->Hsearch(qr/outside the 2-column Section/s),
             "Hsearch match entire second segment",
             { num_segs=>1, offset=>0, end=>28, voffset=>12, vend=>40 } );
      
check_nomatch( $body->Hsearch(qr/^outside/s, debug => 0),
             "Hsearch ^anchored match start of second seg fails");
      
check_match( $body->Hsearch('o'),
             "Hsearch first 'o' (string)",
             { num_segs=>1, offset=>2, end=>3, voffset=>2, vend=>3 } );
check_match( $body->Hsearch(qr/o/),
             "Hsearch first 'o' (regex)",
             { num_segs=>1, offset=>2, end=>3, voffset=>2, vend=>3 } );
      
check_match( $body->Hsearch(qr/o.*o/),
             "Hsearch longest match across mult segs ('o.*o')",
             { num_segs=>2, offset=>2, end=>27, voffset=>2, vend=>39 } );
      
check_match( $body->Hsearch(qr/^Lorem.*another/s, debug => 0),
             "Hsearch match past one newline",
             { num_segs=>11, offset=>0, end=>7, voffset=>76, vend=>168 } );
      
check_match( $body->Hsearch(qr/^Lorem.*laborum\./s),
             "Hsearch match past multiple newlines",
             { num_segs=>33, offset=>0, end=>305, voffset=>76, vend=>708 } );
      

#####################

check_match( $body->Hsearch(qr/5 consecutive-spaces:/),
             "Hsearch until just b4 space b4 multi-space",
             { num_segs=>2, offset=>0, end=>19, voffset=>91, vend=>112 } );
check_match( $body->Hsearch(qr/5 consecutive-spaces: /),
             "Hsearch until just b4 multi-space",
             { num_segs=>2, offset=>0, end=>20, voffset=>91, vend=>113 } );
for my $n (1..6) {
  my $regex_src = '5 consecutive-spaces: '.(" " x $n);
  if ($n < 5) {
    check_match( $body->Hsearch(qr/$regex_src/),
                 "Hsearch including $n of multi-space",
                 { num_segs=>3, offset=>0, end=>$n, voffset=>91, vend=>113+$n } 
               );
  } else {
    check_nomatch( $body->Hsearch(qr/$regex_src/),
                   "Hsearch beyond end of multi-space ($n total)" );
  }
}

#####################

{
  my $tab_match;
  check_match( $tab_match = $body->Hsearch(qr/\t/),
               "Hsearch for tab alone",
               { match => "\t", offset=>0, end=>1 } );

  check_match( $body->Hsearch(qr/tab here:\t/),
               "Hsearch for stuff+tab",
               { match => "tab here:\t", offset=>0, end=>1,
                 voffset => $tab_match->{voffset}-9,
                 vend    => $tab_match->{voffset}+1,
               } );

  check_match( $body->Hsearch(qr/tab here:\t:there/),
               "Hsearch for stuff+tab+stuff",
               { match => "tab here:\t:there", offset=>0, end=>6,
                 voffset => $tab_match->{voffset}-9,
                 vend    => $tab_match->{voffset}+1+6,
               } );

  check_match( $body->Hsearch(qr/\t:there/),
               "Hsearch for tab+stuff",
               { match => "\t:there", offset=>0, end=>6,
                 voffset => $tab_match->{voffset},
                 vend    => $tab_match->{voffset}+1+6,
               } );
}

#####################
if (1) {
  note "=== DUMP OF ENTIRE TEXT ===\n";
  my @paragraphs = $body->descendants_or_self(qr/text:(p|h)/);
  my $off = 0;
  foreach my $para (@paragraphs) {
    my $msg = "";
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
        $msg .= sprintf " %d:", $off;
        $msg .= $pilcrow; $pilcrow = "";
        $msg .= "[$s]";
        $msg .= "\n" if substr($s,-2) eq "\\n";
        $off += $len;
      }
    }
    $msg .= "(no newline)\n" unless substr($msg,-2) eq "\\n";
    note $pilcrow.$msg;
  }
  note "=== (END DUMP) ===\n";
}
#####################

check_match( $body->Hsearch(qr/.*Unicode characters.*/s),
             "Hsearch match variety para",
             { offset=>0, 
               voffset=>708,
               match => 'This «Paragraph» has ☺Unicode characters and bold and italic and underlined and larger text.'
             } );
      
#####################
{ my sub smashwhite($) {
    local $_ = shift;
    s/[ \t\n\N{NO-BREAK SPACE}]+/ /gs;
    $_
  }

  (my $input_txt = $input_path) =~ s/\.o..$/.txt/ or bug;
  #note "> Reading $input_txt ...";
  my $itxt;
  { open my $fh, "<:encoding(UTF-8)", $input_txt or die "$! ";
    local $/; # slurp
    $itxt = <$fh>;
    close $fh or die "Error reading txt:$!";
  }
  my $sitxt = smashwhite($itxt);

  for my $maxlen (1..1000) {
    my $text = "";
    my $nchunks = 0;
    my @paragraphs = $body->descendants_or_self(qr/text:(p|h)/);
    for my $px (0..$#paragraphs) {
      my $para = $paragraphs[$px];
      my $offset = 0;
      #say "[$px] para=$para";
      while (my $m = $para->Hsearch(qr/.{1,${maxlen}}/s, offset => $offset, debug=>0)) {
        $text .= $m->{match};
        $offset = $m->{vend};
        #say "GOT MATCH:", fmt_match($m);
        bug unless $m->{paragraph} == $para;
        $nchunks++;
      }
      $text .= "\n"; # inter-paragraph separator
    }
    my $stext = smashwhite($text);
    my $ok = ($sitxt eq $stext);
    if (! $ok) {
      note "> doc contains ", length($text), " virtual characters";
      note "           and ", length($stext), " of smashed text";
      note "> $input_txt contains ", length($sitxt), " of smashed text";
  
      foreach ([$stext, "smashed text from ODF", "/tmp/j.stext"],
               [$sitxt, "smashed text from ".basename($input_txt), "/tmp/j.sitxt"],
              ) {
        my ($data, $title, $path) = @$_;
        note "> Dumping $title > $path";
        open my $fh, ">:encoding(UTF-8)", $path or die "$path : $!";
        print $fh $data;
        print $fh "\n";
        close $fh or die "Error writing $path";
      }
      bug "Match of Total text FAILED: ($nchunks chunks of max $maxlen chars)";
    }
    last if length($text) < $maxlen;
  }
  ok_with_lineno(1, "Total text matches ".basename($input_txt)." after smashing (variations)");
}
#####################

check_match( $body->Hsearch(qr//s),
             "Hsearch with qr//s", { offset=>0, end=>0, voffset=>0, vend=>0 } );

check_match( $body->Hsearch(qr//),
             "Hsearch with qr//", { offset=>0, end=>0, voffset=>0, vend=>0 } );

for (my $offset=4; $offset < 50; $offset++) {
  check_match( $body->Hsearch(qr//s, offset => $offset, debug => 0),
               "Hsearch with qr//s, offset => $offset", 
               { voffset=>$offset, vend=>$offset },
               1 
             ); #ok_only_if_failed

  check_match( $body->Hsearch(qr//, offset => $offset, debug => 0),
               "Hsearch with qr//, offset => $offset", 
               { voffset=>$offset, vend=>$offset },
               1 
             ); #ok_only_if_failed
}
ok_with_lineno(1, "Hsearch with qr//s or qr// and many offsets");

#####################
{ #Multi-match ADDR{1,2,3} in different table cells 
  my @m = $body->Hsearch(qr/ADDR./, multi => 1, debug => 0);
  check_match($m[0], "multi-match finding ADDR1",
              { voffset=>856, match => 'ADDR1' });
  check_match($m[1], "multi-match finding ADDR2",
              { voffset=>865, match => 'ADDR2' });
  check_match($m[2], "multi-match finding ADDR3",
              { voffset=>874, match => 'ADDR3' });
  ok_with_lineno(@m==3);
}
{ #Multi-match in same paragraph
  # Using offset to start with the last table cell containing **ADDR3**
  my $off = 872;
  my @m = $body->Hsearch(qr/(?:AD|R3|.)/s, multi => 1, offset => $off);
  
  my $ix = 0;
  foreach my $exp (qw/* * AD D R3 * */) {
    my $m = $m[$ix++] // die "Too few matches";
    check_match($m, "multi-match in same para ($exp at $off)",
                { match => $exp, 
                  voffset => $off, vend => $off+length($exp),
                });
    $off += length($exp);
  }
  ok_with_lineno(@m==7, "correct number of matches"); # no extras
  say "##TT99:", avis(map{ $_->{match} } @m);
  my $totstr = reduce { $a . $b } (map{ $_->{match} } @m);
  bug unless defined $totstr;
  like_with_lineno($totstr, qr/^\*\*ADDR3\*\*$/, "Multi-match in same para");
}
#####################

#{ my $m = $body->Hsearch("Front Stuff");
#  say fmt_match($m);
#  say fmt_tree($m->{paragraph});
#  die "tex"
#}

done_testing();
