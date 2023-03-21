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

no autovivification qw/fetch store exists delete/;
no autovivification warn => qw/fetch store exists delete/;
use warnings FATAL => 'all';

sub check_match($$$) {
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
      $err .= "\nparagraph:\n".fmt_tree($m->{paragraph}) if $m->{paragraph};
    }
  } 
  #say "DEBUG at ",join(":",(caller)[1,2]);
  diag "**********\n${err}***********\n" if $err;
  @_ = ( !$err, $test_label );
  goto &Test::More::ok;  # show caller's line number
}
sub check_nomatch($$) {
  my ($m, $test_label) = @_;
  my $result = 1;
  if ($m) {
    diag "Expected no-match, but got:\n",fmt_match($m);
    $result = 0;
  }
  @_ = ( $result, $test_label );
  goto &Test::More::ok;  # show caller's line number
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
      
check_nomatch( $body->Hsearch(qr/^outside/s),
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
      
check_match( $body->Hsearch(qr/^Lorem.*laborum\./s),
             "Hsearch match past newline",
             { num_segs=>33, offset=>0, end=>305, voffset=>0, vend=>632 } );
      
check_match( $body->Hsearch(qr/5 consecutive-spaces:/),
             "Hsearch until just before space b4 multi-space",
             { num_segs=>2, offset=>0, end=>19, voffset=>15, vend=>36 } );
check_match( $body->Hsearch(qr/5 consecutive-spaces: /),
             "Hsearch until just before multi-space",
             { num_segs=>2, offset=>0, end=>20, voffset=>15, vend=>37 } );
check_match( $body->Hsearch(qr/5 consecutive-spaces:  /),
             "Hsearch including 1 of multi-space",
             { num_segs=>3, offset=>0, end=>1, voffset=>15, vend=>38 } );
check_match( $body->Hsearch(qr/5 consecutive-spaces:   /, debug => 0),
             "Hsearch including 2 of multi-space",
             { num_segs=>3, offset=>0, end=>2, voffset=>15, vend=>39 } );
check_match( $body->Hsearch(qr/5 consecutive-spaces:     /),
             "Hsearch including 4 of multi-space",
             { num_segs=>3, offset=>0, end=>4, voffset=>15, vend=>41 } );

#####################
#
check_match( $body->Hsearch(qr/.*Unicode characters.*/s),
             "Hsearch match variety para",
             { offset=>0, voffset=>0,
               match => 'This «Paragraph» has ☺Unicode characters and bold and italic and underlined and larger text.'
             } );
      

#####################
sub smashwhite($) {
  local $_ = shift;
  s/[ \t\n\N{NO-BREAK SPACE}]+/ /gs;
  $_
}

#{ my $m = $body->Hsearch("Front Stuff");
#  say fmt_match($m);
#  say fmt_tree($m->{paragraph});
#  die "tex"
#}

{ (my $input_txt = $input_path) =~ s/\.o..$/.txt/ or bug;
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
  ok(1, "Total text matches ".basename($input_txt)." after smashing (variations)");
}

done_testing();
