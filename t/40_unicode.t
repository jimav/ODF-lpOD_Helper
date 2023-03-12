#!/usr/bin/perl
use FindBin qw($Bin);
use lib $Bin;
use t_Setup qw/bug :silent/; # strict, warnings, Test::More, Carp etc.

use Data::Dumper::Interp qw/visnew ivis dvis vis hvis avis u/;

use ODF::lpOD;
use ODF::lpOD_Helper qw/:DEFAULT fmt_node fmt_match fmt_tree/;
use Encode qw/encode decode/;

my $skel_path = "$Bin/Skel.odt";

my $doc = odf_get_document($skel_path);
my $body = $doc->get_body;


{ my $match = my_search($body, qr/This.*Paragraph.*has.*Unicode/) // bug;
  like($match->{match}, qr/This «Paragraph» has ☺Unicode/, "my_search with unicode");
}

my $smiley_char = "☺"; 
my $full_char_re = qr/This.*Para.*${smiley_char}.*characters./;

my $smiley_octets = encode("UTF-8", $smiley_char, Encode::LEAVE_SRC);
my $justsmiley_octet_re = qr/${smiley_octets}/;
my $full_octet_re = qr/This.*Para.*${smiley_octets}.*characters./;

{ # The paragraph is fragments, so we can not search for the whole text
  my $m0 = $body->search(qr/$full_octet_re/);
  ok(!defined($m0->{segment}), "native search can not span segments");
}
{ # But we can always search for a single character, whose octets will
  # be stored within a single segment
  my $m1 = $body->search(qr/${smiley_octets}/);
  ok($m1, "native search for just smiley");

  my $p = $m1->{segment}->get_parent_paragraph;
  my $p_text = $p->get_text();  # the full text of the para
  like($p_text, qr/$full_octet_re/, "octet search found whole paragraph");
}

# Now enable character params by default, no encode/decode needed
lpod_helper->enable_unicode_text();

{ # Now the octets are treated as single-byte characters and 
  my $octets_as_chars = join "", map{ chr(ord($_)) } split //,$smiley_octets;
  my $m2 = $body->search(qr/${octets_as_chars}/);
  ok(!defined($m2->{segment}), "octests treated as chars after enable_unicode_text");
}
{ # But we can not search for the Unicode character explicitly
  my $m3 = $body->search(qr/${smiley_char}/);
  ok($m3->{segment}, "native serach for Unicode after enable_unicode_text");
  my $p = $m3->{segment}->get_parent_paragraph;
  my $p_text = $p->get_text(); 
  like($p_text, qr/$full_char_re/);
}

done_testing();
