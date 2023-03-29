#!/usr/bin/perl
use FindBin qw($Bin);
use lib $Bin;
use t_Setup qw/:silent/; # strict, warnings, Test::More, Carp etc.

use t_Utils qw/bug oops/;

use Data::Dumper::Interp qw/visnew ivis dvis vis hvis avis u/;

use ODF::lpOD;
use ODF::lpOD_Helper qw/:DEFAULT :chars/;

my $skel_path = "$Bin/../tlib/Skel.odt";
my $doc = odf_get_document($skel_path);
my $body = $doc->get_body;

{
  my $m = $body->search("☺");
  ok($m->{segment}, "The :chars import tag implies Huse_character_strings");

  like(fmt_node($m->{segment}), qr/☺Unicode/, 
       ":DEFAULT still imports others")
}
done_testing();
