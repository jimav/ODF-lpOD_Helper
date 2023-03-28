#!/usr/bin/perl
use FindBin qw($Bin);
use lib $Bin;
use t_Setup; # strict, warnings, Test::More, Carp etc. Unicode STDfiles
use t_Utils qw/mydump bug oops/;

use ODF::lpOD;
use ODF::lpOD_Helper qw(:chars :DEFAULT);

#my $dump_kind = shift(@ARGV); # undef to use default format
my $dump_kind = 2;

my $input_path = "$Bin/../tlib/Skel.odt";
my $doc = odf_get_document($input_path);
my $body = $doc->get_body;
 
use open IO => "utf8", ":std"; ### WHY NECESSAR?
eval {
  note "=== DUMP OF ",File::Spec->abs2rel(abs_path($input_path))," <body> ===\n";
  #mydump($body, {dump_kind => $dump_kind}, @ARGV);
  say mydump($body, {dump_kind => $dump_kind, return_only => 1}, @ARGV);
  note "=== (END DUMP) ===\n";
};
die $@ if $@; # e.g. invalid argument

exit 0;
