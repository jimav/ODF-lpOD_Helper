#!/usr/bin/perl
use open ':std', ':encoding(UTF-8)'; ## WHY DOES NOT t_Setup PROVIDE THIS?
use FindBin qw($Bin);
use lib $Bin;
use t_Setup; # strict, warnings, Test::More, Carp etc. Unicode STDfiles
use t_Utils qw/bug oops/;
use Mydump qw/mydump/;

use ODF::lpOD;
use ODF::lpOD_Helper;
use ODF::lpOD_Helper qw(:chars);

my $dump_kind = shift(@ARGV); # undef to use default format

my $input_path = "$Bin/../tlib/Skel.odt";
my $doc = odf_get_document($input_path);
my $body = $doc->get_body;
 
# croaks on invalid args
my $result = mydump($body, {dump_kind => $dump_kind}, @ARGV);

note "=== DUMP OF ",File::Spec->abs2rel(abs_path($input_path))," <body> ===\n";
note $result;
note "=== (END DUMP) ===\n";

exit 0;
