#!/usr/bin/perl

if (($ENV{PERL_PERTURB_KEYS}//42) ne "2") {
  $ENV{PERL_PERTURB_KEYS} = "2"; # deterministic
  $ENV{PERL_HASH_SEED} = "0xDEADBEEF";
  exec $^X, $0, @ARGV; # for reproducible results
}
use FindBin qw($Bin);
use lib $Bin;
#use t_Setup qw/:silent/; # strict, warnings, Test::More, Carp etc.
use t_Setup ; # strict, warnings, Test::More, Carp etc.
use warnings FATAL => 'all';

use t_Utils qw/bug oops ok_with_lineno like_with_lineno
               rawstr showstr showcontrols displaystr
               show_white show_empty_string
               checkeq_literal check
               _check_end/;

use Mydump qw/mydump/;

use ODF::lpOD;
use ODF::lpOD_Helper qw/:DEFAULT :chars fmt_node fmt_tree fmt_match/;

use File::Copy ();
use Guard qw/guard scope_guard/;

my $master_copy_path = "$Bin/../tlib/Skel.odt";

# Prevent any possibility of over-writing the input file
my $input_path = "./_TMP_".basename($master_copy_path);
my $input_path_remover = guard { unlink $input_path };
File::Copy::copy($master_copy_path, $input_path) or die "File::Copy $!";

my $doc = odf_get_document($input_path, read_only => 1);
my $body = $doc->get_body;
 
{ my $debug = grep /-d/, @ARGV;;
  my $count = 0;
  my $prev_vtext = "Front Stuff";
  foreach (
           [["bold"], "NEW"],
           ["NEW"], ["NEW"], 
           ["\t"], ["NEW\t"], ["\tNEW"], ["NEW\t006"],
           ["\n"], ["NEW\n"], ["\nNEW"], ["NEW\n009"],
           [" "], ["NEW "], [" NEW"], ["NEW 009"],
           ["  "], ["NEW  "], ["  NEW"], ["NEW  009"],
           ["   "], ["NEW   "], ["   NEW"], ["NEW   009"],
           ["NEW \t\t\n   \n\n  "],
           [["italic"], "NEW", "NEW", [17], "17ptNEW", ["bold", 38], "38ptNEW"],
          ) 
  { my $new_content = $_;
    foreach (@$new_content) { 
      s/NEW/sprintf("NEW%03d", $count++)/esg unless ref; 
    }
    my $m = $body->Hsearch($prev_vtext) // bug;
    my $para = $m->{paragraph};
    my $orig_text = $para->get_text;
  
    note dvis 'BEFORE: $prev_vtext $new_content para:\n', fmt_tree($para)
      if $debug;
    
    $para->Hreplace(qr/\Q${prev_vtext}\E/, $new_content, debug => $debug);
  
    note "AFTER :\n", fmt_tree($para) if $debug;
  
    my $n_vtext = join("", grep{! ref} @$new_content);

    my $new_text = $para->get_text;
    (my $t = $orig_text) =~ s/\Q${prev_vtext}\E/${n_vtext}/s or oops;
    ok($new_text eq $t, 
       "Hreplace ".vis($prev_vtext)." with ".vis($new_content))
      || ( note(dvis '$orig_text\n$new_text\n$prev_vtext\n$n_vtext\n$t\n'), bug );
    $prev_vtext = $n_vtext;
  }
}

# TODO: Write Hreplace tests covering all the corner cases in Hsearch.t
# (Idea: discard and re-read the doc before each test, possibly using
# an in-memory xml rep instead of re-reading from disk each time).

#my $output_path = "./_OUTPUT_".basename($master_copy_path);
#$doc->save(target => $output_path);

done_testing();

