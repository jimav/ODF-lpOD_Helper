#!/usr/bin/perl
use FindBin qw($Bin);
use lib $Bin;
use t_Setup qw/bug :silent/; # strict, warnings, Test::More, Carp etc.

use Data::Dumper::Interp qw/visnew ivis dvis vis hvis avis u/;

use ODF::lpOD;
use ODF::lpOD_Helper qw/:DEFAULT fmt_node/;

my $skel_path = "$Bin/Skel.odt";

my $doc = odf_get_document($skel_path);
my $body = $doc->get_body;

{ my $octets = my_

{ my @matches = _my_search($body, "Front Stuff");
  croak "Could not find @_" unless @matches;
  croak "Found more than one for @_" unless @matches == 1;
  my $p = $matches[0]->{segments}->[0]->get_parent_paragraph;
  say "\nFront Stuff para: ", fmt_node($p);
}

{ my $match = my_search($body, qr/ad mumble .*veniam/);
  my $p = $match->{segments}->[0]->get_parent_paragraph;
  my $gt_text = $p->get_text;
  my $mgt_text = ODF::lpOD_Helper::_my_get_text_func($p);
  print "\nLorem ipsum: ";
  if ($gt_text eq $mgt_text) {
    say "get_text & _my_get_text_func IDENTICAL: ",vis($gt_text);
  } else {
    say "get_text & _my_get_text_func DIFFER:";
    say "   -------------";
    say "   get_text         :",vis($gt_text);
    say "   -------------";
    say "   _my_get_text_func:",vis($mgt_text);
    say "   -------------";
  }
  say "\nfmt_node:", fmt_node($p);
  die "tex";
}

{ my $match = my_search($body, "☺");
  my $p = $match->{segments}->[0]->get_parent_paragraph;
  #my $text = $p->get_text;
  my $text = ODF::lpOD_Helper::_my_get_text_func($p);
  say dvis '\nUnicode: @matches\n$p\n$text';
  say "\nfmt_node:", fmt_node($p);
}


__END__
use Scalar::Util qw/reftype refaddr blessed/;
my $usethis;
my @to_visit = @matches;
my %seen;
while (@to_visit) {
  my $item = shift @to_visit;
  my $rt = reftype($item);
  say "..item=",u($item)," rt=",u($rt);
  next unless defined($rt);
  next if $seen{refaddr($item)}++;
  if (blessed($item)) {
    if (
         blessed($item) eq 'ODF::lpOD::TextNode'
         #blessed($item) eq 'Archive::Zip::ZipFileMember'
         #blessed($item) eq 'ODF::lpOD::Container'
       ) {
      $usethis //= $item;
    }
  }
  if ($rt eq "HASH") {
    foreach my $k (keys %$item) {
      my $v = $item->{$k};
      say "..k=",u($k)," v=",u($v);
      if ( 0 
            || $k =~ /^crc32/
            || $k =~ /^segments/
            || $k =~ /^parent/

            || $k =~ /^#/
            || $k =~ /^att/
            || $k =~ /^container/
            || $k =~ /^first_/
            || $k =~ /^next_/
            || $k =~ /^zip/
            || $k =~ /^members/

#           || $k =~ /^match/
#           || $k =~ /^offset/
#           || $k =~ /^pcdata/
#           || $k =~ /^match/
#           || $k =~ /^gi/
#           || $k =~ /^central/
#           || $k =~ /^content/
#           || $k =~ /^context/
#           || $k =~ /^create/
#           || $k =~ /^disk/
#           || $k =~ /^deleted/
#           || $k =~ /^desired/
#           || $k =~ /^document/
#           || $k =~ /^elt/
#           || $k =~ /^eoc/
#           || $k =~ /^empty/
#           || $k =~ /^file/
#           || $k =~ /^last_/
#           || $k =~ /^fo:/
#           || $k =~ /^form/
#           || $k =~ /^load/
#           || $k =~ /^loext:/
#           || $k =~ /^[fl]seg_/
#           || $k =~ /^number/
#           || $k =~ /^office:/
#           || $k =~ /^officeooo:/
#           || $k =~ /^part/
#           || $k =~ /^pretty/
#           || $k =~ /^prev_/
#           || $k =~ /^read/
#           || $k =~ /^style/
#           || $k =~ /^svg:/
#           || $k =~ /^table:/
#           || $k =~ /^text/
#           || $k =~ /^type/
#           || $k =~ /^update/
#           || $k =~ /^uri/
#           || $k =~ /^version/
#           || $k =~ /^write/
#           || $k =~ /^xml/
           #|| 1
         ) {
        push @to_visit, $v;
      } else {
        say "..DELETING {$k}";
        delete $item->{$k};
      }
    }
  }
  elsif ($rt eq "ARRAY") {
    push @to_visit, @$item;
  }
  else {
    say "..Unhand reftype $rt";
  }
}

#say visnew->Objects(0)->Debug(1)->dvis('@matches');
#say visnew->Objects(0)->Debug(1)->dvis('$usethis');
my $noshow = visnew->Objects(0)->Debug(1)->dvis('@matches');

die "When done, put back :silence";
