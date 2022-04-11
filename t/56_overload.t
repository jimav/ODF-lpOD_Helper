#!/usr/bin/perl
use strict; use warnings  FATAL => 'all'; use feature qw(state say); use utf8;
use open IO => ':locale';
STDOUT->autoflush();
STDERR->autoflush();
select STDERR;

# Verify evaluation of overloaded deref operators.

package main::HVObj;

sub new { bless \[ [0..9], {a => 111, b => 222, c => 333} ], shift }

use overload  '@{}' => sub { ${ shift() }->[0] },
              '%{}' => sub { ${ shift() }->[1] },
              "fallback" => 1,
              ;

package main::HObj;

sub new { bless \\42, shift }

use overload  '%{}' => sub { \%{ main::HVObj->new() } },
              "fallback" => 1,
              ;


######################### MAIN IS HERE #####################3

package main;

use Test::More;
use Data::Dumper::Interp;
use Carp;

$Data::Dumper::Interp::Foldwidth = 0; # disable wrap

my $hvobj = main::HVObj->new();

my $hobj = main::HObj->new();

$Data::Dumper::Interp::Overloads = 0;
is (vis \@$hvobj, '[0,1,2,3,4,5,6,7,8,9]', "\@{HVObj}");
is (vis \%$hvobj, '{a => 111,b => 222,c => 333}', "\%{HVObj}");
is (vis $hvobj, do{chomp(my $_=<<'EOF'); $_}, "HVObj: Overloads disabled");
bless(do{\(my $o = [[0,1,2,3,4,5,6,7,8,9],{a => 111,b => 222,c => 333}])},'main::HVObj')
EOF
is (vis $hobj, q<bless(do{\(my $o = \42)},'main::HObj')>, "HObj: Overloads disabled");

$Data::Dumper::Interp::Overloads = 1;
is (vis \@$hvobj, '[0,1,2,3,4,5,6,7,8,9]', "\@{HVObj} again");
is (vis \%$hvobj, '{a => 111,b => 222,c => 333}', "\%{HVObj} again");
is (vis $hvobj, '[0,1,2,3,4,5,6,7,8,9]', "HVObj: Overloads enabled");
is (vis $hobj, '{a => 111,b => 222,c => 333}', "HObj: Overloads enabled");
is (hvis(%$hobj), '(a => 111,b => 222,c => 333)', "\%HObj: Overloads enabled");

done_testing();
exit 0;

