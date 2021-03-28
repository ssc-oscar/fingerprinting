#!/usr/bin/perl
use lib ("$ENV{HOME}/lookup", "$ENV{HOME}/lib64/perl5", "/home/audris/lib64/perl5","$ENV{HOME}/lib/perl5", "$ENV{HOME}/lib/x86_64-linux-gnu/perl", "$ENV{HOME}/share/perl5");
use strict;
use warnings;

use woc;

my (%badE, %badU);
open BE, "bad.e";
while (<BE>){
  chop();
  my ($e, $n) = split(/;/, $_);
  $e = lc($e);
  my $u = (split(/@/, $e))[0];
  $badE{$e} += $n;
  $badU{$u} += $n if defined $u;
}
$badE{""}=1000;
$badU{""}=1000;
my %badN;
my %badFN;
my %badLN;
open BE, "bad.fl";
while (<BE>){
  chop();
  my ($fn, $ln, $c) = split(/;/, $_);
  my $n = "";
  if ($fn ne "" && $ln ne ""){
    $n = "$fn $ln";
    $badN{lc($n)} += $c;
    $badFN{lc($fn)} += $c;
    $badFN{lc($ln)} += $c;
    $badLN{lc($ln)} += $c;
    $badLN{lc($fn)} += $c;
  }else{
    if ($fn ne ""){
      $badFN{lc($fn)} += $c;
      $badLN{lc($fn)} += $c;
    }
    if ($ln ne ""){
      $badFN{lc($ln)} += $c;
      $badLN{lc($ln)} += $c;
    }
  }
}
$badLN{""}=1000000;
$badFN{""}=1000000;
my %badGH;
open BE, "bad.gh";
while (<BE>){
  chop();
  my ($gh, $n) = split(/;/, $_);
  $badGH{lc($gh)} += $n;
}
$badGH{""}=1000;

while (<STDIN>){
  chop();
  my ($t,$k,$b,@as)=split(/;/);
  @as=sort @as;
  for my $i (0..($#as-1)){
    my @x = parseAuthorId ($as[$i]);
    @x = (@x, $x[0], $badFN{lc($x[0])}, $badLN{lc($x[1])});
    my $x0 = join ";", @x;
    for my $j (($i+1)..$#as){
      @x = parseAuthorId ($as[$j]);
      @x = (@x, $x[1], $badFN{lc($x[0])}, $badLN{lc($x[1])});
      my $x1 = join ";", @x;
      print "$t;$k;$b;$as[$i];$x0;$as[$j];$x1\n";
    }
  }
}


