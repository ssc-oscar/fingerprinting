#!/usr/bin/perl


use strict;
use warnings;

my (%e2i, %i2i);
open A, 'zcat *.split | cut -d\\; -f1,3 |';
while (<A>){
  chop ();
  my ($n,$e) = split (/;/); 
  $e =~ tr/[A-Z]/[a-z]/;
  $e2i{$e}{$n}++;
} 

while (my ($e, $v) = each %e2i){ 
  my @x=sort keys %{$v};
  if ($#x > 0){
    my $f = shift @x;
    for my $i (@x){
      $i2i{$i} = $f;
    }
  }
}

my %m;
my %done;
open A, 'zcat *.split |';
while (<A>){
  chop ();
  my ($i, @x) = split(/;/);
  $i = $i2i{$i} if defined $i2i{$i};
  my ($n, $s) = split(/:/, $i);
  $done{$n} ++;
  $m{$i}{$x[2]}++;
}
while (my ($i, $v)=each %m){
  my @x=sort keys %{$v};
  my $f = shift @x;
  print "$f;$f\n";
  for my $z (@x){
    print "$z;$f\n";
  }
}

open A, 'zcat *.whole|';
while (<A>){
  chop();
  my ($i,$n,$e,$a) = split (/;/);
  $m{$i}{$a}++;
  $done{$i}++;
}
while (my ($i, $v)=each %m){
  my @x=sort keys %{$v};
  my $f = shift @x;
  print "$f;$f\n";
  for my $z (@x){
    print "$z;$f\n";
  }
}




%m = ();

open A, 'cat block_chunk_*|';
while (<A>){
  chop();
  my ($i,$n,$e,$a) = split (/;/);
  next if defined $done{$i};
  print STDERR "no $i;$a\n";
}


