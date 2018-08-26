use strict;
use warnings;

my (%a, %t);
my $n = 0;
open A, $ARGV[0];
while(<A>){
    chop();
    my ($i,$ba,$bt)=split(/\;/, $_ ,-1);
    $a{$ba}{$i}++;
    $n = $i;
}
print STDERR "read $n\n";
open A, $ARGV[1];
while(<A>){
    chop();
    my ($i,$bt)=split(/\;/, $_ ,-1);
    $t{$bt}{$i}++;
    $n = $i;
}

my (%la, %lt);
my $mx = 0;
my $cl = 0;
while (my ($ba,$v) = each %a){
  my @vs = keys %{$v};
  $mx =$#vs if $#vs > $mx;
  $cl ++;
  for my $i (0..$#vs){
    for my $j (0..$#vs){
      $la{"$vs[$i];$vs[$j]"}++;
    }
  }
}
print STDERR "created la: $cl clstrs with max $mx\n";
$mx = 0;
$cl = 0;
while (my ($ba,$v) = each %t){
  my @vs = keys %{$v};
  $mx =$#vs if $#vs > $mx;
  $cl ++;
  for my $i (0..$#vs){
    for my $j (0..$#vs){
      $lt{"$vs[$i];$vs[$j]"}++;
    }
  }
}

print STDERR "created lt: $cl clstrs with max $mx\n";

my ($tp, $fp, $fn) = (0, 0, 0);
while (my $i = each %lt){
  if (defined $la{$i}){
    $tp++;
  }else{
    $fp++;
  }
}

print STDERR "tp=$tp fp=$fp\n";

while (my $i = each %la){
  $fn ++ if !defined $lt{$i};
}

print "tp=$tp;fp=$fp;fn=$fn\n";



