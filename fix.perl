
my ($what, $id) = @ARGV;

open A, "$what/of/of.$id";
while (<A>){
  chop();
  my ($id, @x) = split (/;/);
  for my $a (@x){
    $map{$a}=$id;
  }
}
my $line = 1;
open A, "$what/if/fin.$id";
while (<A>){
  chop();
  my ($id, @x) = split (/;/);
  print "$id:$map{$line};".(join ';', @x)."\n";
  $line ++;
}
