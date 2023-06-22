```
ver=U
#parse ID
for i in 0; do sed "s|WHAT|Cmt|g;s|FROM|$i|g;s|PRT|$j|g;s|VER|$ver|;s|MACHINE|beacon|;s|ppn=1|ppn=1|" ~/lookup/c2ta.pbs | qsub; sleep 1; done
zcat /data/basemaps/gz/Cmt${ver}.split|cut -d\; -f1 | perl -I ~/lookup/ -I ~/lib64/perl5/ -ane 'use woc;chop($_);print "$_;".(join ";", parseAuthorId($_))."\n";' |gzip > Cmt${ver}.split
#find sharing shingles by ghm e, fl
zcat Cmt${ver}.split | grep  -v ';$' | lsort 30G -t\; -k7 | gzip > Cmt${ver}.split.gh
zcat Cmt${ver}.split | grep -v ';;[^;]*$' | lsort 30G -t\; -k6 | gzip > Cmt${ver}.split.e
zcat Cmt${ver}.split  | lsort 30G -t\; -k2,3 | gzip > Cmt${ver}.split.fl
#count frequency 
zcat Cmt${ver}.split.gh | cut -d\; -f7 | uniq -c | awk '{print $2";"$1}' > bad${ver}.gh
zcat Cmt${ver}.split.fl | cut -d\; -f2,3 | uniq -c | awk '{print $2";"$1}' > bad${ver}.fl
zcat Cmt${ver}.split.e | cut -d\; -f6 | uniq -c | awk '{print $2";"$1}' > bad${ver}.e 

#get project-induced neighbors (four-author max projects)
for i in {0..31}; do  zcat /data/basemaps/gz/P2aFull$ver0.s|perl -e 'while(<STDIN>){chop();($p,$a)=split(/;/);$p2a{$p}{$a}++};for $p (keys %p2a){@as=keys %{$p2a{$p}};if ($#as<4){print "p;$p;".(join ";",@as)."\n";}}'; done | gzip > pNeighbors$ver

#create shingles for prediction
perl -I ~/lookup/ -I ~/lib64/perl5/ ~/lookup/findShingles.perl $ver | gzip > shingles$ver

zcat shingles$ver pNeighbors$ver | perl toCompare.perl | gzip > shingles$ver.cmp
zcat shingles$ver.cmp| ./prepCompare.perl U > shingles$ver.FoR
perl -ane '@x=split(/;/);print "$x[3];$x[13];$_";' < shingles$ver.FoR|lsort 50G -t\; -k1,2 -s | gzip > shingles$ver.FoR.s
zcat shingles$ver.FoR.s | ~/lookup/splitSecCh.perl shingles$ver.FoR. 32

#count shared projects
for i in {0..31}; do zcat shingles$ver.FoR.$i.gz | sed 's|;|__SEMICOLON__|' | lsort 30G -t\; -k1,1 | join -t\; - <(zcat a2aFull${ver}100.$i.s|sed 's|\r| |g;s|;|__SEMICOLON__|'|lsort 30G -t\; -k1,1) | sed 's|__SEMICOLON__|;|' | cut -d\; -f1,2 |gzip > shingles$ver.FoR.$i.prj; done

#final list for comparison
for i in {0..31}; do perl -e 'open A,"zcat  shingles'$ver'.FoR.'$i'.prj|";while(<A>){chop();$p{$_}++;}open A, "zcat shingles'$ver'.FoR.'$i'.gz|";while(<A>){chop();@x=split(/;/);$a=shift @x;$b=shift @x;$h=0;$h=1 if defined $p{"$a;$b"};@x=(@x,$h);print "".(join ";",@x)."\n";}'; done > shingles$ver.r


#fit on updated golden dataset (or reuse the model)
library(data.table);
library(randomForest)
load("rfmodelsCS.RData");
sel=pairs$pairs$im==1 | apply(pairs$pairs[,c("n","e","ln","fn","un","ifn","ln1","fn1")], 1, max) > .8 | pairs$pairs$ad>0|pairs$pairs$tdz>.9|pairs$pairs$d2vSim>0;
write(rbind(pairs$data1[pairs$pairs$id1[sel],"a"],pairs$data1[pairs$pairs$id2[sel],"a"],pairs$pairs$im[sel]),file="pairs.csv",sep=";",ncol=3);
res = c();
x=read.table("new_data.txt",sep=";")
for (i in names(table(x$V1))){
 vv = as.character(x[x$V1==i&x$V5==1,4]);
 vv1 = as.character(x[x$V1==i&x$V5==0,4]);
 if(length(vv)>1)
  for (j in 1:(length(vv)-1))
   for (k in (j+1):length(vv))
     res = rbind(res, c(vv[j], vv[k], 2))
 if(length(vv1)>0 && length(vv)>0)
  for (j in 1:(length(vv)))
   for (k in 1:length(vv1))
     res = rbind(res, c(vv[j], vv1[k], 1))

}
write(t(res),file="new_pairs.csv",sep=";",ncol=3);

# count shared projects for training
cat pairs.csv new_pairs.csv |lsort 1G -t\; -k1,2 | ~/lookup/splitSecCh.perl pairs. 32
for i in {0..31}; do zcat pairs.$i.gz | sed 's|;|__SEMICOLON__|' | join -t\; - <(zcat a2aFull${ver}100.$i.s|sed 's|;|__SEMICOLON__|') | sed 's|__SEMICOLON__|;|'| cut -d\; -f1-2 |gzip > pairs.$i.prj; done
for i in {0..31}; do perl -e 'open A,"zcat pairs.'$i'.prj|";while(<A>){chop();$p{$_}++;}open A, "zcat pairs.'$i'.gz|";while(<A>){chop();@x=split(/;/);$a=shift @x;$b=shift @x;$h=0;$h=1 if defined $p{"$a;$b"};@x=(@x,$h);print "$a;$b;".(join ";",@x)."\n";}'; done > pairs.r
#parse the ids
cat pairs.r | ./prepCompare1.perl > trainingPairs.r

#now do training
library(randomForest);
library(RecordLinkage);
x=read.table("trainingPairs.r",sep=";",quote="",comment.char="");
names(x)=c("a", "fn", "ln","u","h","e","gh","ifn","ffn","fln", "a1", "fn1", "ln1","u1","h1","e1","gh1","ifn1","ffn1","fln1","p","im") ;
x$ac = jarowinkler(as.character(x$a), as.character(x$a1));
x$fnc = jarowinkler(as.character(x$fn), as.character(x$fn1));
x$lnc = jarowinkler(as.character(x$ln), as.character(x$ln1));
x$uc = jarowinkler(as.character(x$u), as.character(x$u1));
x$hc = jarowinkler(as.character(x$h), as.character(x$h1));
x$ec = jarowinkler(as.character(x$e), as.character(x$e1));
x$ghc = jarowinkler(as.character(x$gh), as.character(x$gh1));
x$ifnc = jarowinkler(as.character(x$ifn), as.character(x$ifn1));
x[is.na(x$ffn),"ffn"] = 0;
x[is.na(x$ffn1),"ffn1"] = 0;
x$ffnc = log(1/(x$ffn+1))+log(1/(x$ffn1+1));
x$lfnc = log(1/(x$fln+1))+log(1/(x$fln1+1));
dt=x[,c("ac","fnc","lnc","uc","hc","ec","ghc","ifnc","ffnc","lfnc")];
im=as.factor(x$im);
rf=randomForest(im ~ ., dt, importance=T);

#now do prediction on shingles
y=read.table("shinglesU.r",sep=";",quote="",comment.char="")
names(y)=c("type","k","bad","a", "fn", "ln","u","h","e","gh","ifn","ffn","fln", "a1", "fn1", "ln1","u1","h1","e1","gh1","ifn1","ffn1","fln1","p") 
y$ac = jarowinkler(as.character(y$a), as.character(y$a1));
y$fnc = jarowinkler(as.character(y$fn), as.character(y$fn1));
y$lnc = jarowinkler(as.character(y$ln), as.character(y$ln1));
y$uc = jarowinkler(as.character(y$u), as.character(y$u1));
y$hc = jarowinkler(as.character(y$h), as.character(y$h1));
y$ec = jarowinkler(as.character(y$e), as.character(y$e1));
y$ghc = jarowinkler(as.character(y$gh), as.character(y$gh1));
y$ifnc = jarowinkler(as.character(y$ifn), as.character(y$ifn1));
y[is.na(y$ffn),"ffn"] = 0;
y[is.na(y$ffn1),"ffn1"] = 0;
y$ffnc = log(1/(y$ffn+1))+log(1/(y$ffn1+1));
y$lfnc = log(1/(y$fln+1))+log(1/(y$fln1+1));
y$im = predict(rf,y);
#write links out
sel=y$im==1;
write(rbind(as.character(y[sel,"a"]),as.character(y[sel,"a1"])),file="linksU.csv",sep=";",ncol=2);
sel=y$im==0;
write(rbind(as.character(y[sel,"a"]),as.character(y[sel,"a1"])),file="nolinksU.csv",sep=";",ncol=2);
sel=x$im==1;
write(rbind(as.character(x[sel,"a"]),as.character(x[sel,"a1"])),file="linksU.csv",append=T,sep=";",ncol=2);

#use links fron ver-[ST] as well
zcat /da0_data/basemaps/gz/a2AFullHT.s | cut -d\; -f1,2 >> links$ver.csv
#make sure all nodes are in the graph
cat  linksU.csv | perl -ane 's|;|\n|;print' | lsort 30G -t\; -k1,1 > links$ver.nodes
zcat Cmt${ver}.split|cut -d\; -f1 | sed 's|\r| |g' | lsort 30G -t\; -k1,1 > Cmt${ver}.nodes
join -t\; -v1 Cmt${ver}.nodes links$ver.nodes | grep -v '^$' | perl -ane 'chop;print "$_;$_\n"' >> links$ver.csv
#connected subgraphs
cat links$ver.csv | perl ~/lookup/connectBasic.perl links$ver | gzip > links$ver.map
#sort by canonical name
zcat  links$ver.map |lsort 100G -t\; -k2 | gzip > links$ver.map.u.cs
# identify homonyms
zcat links$ver.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad$ver | gzip > a2AFullH${ver}.s

# if now frequent (see below) done?
# finalize 

zcat a2AFullHU.s |  perl splitNames.perl | lsort 30G -u -t\; -k1,4 |gzip > namesU
#now prep:
zcat namesU|perl -e '$idp="";$i=-1;while(<STDIN>){chop();($id,$b0,$n,$b1)=split(/;/);$i++ if($id ne $idp); $idp=$id;use Encode;$nd=decode("UTF-8",$n);$n=encode("UTF-8",$nd);@fn=split(/ /,$n);if($#fn>0){print "$i|$n\n";$i++ if($b0>0);}else{print "$i|$n\n" if ($b1 eq ""&&$b0>0);}}' | gzip > ../namesor/toNamesor1

zcat namesU | perl -e '$idp="";$i=-1;while(<STDIN>){chop();($id,$b0,$n,$b1)=split(/;/);$nOrig=$n;$i++ if($id ne $idp); $idp=$id;use Encode;$nd=decode("UTF-8",$n);$n=encode("UTF-8",$nd);@fn=split(/ /,$n);if($#fn>0){print "$i;$idr\n";$i++ if($b0>0);}else{print "$i;$id\n" if ($b1 eq ""&&$b0>0);}}' | gzip > ../namesor/toNamesor1.id2all

cd ../namesor

zcat ../forks/a2AFullHU.s | perl -e 'open A, "zcat toNamesor1.id2all|";while(<A>){chop();($i,$n)=split(/;/);$i2n{$i}=$n};open A, "toNamesor1_idfnlngeo.txt.gender.namsor";while(<A>){chop();($id,@x)=split(/\|/,$_); print STDERR "no:$id\n" if !defined $i2n{$id};$p{$i2n{$id}}{"$x[3];$x[5]"}++;} while(<STDIN>){chop();($n,$rn,$b0,$b1) = split(/;/);if (defined $p{$rn}){$v=join ";", keys %{$p{$rn}};print "$n;$rn;$b0;$b1;$v\n"}else{if (defined $p{$n}){$v=join ";", keys %{$p{$n}};print "$n;$rn;$b0;$b1;$v\n"}else{print STDERR "$n;$rn;$b0;$b1\n"}}}' 2> err | gzip > ../forks/a2AFullHGenU.s

cat err | grep -E '<[^0-9]*@'|perl -ane 'chop();($a,$b,$x,$x1)=split(/;/);$pre="";if ($a=~/<([^0-9]+)\@/){$pre=$1};$pre1="";if ($b=~/<([^0-9]+)\@/){$pre1=$1};if ($pre ne "" || $pre1 ne ""){print "$pre;$pre1;$a;$b;$x;$x1\n";}'|perl -ane 'chop();($pa,$pb,$a,$b,$x,$x1)=split(/;/);if ($pa ne ""){print "$pa;$a;$b\n";}if($pb ne ""){print "$pb;$a;$b\n";}' | perl -ane 'chop();($pa,$a,$b)=split(/;/); if ($pa =~ /[\._]/){$pa =~ s/[._]/ /g;print "$pa;$a;$b\n"}else{ print STDERR "$pa;$a;$b\n"};'  2> err1 | gzip > ../namesor/toNamesor2
#now get new names based on username
zcat toNamesor2 | cut -d\; -f1 | perl -ane 'chop();tr/[A-Z]/[a-z]/;s|[^[:alpha:]]| |g;s|\s+| |g;s|^ ||;s| $||;print "$_\n"' | grep -v '^$' | lsort 10G -u | perl -e 'open A, "zcat toNamesor1|";while(<A>){chop();($i,$n)=split(/\|/);$s{$n}++;$lst=$i if $i > $lst};while(<STDIN>){chop();if (! defined $s{$_}){$lst++;;use Encode;$nd=decode("UTF-8",$_);$n=encode("UTF-8",$nd);@x=split(/ /,$nd);$ml=1;for $i (@x){$ml=length($i) if length($i)>$ml;}print "$lst|$nd\n" if $ml> 1}}' | gzip > toNamesor2.gz

cat err | perl -ane 'chop();s/[&$%=!#"|(){}\-,+_*0-9:.@]//g;s|[%/?\\`]||g;s/\s+/ /g;s/^ //;s/; /;/;s/ ;/;/;($a,$b,$c,$d)=split(/;/);$a=~s/\s*\<.*//;@x=split(/ /,$a);print "@x\n"if $#x>0;$b=~s/\s*\<.*//;@x=split(/ /,$b);print "@x\n"if $#x>0;' | lsort 10G -u | gzip > toNamesor3

zcat  toNamesor3 | perl -e 'open A, "zcat toNamesor1 toNamesor2.gz|";while(<A>){chop();($i,$n)=split(/\|/);$s{$n}++;$lst=$i if $i > $lst};while(<STDIN>){chop();if (! defined $s{$_}){$lst++;;use Encode;$nd=decode("UTF-8",$_);$n=encode("UTF-8",$nd);@x=split(/ /,$nd);$ml=1;for $i (@x){$ml=length($i) if length($i)>$ml;}print "$lst|$nd\n" if $ml> 1}}' | gzip > toNamesor3.gz

zcat toNamesor1 toNamesor[23].gz | gzip >  toNamesor20220316.gz


#now get what is missing
zcat toNamesor2 | perl -ane 'chop();($a,$id,$idr)=split(/;/);$a=~tr/[A-Z]/[a-z]/;$a=~s|[^[:alpha:]]| |g;$a=~s|\s+| |g;$a=~s|^ ||;$a=~s| $||;print "$id;$idr;$a\n"if $a ne ""' | gzip > toNamesor2.map &
cat err | perl -ane 'chop();($n,$nr)=split(/;/);for $i ($n, $nr){ $i=~s/[&$%=!#"|(){}\-,+_*0-9:.@]//g;s|[%/?\\`]||g;$i=~s/\s+/ /g;$i=~s/^ //;$i=~s/ $//;$i=~s/\s*\<.*//;@x=split(/ /,$i);print "$n;$nr;@x\n" if $#x>0;}'|gzip > toNamesor3.map
zcat ../forks/namesU | perl -e '$idp="";$i=-1;while(<STDIN>){chop();($id,$b0,$n,$b1)=split(/;/);$nOrig=$n;$i++ if($id ne $idp); $idp=$id;use Encode;$nd=decode("UTF-8",$n);$n=encode("UTF-8",$nd);@fn=split(/ /,$n);if($#fn>0){print "$id;$idr;@fn\n";}}' | gzip > toNamesor1.map
zcat ../forks/a2AFullHU.s | perl -e 'open A,"zcat toNamesor1.map toNamesor2.map toNamesor3.map|";while(<A>){chop();($a,$b)=split(/;/);$s{$a}++;$s{$b}++;} while(<STDIN>){chop();($a,$b)=split(/;/);print "$a;$b\n" if !defined $s{$a} && !defined $s{$b};}' | gzip > a2AFullHU.missed

zcat ../forks/a2AFullHU.s | perl -e 'open A, "zcat toNamesor20220317.gz|";while(<A>){chop();($i,$nn)=split(/|/);$n2i{$nn}=$i;}; open A, "zcat toNamesor1.map toNamesor2.map toNamesor3.map|";while(<A>){chop();($id,$empty,$nn)=split(/;/,$_,-1);$n2id{$nn}{$id}++;$id2n{$id}{$nn}++;};open A, "toNamesor20220317_idfnln.txt.gender.namsor";while(<A>){chop();($i,@x)=split(/\|/,$_); $p{$i}{"$x[2];$x[4]"}++;}; while(<STDIN>){chop();($n,$rn,$b0,$b1) = split(/;/);if (defined $id2n{$rn}){for $nn (keys %{$id2n{$rn}}){ if (defined $n2i{$nn}){$v = join ";", (keys %{$p{$n2i{$nn}}}); print "$n;$rn;$b0;$b1;$nn;$n2i{$nn};$v;\n";}else{print STDERR "no n2i for $nn;$n;$rn\n";}}}else{ if (defined $id2n{$n}){for $nn (keys %{$id2n{$n}}){ if (defined $n2i{$nn}){ $v=join ";", (keys %{$p{$n2i{$nn}}}); print "$n;$rn;$b0;$b1;$nn;$n2i{$nn};$v\n"}}}else{print STDERR "$n\n$rn\n";}}}' 2> err | gzip > ../forks/a2AFullHGen1U.s


(zcat namesU|perl -ane 's/[^[:print:]]+/ /g;s/[;|]/ /g;s/^\s*//;s/\s*$//;s/\s+/ /g;print "$_\n";' | grep -v '^$'; cat /data/basemaps/gz/flU.s1 |sed "s|'| |g" | perl -ane 's/[^[:print:]]+/ /g;s/[;|]/ /g;s/^\s*//;s/\s*$//;s/\s+/ /g;print "$_\n
";')|sed "s|'| |g;s|^\s*$||" |grep -v '^$' | lsort 30G -t\; -u | gzip > flU.s2
#get rid of single-letter prefixes and suffixes for namesor
zcat flU.s2|perl -ane 's/^\s*//;s/\s*$//;print "$_\n";'|grep -v '^$'| perl -ane 'chop();@x=split(/ /); while ($#x>1 && length($x[0])<2){shift @x;};while ($#x>1 && length($x[$#x])<2){pop @x;};$s="@x";$s=~s/^\s*//;$s=~s/\s*$//;print "$s\n" if(length($s)>3);'|lsort 30G -t\; -u |gzip > flU.s3
#not clear how to match multiple characters
str="zcat flU.s3";for i in {a..z}; do str="$str|perl -ane 's|$i{3,}|$i|g;print'"; done
eval $str|perl -ane 's/^\s*//;s/\s*$//;s/\s+/ /g;print "$_\n" if(length($_)>4);'| lsort 30G -t\; -u |gzip > flU.s4
zcat flU.s4| perl -ane 'chop();@x=split(/ /); while ($#x>1 && length($x[0])<2){shift @x;};while ($#x>1 && length($x[$#x])<2){pop @x;};$s="@x";$s=~s/^\s*//;$s=~s/\s*$//;print "$s\n" if(length($s)>3);'|lsort 30G -t\; -u| gzip >flU5.s




#check frequent
zcat a2AFullH${ver}.s|cut -d\; -f2-4|grep ';0;0$' | cut -d\; -f1 |perl -e 'while(<STDIN>){chop();$n{$_}++;}while(($k,$m)=each %n){print "$k;$m\n" if $m > 5}' | gzip > a2AFullFreq${ver}.s
zcat a2AFullFreq${ver}.s |cut -d\; -f1 |gzip > find$ver.gz
zcat a2AFullH${ver}.s|~/lookup/grepField.perl find$ver.gz 1|perl -e 'while(<STDIN>){chop();($a,$ca,$b0,$b1)=split(/;/);$n{$ca}{$a}++;}while(($k,$v)=each %n){@as=sort keys %$v; for $i (0..($#as-1)){for $j (($i+1)..$#as){print "$as[$i];$as[$j]\n";}}}'| gzip > check$ver
zcat check$ver | lsort 1G -t\; -k1,2 | ~/lookup/splitSecCh.perl check$ver. 32
for i in {0..31}; do zcat check$ver.$i.gz | sed 's|;|__SEMICOLON__|' | join -t\; - <(zcat a2aFull${ver}100.$i.s|sed 's|;|__SEMICOLON__|') | sed 's|__SEMICOLON__|;|'| cut -d\; -f1-2 |gzip > check$ver.$i.prj; done
for i in {0..31}; do perl -e 'open A,"zcat check'$ver'.'$i'.prj|";while(<A>){chop();$p{$_}++;}open A, "zcat check'$ver'.'$i'.gz|";while(<A>){chop();@x=split(/;/);$a=shift @x;$b=shift @x;$h=0;$h=1 if defined $p{"$a;$b"};@x=(@x,$h);print "$a;$b;".(join ";",@x)."\n";}'; done > check$ver.r
cat check$ver.r | ./prepCompare2.perl > checkingPairs$ver.r
#empty

z=read.table("checkingPairsU.r",sep=";",quote="",comment.char="")
names(z)=c("a", "fn", "ln","u","h","e","gh","ifn","ffn","fln", "a1", "fn1", "ln1","u1","h1","e1","gh1","ifn1","ffn1","fln1","p") 
z$ac = jarowinkler(as.character(z$a), as.character(z$a1));
z$fnc = jarowinkler(as.character(z$fn), as.character(z$fn1));
z$lnc = jarowinkler(as.character(z$ln), as.character(z$ln1));
z$uc = jarowinkler(as.character(z$u), as.character(z$u1));
z$hc = jarowinkler(as.character(z$h), as.character(z$h1));
z$ec = jarowinkler(as.character(z$e), as.character(z$e1));
z$ghc = jarowinkler(as.character(z$gh), as.character(z$gh1));
z$ifnc = jarowinkler(as.character(z$ifn), as.character(z$ifn1));
z[is.na(z$ffn),"ffn"] = 0;
z[is.na(z$ffn1),"ffn1"] = 0;
z$ffnc = log(1/(z$ffn+1))+log(1/(z$ffn1+1));
z$lfnc = log(1/(z$fln+1))+log(1/(z$fln1+1));
z$im = predict(rf,z);
#write links out
sel=z$im==1;
#write(rbind(as.character(z[sel,"a"]),as.character(z[sel,"a1"])),file="links.csv",sep=";",ncol=2);

kapil <kapil@fluendo.com>
#complicated, most of the time RF correctly predicts no match, but sometimes it does predict a match

#try this
zcat a2AFullH${ver}.s1|grep -v ';0;0$' | cut -d\; -f1,2 | perl -ane 'chop();($a,$b)=split(/;/);print "$a;$b\n" if $a ne $b;' | gzip > badLinks
cat links.csv | sed 's|;|__SEMICOLON__|' |lsort 100G -t\; -k1,1 | join -t\; -v1 - <(sed 's|;|__SEMICOLON__|' badLinks | lsort 10G -t\; -k1,1)  | sed 's|__SEMICOLON__|;|' > links1.csv
cat links1.csv | perl ~/lookup/connectBasic.perl links1 | gzip > links1.map
zcat  links1.map |lsort 100G -t\; -k2 | gzip > links1.map.u.cs
# identify homonyms
zcat links1.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad | gzip > a2AFullH${ver}.s



grep -v ';dane <dane@watcha.com>$' links1.csv > links2.csv
echo 'dane <dane@watcha.com>;dane <dane@watcha.com>' >> links2.csv
cat links2.csv | sed 's|;|__SEMICOLON__|' |lsort 100G -t\; -k1,1 | join -t\; -v1 - <(sed 's|;|__SEMICOLON__|' nolinks.csv | lsort 100G -t\; -k1,1)  | sed 's|__SEMICOLON__|;|' > links3.csv

cat links3.csv | perl ~/lookup/connectBasic.perl links3 | gzip > links3.map
zcat  links3.map |lsort 100G -t\; -k2 | gzip > links3.map.u.cs
# identify homonyms
zcat links3.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad | gzip > a2AFullH${ver}.s
zcat a2AFullH${ver}.s | grep  ';0;0$' | cut -d\; -f2 | uniq -c | lsort 10G -rn | head -50

grep -v 'lin <citylcs@gmail.com>' links3.csv | grep -v 'jake <jdowd7@gmail.com' > links4.csv
echo 'lin <citylcs@gmail.com>;lin <citylcs@gmail.com>' >> links4.csv
echo 'jake <jdowd7@gmail.com>;jake <jdowd7@gmail.com>' >> links4.csv

zcat links3.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad | gzip > a2AFullH${ver}.s
zcat a2AFullH${ver}.s|grep -v ';0;0$' | cut -d\; -f1,2 | perl -ane 'chop();($a,$b)=split(/;/);print "$a;$b\n" if $a ne $b;' | gzip > badLinks4
cat links4.csv | sed 's|;|__SEMICOLON__|' |lsort 100G -t\; -k1,1 | join -t\; -v1 - <(sed 's|;|__SEMICOLON__|' badLinks4 | lsort 10G -t\; -k1,1)  | sed 's|__SEMICOLON__|;|' > links5.csv

cat links5.csv | perl ~/lookup/connectBasic.perl links5 | gzip > links5.map
zcat  links5.map |lsort 100G -t\; -k2 | gzip > links5.map.u.cs
# identify homonyms
zcat links5.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad | gzip > a2AFullH${ver}.s
zcat a2AFullH${ver}.s | grep  ';0;0$' | cut -d\; -f2 | uniq -c | lsort 10G -rn | head -50

#get links above 4 and run prediction on them
zcat a2AFullH${ver}.s|cut -d\; -f2|perl -e 'while(<STDIN>){chop();$n{$_}++;}while(($k,$m)=each %n){print "$k\n" if $m > 4}' |gzip > find.gz
zcat a2AFullH${ver}.s|~/lookup/grepField.perl find.gz 2 | perl -e 'while(<STDIN>){chop();($a,$ca,$b0,$b1)=split(/;/);next if $b0+$b1>0;$n{$ca}{$a}++;}while(($k,$v)=each %n){@as=sort keys %$v; for $i (0..($#as-1)){for $j (($i+1)..$#as){print "$as[$i];$as[$j]\n";}}}'| gzip > check5
#use links 4 and below as is
zcat a2AFullH${ver}.s| cut -d\; -f1,2 | ~/lookup/grepFieldv.perl find.gz 2 > links6.csv 


zcat check5 | lsort 1G -t\; -k1,2 | ~/lookup/splitSecCh.perl check5. 32
for i in {0..31}; do zcat check5.$i.gz | sed 's|;|__SEMICOLON__|' | join -t\; - <(zcat a2aFull${ver}100.$i.s|sed 's|;|__SEMICOLON__|') | sed 's|__SEMICOLON__|;|'| cut -d\; -f1-2 |gzip > check5.$i.prj; done
for i in {0..31}; do perl -e 'open A,"zcat check5.'$i'.prj|";while(<A>){chop();$p{$_}++;}open A, "zcat check5.'$i'.gz|";while(<A>){chop();@x=split(/;/);$a=shift @x;$b=shift @x;$h=0;$h=1 if defined $p{"$a;$b"};@x=(@x,$h);print "$a;$b;".(join ";",@x)."\n";}'; done > check5.r
cat check5.r | ./prepCompare2.perl > checkingPairs5.r
sed -i 's|\r| |g' checkingPairs5.r
z=read.table("checkingPairs5.r",sep=";",quote="",comment.char="")
names(z)=c("a", "fn", "ln","u","h","e","gh","ifn","ffn","fln", "a1", "fn1", "ln1","u1","h1","e1","gh1","ifn1","ffn1","fln1","p") 
z$ac = jarowinkler(as.character(z$a), as.character(z$a1));
z$fnc = jarowinkler(as.character(z$fn), as.character(z$fn1));
z$lnc = jarowinkler(as.character(z$ln), as.character(z$ln1));
z$uc = jarowinkler(as.character(z$u), as.character(z$u1));
z$hc = jarowinkler(as.character(z$h), as.character(z$h1));
z$ec = jarowinkler(as.character(z$e), as.character(z$e1));
z$ghc = jarowinkler(as.character(z$gh), as.character(z$gh1));
z$ifnc = jarowinkler(as.character(z$ifn), as.character(z$ifn1));
z[is.na(z$ffn),"ffn"] = 0;
z[is.na(z$ffn1),"ffn1"] = 0;
z$ffnc = log(1/(z$ffn+1))+log(1/(z$ffn1+1));
z$lfnc = log(1/(z$fln+1))+log(1/(z$fln1+1));
z$im = predict(rf,z);
#write links out
sel=z$im==1;
#only estimated links are written out
write(rbind(as.character(z[sel,"a"]),as.character(z[sel,"a1"])),file="links6.csv",append=T,sep=";",ncol=2);

#keep detached nodes
cat  links6.csv | perl -ane 's|;|\n|;print' | lsort 30G -t\; -k1,1 > links6.nodes
join -t\; -v1 Cmt${ver}.nodes links6.nodes | grep -v '^$' | perl -ane 'chop;print "$_;$_\n"' >> links6.csv


cat links6.csv | perl ~/lookup/connectBasic.perl links6 | gzip > links6.map
zcat  links6.map |lsort 100G -t\; -k2 | gzip > links6.map.u.cs
# identify homonyms
zcat links6.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad | gzip > a2AFullH${ver}.s
zcat a2AFullH${ver}.s | grep  ';0;0$' | cut -d\; -f2 | uniq -c | lsort 10G -rn | head -50

zcat a2AFullH${ver}.s|grep -v ';0;0$' | cut -d\; -f1,2 | perl -ane 'chop();($a,$b)=split(/;/);print "$a;$b\n" if $a ne $b;' | gzip > badLinks6
cat links6.csv | sed 's|;|__SEMICOLON__|' |lsort 100G -t\; -k1,1 | join -t\; -v1 - <(sed 's|;|__SEMICOLON__|' badLinks6 | lsort 10G -t\; -k1,1)  | sed 's|__SEMICOLON__|;|' > links7.csv
cat  links7.csv | perl -ane 's|;|\n|;print' | lsort 30G -t\; -k1,1 > links7.nodes
join -t\; -v1 Cmt${ver}.nodes links7.nodes | grep -v '^$' | perl -ane 'chop;print "$_;$_\n"' >> links7.csv
cat links7.csv | perl ~/lookup/connectBasic.perl links7 | gzip > links7.map
zcat  links7.map |lsort 100G -t\; -k2 | gzip > links7.map.u.cs
zcat links7.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad | gzip > a2AFullH${ver}.s
zcat a2AFullH${ver}.s|grep -v ';0;0$' | cut -d\; -f1,2 | perl -ane 'chop();($a,$b)=split(/;/);print "$a;$b\n" if $a ne $b;' | gzip > badLinks7
grep -v  'Christian Clauss <cclauss@me.com>;Fabienne Clauss <cclauss@me.com>' links7.csv |\
grep -v  'Fabienne Clauss <cclauss@me.com>;cclauss <cclauss@bluewin.ch>'|\
grep -v  'Fabienne Clauss <cclauss@me.com>;cclauss <cclauss@me.com>'|\
sed 's|;|__SEMICOLON__|' |lsort 100G -t\; -k1,1 | join -t\; -v1 - <(sed 's|;|__SEMICOLON__|' badLinks7 | lsort 10G -t\; -k1,1)  | sed 's|__SEMICOLON__|;|' > links8.csv
cat  links8.csv | perl -ane 's|;|\n|;print' | lsort 30G -t\; -k1,1 > links8.nodes
join -t\; -v1 Cmt${ver}.nodes links8.nodes | grep -v '^$' | perl -ane 'chop;print "$_;$_\n"' >> links8.csv
cat links8.csv | perl ~/lookup/connectBasic.perl links8 | gzip > links8.map
zcat  links8.map |lsort 100G -t\; -k2 | gzip > links8.map.u.cs
zcat links8.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad | gzip > a2AFullH${ver}.s

zcat a2AFullH${ver}.s|grep -v ';0;0$' | cut -d\; -f1,2 | perl -ane 'chop();($a,$b)=split(/;/);print "$a;$b\n" if $a ne $b;' | gzip > badLinks8
sed 's|;|__SEMICOLON__|' links8.csv  |lsort 100G -t\; -k1,1 | join -t\; -v1 - <(sed 's|;|__SEMICOLON__|' badLinks8 | lsort 10G -t\; -k1,1)  | sed 's|__SEMICOLON__|;|' > links9.csv
cat  links9.csv | perl -ane 's|;|\n|;print' | lsort 30G -t\; -k1,1 > links9.nodes
join -t\; -v1 Cmt${ver}.nodes links9.nodes | grep -v '^$' | perl -ane 'chop;print "$_;$_\n"' >> links9.csv
cat links9.csv | perl ~/lookup/connectBasic.perl links9 | gzip > links9.map
zcat  links9.map |lsort 100G -t\; -k2 | gzip > links9.map.u.cs
zcat links9.map.u.cs | ~/lookup/findHomonyms.perl 2> potBad | gzip > a2AFullH${ver}.s
cp -p a2AFullH${ver}.s /data/basemaps/gz
scp -p a2AFullH${ver}.s da0:/data/basemaps/gz

# Next steps

Streamline the process of updating/adding new training data/etc

What to do about proper emails but indicatiing atomation:
Travis-CI <wfortin@coveo.com>

Minor

- perhaps pick some of the incorrect matches as trainig:  'Christian Clauss <cclauss@me.com>;Fabienne Clauss <cclauss@me.com>'
- add more predictors, such as commit message text, timezone, API usage?


Major

- do ngram shingling as additional similarity operation
- investigate exclusion of very large shingles
- investigate including first and last name shingle
- fit a special model for bot detection (need to prep a golden dataset
for training) 

```