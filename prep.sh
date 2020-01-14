



#ght map
zcat bobo_commitSHA_author1.gz | perl -ane 'chop();@x=split(/\t/);$c=pop @x;print "$c;".(join ";", @x)."\n"' | ~/lookup/splitSec.perl c2a.bobo. 32 
for j in {0..31}
do zcat c2a.bobo.$j.gz | \
	perl -ane '@x=split(/;/);if ($x[0]=~m/^[0-f]{40}$/){print "".(shift @x).";".(join "__SEMICOLON__", @x)}' | \
	join -t\; - <(zcat /data/basemaps/gz/c2taFullQ.$j.s) | \
	cut -d\; -f2,4 | uniq | lsort 50G -u -t\; -k1,2
done | lsort 100G -u -t\; -k1,2 | gzip > ght2aQ.gz
zcat ght2aQ.gz | \
 perl -e '$p="";while(<STDIN>){($c,@x)=split(/;/); if ($c ne $p && $p ne ""){ if (scalar(keys %tmp)>1){ for $i (keys %tmp){ print "$i"};} %tmp=(); }; $p=$c; $tmp{$_}++;}' | \
 perl -ane '@x=split(/;/);print if length($x[0]) >= 5 ; ' | \
 perl $HOME/lookup/connectExportPreNoExclude.perl | gzip > ght2aQ.p2p 


i=ght2aQ
k=12
zcat $i.p2p | perl -ane 'chop();@x=split(/;/);print "$_\n" if $#x<'$k';'|gzip > $i.p2p.$k 
(zcat $i.p2p.$k |perl $HOME/lookup/connectExportSrt.perl $i;zcat $i.versions |  perl $HOME/lookup/connectPrune.perl |gzip > $i.versions1; zcat $i.versions1| ~/bin/connect | gzip > $i.clones; perl $HOME/lookup/connectImport.perl $i | gzip > $i.map.$k) 


# WoC map based on email

zcat /data/basemaps/gz/asQ.gz | \
  perl -ane 'chop();m/.*<(.*)>/;$e=$1;$e=~tr/[A-Z]/[a-z]/;$e=~s/^\s+//;$e=~s/\s+$//;$e=~s/"//g;print "$e;$_\n" if $e =~ m|.+\@.+\..+| && length ($e) > 8 && !($e =~ m/\.\(none\)|\@none|\@think\.pad|\.local$|\@example.com|\@corpmail\.com|\@user-laptop|macbook-pro\.local|localhost\.localdomain|ubuntu\.\(none\)|users\.noreply\.github\.com|example\.com|imac\.local|ubuntu\.localdomain|ubuntu\.ubuntu-domain/);' | \
	lsort 100G -t\; -k1,2 | gzip >  /data/basemaps/gz/E2aQ.gz.clean

zcat /data/basemaps/gz/E2aQ.gz.clean | \
  perl -e '$p="";while(<STDIN>){($c,@x)=split(/;/); if ($c ne $p && $p ne ""){ if (scalar(keys %tmp)>1){ for $i (keys %tmp){ print "$i"};} %tmp=(); }; $p=$c; $tmp{$_}++;}' | \
	perl -ane '@x=split(/;/);print if $x[0] =~ m/^.+\@[^\.]+\.[^\.]+/ && length($x[0]) > 8; ' | \
	perl $HOME/lookup/connectExportPreNoExclude.perl | gzip > E2aQ.p2p

i=E2aQ
k=12
zcat $i.p2p | perl -ane 'chop();@x=split(/;/);print "$_\n" if $#x<'$k';'|gzip > $i.p2p.$k 
(zcat $i.p2p.$k |perl $HOME/lookup/connectExportSrt.perl $i;zcat $i.versions |  perl $HOME/lookup/connectPrune.perl |gzip > $i.versions1; zcat $i.versions1| ~/bin/connect | gzip > $i.clones; perl $HOME/lookup/connectImport.perl $i | gzip > $i.map.$k) 


#what about brun@27541ba8-7e3a-0410-8455-c3a389f83636



#inspect
zcat $i.map.$k|cut -d\; -f2 | lsort 10G | uniq -c | sort -rn | head -100

#plain ght
zcat users.gz | perl -ane '@x=split(/\t/);print "$x[0]\;$x[1];$x[4];$x[5]\n";' | gzip > users0.gz 

zcat commits.gz | perl -ane 'chop();@x=split(/\t/);print "$x[1];$$x[2]\n";' |~/lookup/splitSec.perl commit. 32
for i in {0..31}; do zcat commit.$i.gz | lsort 100G -t\; -k1,2 -u |gzip > commit.$i.s; done
for j in {0..31}; do zcat  commit.$j.s | join -t\; - <(zcat /data/basemaps/gz/c2taFullQ.$j.s) | cut -d\; -f2,4| uniq | lsort 50G -u -t\; -k1,2;  done | lsort 100G -u -t\; -k1,2 | gzip > gh2aidQ.gz
zcat gh2aidQ.gz | ~/bin/joinField.perl users0.gz 1 | cut -d\: -f2- | gzip > a2ghQ.gz

#use onlu user ids and only the ones that are not fake
# would be nice not to link by short autor IDs
zcat a2ghQ.gz |   perl -e '$p="";while(<STDIN>){($c,@x)=split(/;/); if ($c ne $p && $p ne ""){ if (scalar(keys %tmp)>1){ for $i (keys %tmp){ print "$i"};} %tmp=(); }; $p=$c; $tmp{$_}++;}' |   perl -ane '@x=split(/;/);print "$x[0];$x[1]\n" if $x[3] eq "USR" && $x[4] == 0' |   perl $HOME/lookup/connectExportPreNoExclude.perl | gzip > a2ghQ.p2p

zcat a2ghQ.gz |   perl -e '$p="";while(<STDIN>){($c,@x)=split(/;/); if ($c ne $p && $p ne ""){ if (scalar(keys %tmp)>1){ for $i (keys %tmp){ print "$i"};} %tmp=(); }; $p=$c; $tmp{$_}++;}' |  \
  perl -ane '@x=split(/;/);print "$x[0];$x[1]\n" if $x[3] eq "USR" && $x[4] == 0 && !($x[1] =~ m/\.\(none\)|\@none|\@think\.pad|\.local$|\@example.com|\@corpmail\.com|\@user-laptop|macbook-pro\.local|localhost\.localdomain|ubuntu\.\(none\)|users\.noreply\.github\.com|example\.com|imac\.local|ubuntu\.localdomain|ubuntu\.ubuntu-domain/)' |   perl $HOME/lookup/connectExportPreNoExclude.perl | gzip > a2ghQ.p2p.min
i=a2ghQ
k=12
zcat $i.p2p.min | perl -ane 'chop();@x=split(/;/);print "$_\n" if $#x<'$k';'|gzip > $i.p2p.$k
(zcat $i.p2p.$k |perl $HOME/lookup/connectExportSrt.perl $i;zcat $i.versions |  perl $HOME/lookup/connectPrune.perl |gzip > $i.versions1; zcat $i.versions1| ~/bin/connect | gzip > $i.clones; perl $HOME/lookup/connectImport.perl $i | gzip > $i.map.$k)



# Now use both
i=egh
zcat {E2aQ,a2ghQ}.p2p.$k | perl $HOME/lookup/connectExportSrt.perl $i
zcat $i.versions |  perl $HOME/lookup/connectPrune.perl |gzip > $i.versions1
zcat $i.versions1 | ~/bin/connect | gzip > $i.clones
perl $HOME/lookup/connectImport.perl $i | gzip > $i.map.$k

#calculate summaries for match
zcat $i.map.$k | awk -F\; '{print $2";"$1}' | perl -e '$p="";while(<STDIN>){($c,@x)=split(/;/); if ($c ne $p && $p ne ""){ if (scalar(keys %tmp)>1){ for $i (keys %tmp){ print "$i"};} %tmp=(); }; $p=$c; $tmp{$_}++;}' | perl -ane 's/;/\n/g;print' | gzip > $i.list

# Now use all three (ght has newer commits)
i=ghtE2aQ
zcat {E2aQ,ght2aQ,a2ghQ}.p2p.$k | perl $HOME/lookup/connectExportSrt.perl $i
zcat $i.versions |  perl $HOME/lookup/connectPrune.perl |gzip > $i.versions1
zcat $i.versions1 | ~/bin/connect | gzip > $i.clones
perl $HOME/lookup/connectImport.perl $i | gzip > $i.map.$k


#fix matching: exclude administrator@192.168 also @192.168  ubuntu@ip @Paula-PC -MacBook-Pro.local notebook.(none)
pivotal.io has tons of pairs, see cl="2872687" for ghtE2aQ.forML.top.gz: what to do about them? partial attribution?
table of elements for cl "3693783"?
cl "5116388" cluster 1 needs to be broken




#examples of problems with ght
zcat ght2aQ.gz|grep '^brun;' | cut -d\; -f2- | ~/lookup/getValues a2p
brun <brun@27541ba8-7e3a-0410-8455-c3a389f83636>;SimonHYLu_root;asanyal902_ROOT;bbannier_ROOT;bbockelm_root;bbockelm_root-historical;dawehner_root;leloulight_ROOT;pemryan_ROOT;sandrofonseca_ROOT;sunfirefox_root
brun <brun@brun-System-Product-Name>;Eeezzzddd_CodeChef
brun <brun@c85144d4-813a-0410-b911-affd6e4005a5>;snoopspy_reflex
brun <brun@laas.fr>;Olivier-LAAS_SMART
brun <brun@nceas.ucsb.edu>;NCEAS_2016-postdoc-training;NCEAS_ds-workshop-2015;NCEAS_training-jupyter-notebook;Science-for-Nature-and-People_ecological-drought
brun <brun@radar>;NUOG_Admiss-rating;holovchak_Admiss-rating
brun <brun@statim.fr>;sacav1_Ionic2-Calendar
brun <zhibo501@gmail.com>;zhibo501_c-project-demo;zhibo501_mockcpp;zhibo501_work_env;zhibo501_yFun



# see https://github.com/ssc-oscar/fingerprinting/blob/master/basicModel.r
# for how to process each block


#use the latest to create blocks
zcat egh.map.12 | awk -F\; '{print $2";"$1}' | perl $HOME/lookup/connectExportPreNoExclude.perl | gzip > egh.map.12.p2p
zcat egh.map.12.p2p | perl -ane '$i++;chop (); @x=split(/;/); for $a (@x){ $a=~ m/([^<]*)<([^>]*)>/; $n=$1;$e=$2;$n=~s/\s*$//;print "$i;$n;$e;$a\n"};' | gzip > forML.gz

