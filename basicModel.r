#######################
See prep.sh in da0:/data/play/idRes

#############################
#first fit a very basic model
#############################


load("/home/audris/rfmodelsCS.RData");
#get reasonable sample of pairs with/without match
mmC = pairs$pairs$im == 1;
nmmC=!mmC & apply(pairs$pairs[,c("n","e","ln","fn","un","ifn","ln1","fn1")], 1, max) > .8;
selC=sample(0:9,sum(mmC),replace=T);
selC1=sample(0:9,sum(nmmC),replace=T);
vv = 1:dim(pairs$pairs)[1];
i=0
mm1t = vv[mmC][selC!=i];
mm1v = vv[mmC][selC==i];
mm0t = vv[nmmC][selC1!=i];
mm0v = vv[nmmC][selC1==i];

dt=pairs$pairs[c(mm1t,mm0t),c("n","e","ln","fn","un", "im")];
dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln","fn","un", "im")];
rf=randomForest(im ~ ., dt, importance=T);
pv = predict(rf,dv);
print (table(pv,dv$im));
i=0
pv     0    1
  0 5634    2
  1    4  548


i=3
pv     0    1
  0 5695    0
  1    8  573

i=5
pv     0    1
  0 5552    3
  1    6  575

i=8
pv     0    1
  0 5520    1
  1    7  589


da=pairs$pairs[c(mm1t,mm0t,mm1v,mm0v),c("n","e","ln","fn","un", "im")];
rfa=randomForest(im ~ ., da, importance=T);
pv = predict(rfa,da);
print (table(pv,da$im));
pv      0     1
  0 55811    22
  1    56  5611

# this model uses camelcase to separate the names
da1=pairs$pairs[c(mm1t,mm0t,mm1v,mm0v),c("n","e","ln1","fn1","un", "im")];
rfa1=randomForest(im ~ ., da1, importance=T);
pv1 = predict(rfa1,da1);
print (table(pv,da1$im));

pv1      0     1
  0 55809     8
  1    58  5625

#now save all rthree training on i=0, full in rfa and in rfa1 using the camel case
save(rfa, rfa1, rf, file = "/home/audris/rfBasicModel.RData");



#############################
#now prepare blocks from the gh handle + shared email
#############################
i=egh
k=12

zcat $i.map.$k | awk -F\; '{print $2";"$1}' | lsort 50G -t\; -k1,2 | perl -e '$p="";while(<STDIN>){chop();($c,@x)=split(/;/); if ($c ne $p && $p ne ""){ for $i (keys %tmp){ print ";$i"}; print "\n"; %tmp=(); }; $p=$c; $tmp{$p}++;$tmp{$x[0]}++;}' | gzip > egh.map.12.p2p
zcat egh.map.12.p2p | sed 's|^;||' | perl -ane '$i++;chop ();s/\r//g; @x=split(/;/); for $a (@x){ $a=~ m/([^<]*)<([^>]*)>/; $n=$1;$e=$2;$n=~s/\s*$//;print "$i;$n;$e;$a\n"};' | gzip > forML.gz


library(randomForest)
library(RecordLinkage)
load("/home/audris/rfBasicModel.RData")


alist = read.table("forML.gz",sep=";",quote="",comment.char="")
names(alist) = c("cl","n","e","a");
alist$ln=sub(".* ","",alist$n);
alist$fn=sub(" .*","",alist$n);
alist$n1=sub("([a-z])([A-Z])","\\1 \\2",alist$n,fixed=FALSE) #handle camel case
alist$ln1=sub(".*[ _+-,.]","",alist$n1)
alist$fn1=sub("[ _+-,.].*","",alist$n1);
alist$un=sub("@.*","",alist$e);
#matching first to last
alist$ifn = alist$ln;
alist$iln = alist$fn;
alist$ifn1 = alist$ln1;
alist$iln1 = alist$fn1;
#get names of clusters
vals=names(table(alist$cl))

res=c();
for (cl in vals){
 sel=alist$cl==cl;
 pairs = compare.linkage (alist[sel,c("n", "e", "ln1", "fn1", "un", "a")],
                         alist[sel,c("n", "e", "ln1", "fn1", "un", "a")],
                         exclude=c(6),strcmp=c(1:5),strcmpfun = jarowinkler)
 pairs$pairs[is.na(pairs$pairs)] = 0
 pv1 = predict(rfa1,pairs$pairs);
 val = sum(pv1==1)-length(pv1);
 if (is.na(val) || val < 0) {
   print (c(cl,val));
   res = c(res, cl);
 }
}

#####################
# Now the clusters that are connected stay connectsed, but what about the rest?
# Separated into conected subsets -> output connected subsets 

#this code does just that
library(igraph)
for (cl in res){
 sel=alist$cl==cl;
 pairs = compare.linkage (alist[sel,c("n", "e", "ln1", "fn1", "un", "a")],
                         alist[sel,c("n", "e", "ln1", "fn1", "un", "a")],
                         exclude=c(6),strcmp=c(1:5),strcmpfun = jarowinkler)
 pairs$pairs[is.na(pairs$pairs)] = 0;
 pv1 = predict(rfa1,pairs$pairs);
 n=dim(pairs$data1)[1];
 gg = make_empty_graph(n, directed = FALSE) # empty graph
 gg = set_vertex_attr(gg, "id", value = 1:n) 
 gg = add_edges(gg, as.vector(t(pairs$pairs[pv1==1,1:2]))); 
 gg = simplify(gg,remove.multiple = TRUE, remove.loops = TRUE)
 clustgg <- components(gg, "weak") # get clusters
 blocksgg <- data.frame(id=V(gg)$id, block=clustgg$membership)
 for (cc in names(table(blocksgg$block))){
   #pairs$data1[blocksgg$id[blocksgg$block == cc],"a"]
   write(paste(c(cl,blocksgg$id[blocksgg$block == cc]),collapse=";"), file="out", append=T);
 }
}



