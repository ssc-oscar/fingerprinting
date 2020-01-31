library(randomForest)
library(RecordLinkage)
library(igraph)
load("rfBasicModel.RData")

alist = read.table("INPUT",sep=";",quote="",comment.char="")
names(alist) = c("clid", "n","e","a");
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
pairs = compare.linkage (alist[TRUE,c("n", "e", "ln1", "fn1", "un", "a")],
                         alist[TRUE,c("n", "e", "ln1", "fn1", "un", "a")],
                         exclude=c(6),strcmp=c(1:5),strcmpfun = jarowinkler)

pairs$pairs[is.na(pairs$pairs)] = 0
pv1 = predict(rfa1,pairs$pairs);
 

val = sum(pv1==1)-length(pv1);

if (is.na(val) || val < 0) 
{
    n=dim(pairs$data1)[1];
 	gg = make_empty_graph(n, directed = FALSE) # empty graph
  	gg = set_vertex_attr(gg, "id", value = 1:n) 
  	gg = add_edges(gg, as.vector(t(pairs$pairs[pv1==1,1:2]))); 
  	gg = simplify(gg,remove.multiple = TRUE, remove.loops = TRUE)
  	clustgg <- components(gg, "weak") # get clusters
  	blocksgg <- data.frame(id=V(gg)$id, block=clustgg$membership)
	for (cc in names(table(blocksgg$block)))
	{
		write(paste(c(cc,blocksgg$id[blocksgg$block == cc]),collapse=";"), file="OUTPUT", append=T);
	}
}else{
    write(paste(alist[1, 1],collapse=";"), file="OUTPUT", append=F);
}


