tos all
#try random forest
library(randomForest);

pairs <- readRDS("../MODELS_PHASE5/match2345_pairs.RDS");

pairs$pairs$im = as.factor(pairs$pairs$"is_match");
mm = pairs$pairs$"is_match" == 1;
nmm=!mm & apply(pairs$pairs[,c("n","e","ln","fn","un","ifn","ln1","fn1")], 1, max) > .8 | pairs$pairs$ad>0|pairs$pairs$tdz>.9|pairs$pairs$d2vSim>0;
nnmm=!mm & !nmm
vv = 1:dim(pairs$pairs)[1];


#Fit RF Model with fingerprints

rf = list();
p = list();
sel=sample(0:9,sum(mm),replace=T);
sel1=sample(0:9,sum(nmm),replace=T);
sel2=sample(0:9,sum(nnmm),replace=T);

for (i in 0:9){
  mm1t = vv[mm][sel!=i];
  mm1v = vv[mm][sel==i];
  mm0t = vv[nmm][sel1!=i];
  mm0v = vv[nmm][sel1==i];
  mm0v = c(mm0v, vv[nnmm][sel2==i])
  dt=pairs$pairs[c(mm1t,mm0t),c("n","e","ln","fn","un","d2vSim", "ad", "tdz", "ifn", "ln1f", "fnf", "ln1","fn1", "im")];
  dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln","fn","un","d2vSim","ad", "tdz", "ifn", "ln1f", "fnf", "ln1","fn1","im")];
  rf[[i+1]] = randomForest(im ~ ., dt, importance=T);
  p[[i+1]] = predict(rf[[i+1]],dv);
  print (table(p[[i+1]],dv$im));
}
