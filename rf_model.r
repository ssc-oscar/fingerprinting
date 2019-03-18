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

#inspect errors
res = c();
for (i in 0:9){
    mm1v = vv[mm][sel==i];
    mm0v = vv[nmm][sel1==i];
    mm0v = c(mm0v, vv[nnmm][sel2==i])
    dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln","fn","un","d2vSim","ad", "tdz", "ifn", "ln1f", "fnf", "ln1","fn1","im","id1","id2")];
    miss = dv[dv$im!=p[[i+1]],c("id1","id2","im")];
    ss = match(paste(pairs$pairs$id1, pairs$pairs$id2,sep=";"),paste(miss$id1,miss$id2,sep=";"),nomatch=0)>0;
    misd = pairs$pairs[ss,];
    aa = paste(pairs$data1[misd$id1,"a"],pairs$data2[misd$id2,"a"], sep=";");
    res = rbind(res, cbind(misd[,c("id1","id2","d2vSim","ad", "tdz","im")], aa));
}
#save
save(rf, p, res, sel, sel1, sel2, file = "/home/audris/rfmodels.RData");

res[res$im==0,c("id1","id2","d2vSim","ad","tdz","im")];

#manually correct errors
eq= c(1817022,2620775,3198135,3823436,4923380,2469101,5117770,429508 ,1674130,2018882,2469100,
      2863731,4667324,5063908,5351827,1674131 ,2566524,3924916,4669669,5028394,5066253,
      1814677,3198134,3305223,4454519,4603843,3647564,3943395,4452175,4667325,1425379,2552916,
      4452174,5026049,5030739,5115306,1814676,2021226,2550570);

pairs$pairs$im=as.factor(pairs$pairs$"is_match");
fr=c(1117,1771,2183,1632,2265,2265,1221, 666,1900,487,1053,1075,1085,1085,1207,1411,748, 1411, 1221, 1632, 1076, 1148, 748,  774,  775, 774, 1115, 1053, 981,  185, 1093, 1899, 2099, 373, 966, 1094, 184, 2100, 2144, 424, 425, 1450, 1228, 1116,  775, 1117, 1364, 774,  1086, 1900, 1118, 1227, 1227);
to=c(1411, 666, 980,1085, 749, 748,487,1772,1365,1221,2160,1207,1632,1631,1076,1117,2264,1118, 486,  1086, 1207, 528, 2264, 1992, 1992, 775, 1409, 2161, 2183, 373, 1096, 1365, 1225, 184, 1619,1096, 373, 1225, 2255, 229, 230, 1683, 2099, 1410, 1991, 1410, 1899, 1991, 1631, 1364, 1410, 2100, 2099);
for (i in c(1:length(fr))){
  pairs$pairs$im[pairs$pairs$id1==fr[i]&pairs$pairs$id2==to[i]]=1;
  pairs$pairs$im[pairs$pairs$id2==fr[i]&pairs$pairs$id1==to[i]]=1;
}                   
pp=pairs$pairs[eq,]
pp=pp[pp$im==0,]
paste(pairs$data1[pp$id1,"a"],pairs$data1[pp$id2,"a"])

for (i in c(1:length(pp$id1))){
  pairs$pairs$im[pairs$pairs$id1==pp$id1[i]&pairs$pairs$id2==pp$id2[i]]=1;
  pairs$pairs$im[pairs$pairs$id2==pp$id1[i]&pairs$pairs$id1==pp$id2[i]]=1;
}                   

pp=pairs$pairs[pairs$pairs$im!=pairs$pairs$"is_match",];
pp=pp[pp$im==1&pp$id1<pp$id2,];
paste(pairs$data1[pp$id1,"a"],pairs$data1[pp$id2,"a"]);
 

#Fit again after correcting errors
mmC = pairs$pairs$im == 1;
nmmC=!mmC & apply(pairs$pairs[,c("n","e","ln","fn","un","ifn","ln1","fn1")], 1, max) > .8 | pairs$pairs$ad>0|pairs$pairs$tdz>.9|pairs$pairs$d2vSim>0;
nnmmC=!mmC & !nmmC
selC=sample(0:9,sum(mmC),replace=T);
selC1=sample(0:9,sum(nmmC),replace=T);
selC2=sample(0:9,sum(nnmmC),replace=T);
rfC = list();
pC = list();
for (i in 0:9){
  mm1t = vv[mmC][selC!=i];
  mm1v = vv[mmC][selC==i];
  mm0t = vv[nmmC][selC1!=i];
  #mm0t = c(mm0t, vv[nnmmC][selC2!=i]) #mm0t = c(mm0t, vv[nnmmC&fullP!=pairs$pairs$im])          
  mm0v = vv[nmmC][selC1==i];
  mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
  dt=pairs$pairs[c(mm1t,mm0t),c("n","e","ln","fn","un","d2vSim", "ad", "tdz", "ifn", "ln1f", "fnf", "ln1","fn1", "im")];
  dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln","fn","un","d2vSim","ad", "tdz", "ifn", "ln1f", "fnf", "ln1","fn1","im")];
  rfC[[i+1]] = randomForest(im ~ ., dt, importance=T);
  pC[[i+1]] = predict(rfC[[i+1]],dv);
  print (table(pC[[i+1]],dv$im));
}

resC = c();
for (i in 0:9){
  mm1v = vv[mmC][selC==i];
  mm0v = vv[nmmC][selC1==i];
  mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
  dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln","fn","un","d2vSim","ad", "tdz", "ifn", "ln1f", "fnf", "ln1","fn1","im","id1","id2")];
  miss = dv[dv$im!=pC[[i+1]],c("id1","id2","im")];
  ss = match(paste(pairs$pairs$id1, pairs$pairs$id2,sep=";"),paste(miss$id1,miss$id2,sep=";"),nomatch=0)>0;
  misd = pairs$pairs[ss,];
  aa = paste(pairs$data1[misd$id1,"a"],pairs$data2[misd$id2,"a"], sep=";");
  resC = rbind(resC, cbind(misd[,c("id1","id2","d2vSim","ad", "tdz","im")], aa));
}
#save
save(rfC, pC, resC, selC, selC1, selC2, file = "/home/audris/rfmodelsC.RData");
load(file = "/home/audris/rfmodelsC.RData");

resC[resC$im==0,];


#try simpler models that can be applied on the entire universe
rfCS = list();
pCS = list();
for (i in 0:9){
  mm1t = vv[mmC][selC!=i];
  mm1v = vv[mmC][selC==i];
  mm0t = vv[nmmC][selC1!=i];
  #mm0t = c(mm0t, vv[nnmmC][selC2!=i]) #mm0t = c(mm0t, vv[nnmmC&fullP!=pairs$pairs$im])          
  mm0v = vv[nmmC][selC1==i];
  mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
  dt=pairs$pairs[c(mm1t,mm0t),c("n","e","ln","fn","un", "ifn", "d2vSim", "im")];
  dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln","fn","un", "ifn", "d2vSim", "im")];
  rfCS[[i+1]] = randomForest(im ~ ., dt, importance=T);
  pCS[[i+1]] = predict(rfCS[[i+1]],dv);
  print (table(pCS[[i+1]],dv$im));
}

resCS = c();
for (i in 0:9){
  mm1v = vv[mmC][selC==i];
  mm0v = vv[nmmC][selC1==i];
  mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
  dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln","fn","un","d2vSim","ad", "tdz", "ifn", "ln1f", "fnf", "ln1","fn1","im","id1","id2")];
  miss = dv[dv$im!=pCS[[i+1]],c("id1","id2","im")];
  ss = match(paste(pairs$pairs$id1, pairs$pairs$id2,sep=";"),paste(miss$id1,miss$id2,sep=";"),nomatch=0)>0;
  misd = pairs$pairs[ss,];
  aa = paste(pairs$data1[misd$id1,"a"],pairs$data2[misd$id2,"a"], sep=";");
  resCS = rbind(resCS, cbind(misd[,c("id1","id2","d2vSim","im")], aa));
}
save(rfCS, pCS, resCS, pairs, file = "/home/audris/rfmodelsCS.RData");

#try even simpler model without the fingerprints
rfCSB = list();
pCSB = list();
for (i in 0:9){
  mm1t = vv[mmC][selC!=i];
  mm1v = vv[mmC][selC==i];
  mm0t = vv[nmmC][selC1!=i];
  #mm0t = c(mm0t, vv[nnmmC][selC2!=i]) #mm0t = c(mm0t, vv[nnmmC&fullP!=pairs$pairs$im])          
  mm0v = vv[nmmC][selC1==i];
  mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
  dt=pairs$pairs[c(mm1t,mm0t),c("n","e","ln","fn","un", "ifn", "im")];
  dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln","fn","un", "ifn", "im")];
  rfCSB[[i+1]] = randomForest(im ~ ., dt, importance=T);
  pCSB[[i+1]] = predict(rfCSB[[i+1]],dv);
  print (table(pCSB[[i+1]],dv$im));
}
