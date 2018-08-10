#try random forest
library(randomForest);
pairs <- readRDS("../MODELS_PHASE5/match2345_pairs.RDS");
pairs$pairs$im = as.factor(pairs$pairs$"is_match");
mm = pairs$pairs$"is_match" == 1;
nmm=!mm & apply(pairs$pairs[,c("n","e","ln","fn","un","ifn","ln1","fn1")], 1, max) > .8 | pairs$pairs$ad>0|pairs$pairs$tdz>.9|pairs$pairs$d2vSim>0;
nnmm=!mm & !nmm
vv = 1:dim(pairs$pairs)[1];

#a=paste(pairs$pairs$id1,pairs$pairs$id2,sep=";");
#b=paste(pairs$pairs$id2,pairs$pairs$id1,sep=";");
#ab=match(a,b);
#sum(pairs$pairs$im!=pairs$pairs$im[ab])

#Fit
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
         0      1
  0 549693      1
  1     11   1028

         0      1
  0 548507      1
  1     16    999

         0      1
  0 549742      0
  1     20    989

         0      1
  0 549492      3
  1     14   1046

         0      1
  0 549225      3
  1     15   1010

         0      1
  0 549840      0
  1     20   1016

         0      1
  0 550595      1
  1     12   1086

         0      1
  0 548972      3
  1     15   1060

         0      1
  0 548861      2
  1     13   1032



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
         id1  id2    d2vSim           ad          tdz im
1817022  775 1992 0.0000000 0.0000000000  0.992805755  0
2620775 1118 1410 0.0000000 0.0000000000  0.782660190  0
3198135 1364 1900 0.0000000 0.0000000000 -0.007196270  0
3823436 1631 1086 0.0000000 0.0000000000 -0.011670606  0
4923380 2100 1225 0.0000000 0.0447984072  0.768176577  0
2469101 1053 2161 0.5482726 0.0000000000  0.094913538  0
5117770 2183  980 0.7300389 0.0000000000  0.992805755  0
429508   184  373 0.0000000 2.9642246777  0.985368371  0
1674130  714 2145 0.0000000 0.0000000000 -0.007170286  0
2018882  861 2182 0.0000000 0.0000000000  0.341861193  0
2469100 1053 2160 0.7403789 6.3988278451  0.247812187  0
2863731 1222  486 0.0000000 0.0000000000 -0.007194255  0
4667324 1991  774 0.0000000 0.0000000000  0.630051985  0
5063908 2160 1053 0.7403789 6.3988278451  0.247812187  0
5351827 2283  537 0.0000000 0.0000000000 -0.007183737  0
1674131  714 2146 0.0000000 0.0000000000 -0.007170286  0
2566524 1095 1094 0.0000000 0.0000000000  0.548521202  0
3924916 1674 1731 0.0000000 0.0000000000 -0.007194245  0
4669669 1992  774 0.0000000 0.0000000000  0.992805755  0
5028394 2145  714 0.0000000 0.0000000000 -0.007170286  0
5066253 2161 1053 0.5482726 0.5440382033  0.094913538  0
1814677  774 1992 0.0000000 0.0000000000  0.992805755  0
3198134 1364 1899 0.0000000 0.0000000000 -0.006953688  0
3305223 1410 1118 0.0000000 0.0000000000  0.782660190  0
4454519 1900 1364 0.0000000 0.0000000000 -0.007196270  0
4603843 1964  608 0.0000000 0.0000000000  0.391344878  0
3647564 1556 1089 0.0000000 0.0000000000  0.081967324  0
3943395 1682 1450 0.0000000 0.0217592593  0.992805755  0
4452175 1899 1365 0.0000000 0.0005681818 -0.007611807  0
4667325 1991  775 0.0000000 0.0000000000  0.630051985  0
1425379  608 1964 0.0000000 0.0000000000  0.391344878  0
2552916 1089 1556 0.0000000 0.0000000000  0.081967324  0
4452174 1899 1364 0.0000000 0.0000000000 -0.006953688  0
5026049 2144  714 0.0000000 0.0000000000 -0.007170286  0
5030739 2146  714 0.0000000 0.0000000000 -0.007170286  0
5115306 2182  861 0.0000000 0.0000000000  0.341861193  0
1814676  774 1991 0.0000000 0.0000000000  0.630051985  0
2021226  862 2181 0.0000000 0.0000000000 -0.007801478  0
2550570 1088 1555 0.0000000 0.0000000000  0.783570825  0



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
 [1] "yong sheng gong <gong.yongsheng@99cloud.net> yong sheng gong <gongysh@cn.ibm.com>"
 [2] "hayderimran7 <imran@cloudscaling.com> imran <imran@cloudscaling.com>"
 [3] "abhiram_moturi <abhiram.moturi@oracle.com> abhiram moturi <abhiram.moturi@oracle.com>"
 [4] "Elizabeth Elwell <e.r.elwell@gmail.com> Beth Elwell <e.r.elwell@gmail.com>"
 [5] "yong sheng gong <gong.yongsheng@99cloud.net> Yong Sheng Gong <gongysh@unitedstack.com>"
 [6] "fabien Boucher <fabien.boucher@enovance.com> Fabien Boucher <fabien.dot.boucher@gmail.com>"
 [7] "Zara <zara.zaimeche@codethink.co.uk> Zahra <zahra.zahra@tcs.com>"
 [8] "yong sheng gong <gongysh@cn.ibm.com> yong sheng gong <gong.yongsheng@99cloud.net>"
 [9] "Joe Talerico <joe.talerico@gmail.com> Joe <joe.talerico@gmail.com>"
[10] "K Jonathan Harker <k.jonathan.harker@hp.com> Jonathan Harker <k.jonathan.harker@hp.com>"
[11] "Anju Tiwari <anju.tiwari@nectechnologies.in> Anju Tiwari <anjutiwari5@gmail.com>"
[12] "Joe <joe.talerico@gmail.com> Joe Talerico <joe.talerico@gmail.com>"
[13] "Jonathan Harker <k.jonathan.harker@hp.com> K Jonathan Harker <k.jonathan.harker@hp.com>"
[14] "Yong Sheng Gong <gongysh@cn.ibm.com> yong sheng gong <gong.yongsheng@99cloud.net>"
[15] "Yong Sheng Gong <gongysh@unitedstack.com> yong sheng gong <gong.yongsheng@99cloud.net>"
[16] "imran <imran@cloudscaling.com> hayderimran7 <imran@cloudscaling.com>"
[17] "hayderimran7 <hayderimran7@gmail.com> imran <hayderimran7@gmail.com>"
[18] "Jonathan Harker <code@gentlydownthe.net> K Jonathan Harker <code@gentlydownthe.net>"

for (i in c(1:length(pp$id1))){
  pairs$pairs$im[pairs$pairs$id1==pp$id1[i]&pairs$pairs$id2==pp$id2[i]]=1;
  pairs$pairs$im[pairs$pairs$id2==pp$id1[i]&pairs$pairs$id1==pp$id2[i]]=1;
}                   

pp=pairs$pairs[pairs$pairs$im!=pairs$pairs$"is_match",];
pp=pp[pp$im==1&pp$id1<pp$id2,];
paste(pairs$data1[pp$id1,"a"],pairs$data1[pp$id2,"a"]);
 [1] "Victoria Martinez de la Cruz <victoria@vmartinezdelacruz.com> Victoria Martinez de la Cruz <victoria@redhat.com>"       
 [2] "Victoria Mart\303\255nez de la Cruz <victoria@vmartinezdelacruz.com> Victoria Martinez de la Cruz <victoria@redhat.com>"
 [3] "ghanshyam <ghanshyammann@gmail.com> Ghanshyam Mann <ghanshyammann@gmail.com>"                                           
 [4] "ghanshyam <ghanshyam.mann@nectechnologies.in> Ghanshyam Mann <ghanshyam.mann@nectechnologies.in>"                       
 [5] "abhiram moturi <abhiram.moturi@oracle.com> amoturi <abhiram.moturi@oracle.com>"                                         
 [6] "abhiram moturi <abhiram.moturi@oracle.com> abhiram_moturi <abhiram.moturi@oracle.com>"                                  
 [7] "Abhiram Moturi <abhiram.moturi@oracle.com> amoturi <abhiram.moturi@oracle.com>"                                         
 [8] "Sergey Vasilenko <svasilenko@mirantis.com> vsaienko <vsaienko@mirantis.com>"                                            
 [9] "Beth Elwell <e.r.elwell@gmail.com> Elizabeth Elwell <e.r.elwell@gmail.com>"                                             
[10] "Joe <joe.talerico@gmail.com> Joe Talerico <joe.talerico@gmail.com>"                                                     
[11] "Sukhdev Kapur <sukhdev@aristanetworks.com> sukhdev <sukhdev@aristanetworks.com>"                                        
[12] "Sukhdev Kapur <sukhdev@aristanetworks.com> Sukhdev <sukhdev@aristanetworks.com>"                                        
[13] "yong sheng gong <gong.yongsheng@99cloud.net> Yong Sheng Gong <gongysh@cn.ibm.com>"                                      
[14] "yong sheng gong <gong.yongsheng@99cloud.net> yong sheng gong <gongysh@cn.ibm.com>"                                      
[15] "yong sheng gong <gong.yongsheng@99cloud.net> Yong Sheng Gong <gongysh@unitedstack.com>"                                 
[16] "Lingxian Kong <anlin.kong@gmail.com> lingxiankong <anlin.kong@gmail.com>"                                               
[17] "Lingxian Kong <anlin.kong@gmail.com> LingxianKong <konglingxian@huawei.com>"                                            
[18] "Lingxian Kong <konglingxian@huawei.com> LingxianKong <konglingxian@huawei.com>"                                         
[19] "pcarlton <paul.carlton2@hp.com> pcarlton <paul.carlton2@hpe.com>"                                                       
[20] "pcarlton <paul.carlton2@hp.com> paul-carlton2 <paul.carlton2@hp.com>"                                                   
[21] "paul-carlton2 <paul.carlton2@hpe.com> pcarlton <paul.carlton2@hpe.com>"                                                 
[22] "paul-carlton2 <paul.carlton2@hpe.com> paul-carlton2 <paul.carlton2@hp.com>"                                             
[23] "hayderimran7 <imran@cloudscaling.com> imran <imran@cloudscaling.com>"                                                   
[24] "hayderimran7 <hayderimran7@gmail.com> imran <hayderimran7@gmail.com>"                                                   
[25] "watanabe.fumitaka <watanabe.fumitaka1@gmail.com> watanabe.fumitaka <watanabe.fumitaka@nttcom.co.jp>"                    
[26] "David Purcell <david.purcell@att.com> David Purcell <dp612u@att.com>"                                                   
[27] "David Purcell <d.purcell222@gmail.com> David Purcell <dp612u@att.com>"                                                  
[28] "Salvatore Orlando <salv.orlando@gmail.com> Salvatore <salv.orlando@gmail.com>"                                          
[29] "Salvatore Orlando <salv.orlando@gmail.com> salvatore <salv.orlando@gmail.com>"                                          
[30] "janki <jchhatba@redhat.com> Janki Chhatbar <jchhatba@redhat.com>"                                                       
[31] "Janki <jchhatba@redhat.com> Janki Chhatbar <jchhatba@redhat.com>"                                                       
[32] "Ulrik Sjolin <ulrik.sjolin@gmail.com> Ulrik Sjolin <ulrik.sjolin@sonyericsson.com>"                                     
[33] "Ulrik Sjolin <ulrik.sjolin@gmail.com> Ulrik Sj\303\266lin <ulrik.sjolin@sonyericsson.com>"                              
[34] "Ulrik Sj\303\266lin <ulrik.sjolin@gmail.com> Ulrik Sjolin <ulrik.sjolin@sonyericsson.com>"                              
[35] "Ulrik Sj\303\266lin <ulrik.sjolin@gmail.com> Ulrik Sj\303\266lin <ulrik.sjolin@sonyericsson.com>"                       
[36] "Jonathan Harker <code@gentlydownthe.net> K Jonathan Harker <code@gentlydownthe.net>"                                    
[37] "Jonathan Harker <k.jonathan.harker@hp.com> K Jonathan Harker <k.jonathan.harker@hp.com>"                                
[38] "Fabien Boucher <fboucher@redhat.com> Fabien Boucher <fabien.boucher@enovance.com>"                                      
[39] "Fabien Boucher <fabien.dot.boucher@gmail.com> fabien Boucher <fabien.boucher@enovance.com>"                             
[40] "Fabien Boucher <fabien.dot.boucher@gmail.com> Fabien Boucher <fabien.boucher@enovance.com>"                             
[41] "Emilien Macchi <emilien@enovance.com> emilienm <emilien@enovance.com>"                                                  
[42] "Emilien Macchi <emilien.macchi@stackops.com> EmilienM <emilien.macchi@enovance.com>"                                    
[43] "Emilien Macchi <emilien.macchi@enovance.com> EmilienM <emilien.macchi@enovance.com>"                                    
[44] "Emilien Macchi <emilien.macchi@enovance.com> emilienm <emilien.macchi@enovance.com>"                                    
[45] "\303\211milien Macchi <emilien.macchi@enovance.com> EmilienM <emilien.macchi@enovance.com>"                             
[46] "\303\211milien Macchi <emilien.macchi@enovance.com> emilienm <emilien.macchi@enovance.com>"                             
[47] "Kevin Benton <kevin@benton.pub> Mr. Bojangles <kevin@benton.pub>"                                                       
[48] "Kevin Benton <kevin@benton.pub> Mr. Bojangles <kevinbenton@buttewifi.com>"                                              
[49] "Kevin Benton <kevinbenton@buttewifi.com> Mr. Bojangles <kevin@benton.pub>"                                              
[50] "Kevin Benton <kevinbenton@buttewifi.com> Mr. Bojangles <kevinbenton@buttewifi.com>"                                     
[51] "Kevin Benton <kevin.benton@bigswitch.com> Mr. Bojangles <kevin@benton.pub>"                                             
[52] "irina povolotskaya <ipovolotskaya@mirantis.com> Irina <ipovolotskaya@mirantis.com>"                                     
[53] "irina povolotskaya <ipovolotskaya@mirantis.com> ipovolotskaya <ipovolotskaya@mirantis.com>"                             
[54] "Irina Povolotskaya <ipovolotskaya@mirantis.com> Irina <ipovolotskaya@mirantis.com>"                                     
[55] "Irina Povolotskaya <ipovolotskaya@mirantis.com> ipovolotskaya <ipovolotskaya@mirantis.com>"                             
[56] "Anju Tiwari <anjutiwari5@gmail.com> Anju Tiwari <anju.tiwari@nectechnologies.in>"                                       
[57] "Anju Tiwari <anjutiwari5@gmail.com> anju tiwari <anju.tiwari@nectechnologies.in>"                                       
[58] "Zara <zara.zaimeche@codethink.co.uk> Zahra <zahra.zahra@tcs.com>"                                                       
[59] "Yong Sheng Gong <gongysh@cn.ibm.com> gongysh <gongysh@cn.ibm.com>"                                                       


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
         0      1
  0 549609      3
  1      0    993

         0      1
  0 548181      3
  1      2   1108

         0      1
  0 549470      4
  1      0   1079

         0      1
  0 551139      2
  1      0   1039

         0      1
  0 550112      5
  1      1   1012

         0      1
  0 549211      1
  1      3   1067

         0      1
  0 549404      1
  1      0   1032

         0      1
  0 547961      3
  1      0   1019

         0      1
  0 548730      2
  1      0   1086

         0      1
  0 549569      2
  1      0   1010

#very nice results!

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
         id1  id2 d2vSim       ad       tdz im
128525    55 1895      0 5.690182 0.9928058  0
4441485 1895   55      0 5.690182 0.9928058  0
1202126  513 1486      0 0.000000 0.9840866  0
3401933 1451 1683      0 0.000000 0.9928058  0
3482838 1486  513      0 0.000000 0.9840866  0
3945741 1683 1451      0 0.000000 0.9928058  0
                                                                                      aa
128525    alevine <alevine@cloudscaling.com>;Alexandre Levine <alevine@cloudscaling.com>
4441485   Alexandre Levine <alevine@cloudscaling.com>;alevine <alevine@cloudscaling.com>
1202126                  Newptone <yuxcer@gmail.com>;Newptone <xingchao@unitedstack.com>
3401933 anju Tiwari <anjutiwari5@gmail.com>;anju tiwari <anju.tiwari@nectechnologies.in>
3482838                  Newptone <xingchao@unitedstack.com>;Newptone <yuxcer@gmail.com>
3945741 anju tiwari <anju.tiwari@nectechnologies.in>;anju Tiwari <anjutiwari5@gmail.com>

resC[resC$im==1,]
          id1  id2 d2vSim         ad          tdz im
4286144  1828 1829      0 0.00000000  0.992805755  1
4511360  1924 1925      0 0.00000000  0.511877231  1
4513704  1925 1924      0 0.00000000  0.511877231  1
145454     63   64      0 0.00000000  0.265827278  1
4316638  1841 1838      0 0.08396192 -0.007194245  1
37540      17   20      0 0.00000000 -0.009652092  1
1916682   818  817      0 0.00000000  0.992805755  1
3781757  1613 1617      0 0.00000000  0.992783249  1
5088476  2170 2171      0 0.00000000  0.992805755  1
147798     64   63      0 0.00000000  0.265827278  1
2177091   929  931      0 0.00000000 -0.009083949  1
1327838   567  568      0 0.00000000  0.494679582  1
2181779   931  929      0 0.00000000 -0.009083949  1
4288487  1829 1827      0 0.00000000  0.093287908  1
50884761 2170 2171      0 0.00000000  0.992805755  1
5090820  2171 2170      0 0.00000000  0.992805755  1
1330182   568  567      0 0.00000000  0.494679582  1
50908201 2171 2170      0 0.00000000  0.992805755  1
1914338   817  818      0 0.00000000  0.992805755  1
3791133  1617 1613      0 0.00000000  0.992783249  1
42861441 1828 1829      0 0.00000000  0.992805755  1
37817571 1613 1617      0 0.00000000  0.992783249  1
37911331 1617 1613      0 0.00000000  0.992783249  1
19143381  817  818      0 0.00000000  0.992805755  1
19166821  818  817      0 0.00000000  0.992805755  1
                                                                                         aa
4286144                Hua ZHANG <zhuadl@cn.ibm.com>;Zhang Hua <joshua.zhang@canonical.com>
4511360                 Matthieu Huin <mhu@enovance.com>;Marius Cornea <mcornea@redhat.com>
4513704                 Marius Cornea <mcornea@redhat.com>;Matthieu Huin <mhu@enovance.com>
145454                      root <rahulunair@gmail.com>;root <root@unrahul-box.localdomain>
4316638    Reedip Banerjee <reedip.banerjee@nectechnologies.in>;Reedip <reedip14@gmail.com>
37540                    Eric Peterson <eric.peterson@charter.com>;eric <ejpetey@gmail.com>
1916682  Masayuki Nakamura <m.nakamura_2@jp.fujitsu.com>;Jin Hase <hase.jin@jp.fujitsu.com>
3781757       lawrancejing <lawrancejing@gamil.com>;jing.liuqing <jing.liuqing@99cloud.net>
5088476                        root <nish.patwa@intel.com>;root <root@aio1.openstack.local>
147798                      root <root@unrahul-box.localdomain>;root <rahulunair@gmail.com>
2177091         dimtruck <dimalg@yahoo.com>;Dimitry Ushakov <dimitry.ushakov@rackspace.com>
1327838          andrewbogott <abogott@wikimedia.org>;Andrew Bogott <ABogott@WikiMedia.org>
2181779         Dimitry Ushakov <dimitry.ushakov@rackspace.com>;dimtruck <dimalg@yahoo.com>
4288487                Zhang Hua <joshua.zhang@canonical.com>;ZHANG Hua <zhuadl@cn.ibm.com>
50884761                       root <nish.patwa@intel.com>;root <root@aio1.openstack.local>
5090820                        root <root@aio1.openstack.local>;root <nish.patwa@intel.com>
1330182          Andrew Bogott <ABogott@WikiMedia.org>;andrewbogott <abogott@wikimedia.org>
50908201                       root <root@aio1.openstack.local>;root <nish.patwa@intel.com>
1914338  Jin Hase <hase.jin@jp.fujitsu.com>;Masayuki Nakamura <m.nakamura_2@jp.fujitsu.com>
3791133       jing.liuqing <jing.liuqing@99cloud.net>;lawrancejing <lawrancejing@gamil.com>
42861441               Hua ZHANG <zhuadl@cn.ibm.com>;Zhang Hua <joshua.zhang@canonical.com>
37817571      lawrancejing <lawrancejing@gamil.com>;jing.liuqing <jing.liuqing@99cloud.net>
37911331      jing.liuqing <jing.liuqing@99cloud.net>;lawrancejing <lawrancejing@gamil.com>
19143381 Jin Hase <hase.jin@jp.fujitsu.com>;Masayuki Nakamura <m.nakamura_2@jp.fujitsu.com>
19166821 Masayuki Nakamura <m.nakamura_2@jp.fujitsu.com>;Jin Hase <hase.jin@jp.fujitsu.com>


#See if the method works with the predictors used in bagging
"n"        "e"        "ln1"      "fn1"      "un"       "ifn"      "ln1f"     "fn1f"     "ad"       "tdz"      "d2vSim"    
rfC1 = list();
pC1 = list();
for (i in 0:9){
  mm1t = vv[mmC][selC!=i];
  mm1v = vv[mmC][selC==i];
  mm0t = vv[nmmC][selC1!=i];
  #mm0t = c(mm0t, vv[nnmmC][selC2!=i]) #mm0t = c(mm0t, vv[nnmmC&fullP!=pairs$pairs$im])          
  mm0v = vv[nmmC][selC1==i];
  mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
  dt=pairs$pairs[c(mm1t,mm0t),c("n","e","ln1","fn1","un","d2vSim", "ad", "tdz", "ifn", "ln1f", "fn1f", "im")];
  dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln1","fn1","un","d2vSim", "ad", "tdz", "ifn", "ln1f", "fn1f", "im")];
  rfC1[[i+1]] = randomForest(im ~ ., dt, importance=T);
  pC1[[i+1]] = predict(rfC1[[i+1]],dv);
  print (table(pC1[[i+1]],dv$im));
}

         0      1
  0 549609      4
  1      0    992

         0      1
  0 548181      3
  1      2   1108

         0      1
  0 549470      3
  1      0   1080

         0      1
  0 551139      5
  1      0   1036

         0      1
  0 550111      5
  1      2   1012

         0      1
  0 549211      1
  1      3   1067
   
         0      1
  0 549403      1
  1      1   1032
   
         0      1
  0 547961      5
  1      0   1017
   
         0      1
  0 548730      5
  1      0   1083

         0      1
  0 549569      2
  1      0   1010

save(rfC1, pC1, file = "/home/audris/rfmodelsC1.RData");
##############################################################################################################
#load ..
#do errors under transitive closure:
#Below is final
n=dim(pairs$data1)[1];
pC1t = pC1
for (i in 0:9){
  mm1v = vv[mmC][selC==i];
  mm0v = vv[nmmC][selC1==i];
  mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
  dv=pairs$pairs[c(mm1v,mm0v),c("id1","id2","n","e","ln1","fn1","un","d2vSim", "ad", "tdz", "ifn", "ln1f", "fn1f", "im")];
  crrf = closeG (n, dv[pC1[[i+1]]==1,c("id1","id2")]);
  idm = paste(dv[,"id1"],dv[,"id2"],sep=";")  
  pC1t[[i+1]][match(idm,paste(crrf[,1],crrf[,2],sep=";"),nomatch=0)>0] = 1;               

  crrf = closeG (n, dv[dv$im==1,c("id1","id2")]);
  imt = dv$im;
  idm = paste(dv[,"id1"],dv[,"id2"],sep=";")  
  imt[match(idm,paste(crrf[,1],crrf[,2],sep=";"),nomatch=0)>0] = 1;               
  print(table(pC1t[[i+1]],imt))
}
         0      1
  0 549609      4
  1      0    992
   imt
         0      1
  0 548179      3
  1      2   1110
   imt
         0      1
  0 549469      2
  1      0   1082
   imt
         0      1
  0 551136      5
  1      0   1039
   imt
         0      1
  0 550108      5
  1      3   1014
   imt
         0      1
  0 549204      1
  1      2   1075
   imt
         0      1
  0 549402      1
  1      1   1033
   imt
         0      1
  0 547958      4
  1      0   1021
   imt
         0      1
  0 548730      4
  1      0   1084
   imt
         0      1
  0 549569      2
  1      0   1010
##############################################################################################################

# a bit worse than above, perhaps add ln?
# produce all 10 full preditions
#pairsf <- readRDS("../MODELS_PHASE4/RDSFiles/full_pairs.RDS");


seq=1:25622405
fullP7 = c()
for (i in 0:9){
 fullP7 = c(fullP7, predict(rfC1[[7]],pairsf$pairs[seq+i*25622405,]));
}
fullP7=fullP7[-256224050];
seq=1:25622405
fullP1 = c()
for (i in 0:9){
 fullP1 = c(fullP1, predict(rfC1[[1]],pairsf$pairs[seq+i*25622405,]));
}
fullP1=fullP1[-256224050];
n=dim(pairsf$data1)[1];
calcMeas(n,pairsf$pairs[fullP1==2,1:2]);
          n         nid    nid+/nid          n+          n1 
1.60070e+04 1.07950e+04 2.71144e-01 8.13900e+03 7.86800e+03 

seq=1:25622405
fullP2 = c()
for (i in 0:9){
 fullP2 = c(fullP2, predict(rfC1[[2]],pairsf$pairs[seq+i*25622405,]));
}
fullP2=fullP2[-256224050];
calcMeas(n,pairsf$pairs[fullP2==2,1:2]);
          n         nid    nid+/nid          n+          n1 
1.60070e+04 1.07900e+04 2.70899e-01 8.14000e+03 7.86700e+03 

fullP3 = c()
for (i in 0:9){
 fullP3 = c(fullP3, predict(rfC1[[3]],pairsf$pairs[seq+i*25622405,]));
}
fullP3=fullP3[-256224050];
calcMeas(n,pairsf$pairs[fullP3==2,1:2]);
           n          nid     nid+/nid           n+           n1 
1.600700e+04 1.076300e+04 2.707424e-01 8.158000e+03 7.849000e+03 

fullP4 = c()
for (i in 0:9){
 fullP4 = c(fullP4, predict(rfC1[[4]],pairsf$pairs[seq+i*25622405,]));
}
fullP4=fullP4[-256224050];
calcMeas(n,pairsf$pairs[fullP4==2,1:2]);
           n          nid     nid+/nid           n+           n1 
1.600700e+04 1.080800e+04 2.699852e-01 8.117000e+03 7.890000e+03 

fullP5 = c()
for (i in 0:9){
 fullP5 = c(fullP5, predict(rfC1[[5]],pairsf$pairs[seq+i*25622405,]));
}
fullP5=fullP5[-256224050];

fullP6 = c()
for (i in 0:9){
 fullP6 = c(fullP6, predict(rfC1[[6]],pairsf$pairs[seq+i*25622405,]));
}
fullP6=fullP6[-256224050];

fullP8 = c()
for (i in 0:9){
 fullP8 = c(fullP8, predict(rfC1[[8]],pairsf$pairs[seq+i*25622405,]));
}
fullP8=fullP8[-256224050];

fullP7ullP9 = c()
for (i in 0:9){
 fullP9 = c(fullP9, predict(rfC1[[9]],pairsf$pairs[seq+i*25622405,]));
}
fullP9=fullP9[-256224050];

fullP10 = c()
for (i in 0:9){
 fullP10 = c(fullP10, predict(rfC1[[10]],pairsf$pairs[seq+i*25622405,]));
}
fullP10=fullP10[-256224050];

save(fullP1, fullP2, fullP3, fullP4, fullP5, fullP6, fullP7, fullP8, fullP9, fullP10, file = "/home/audris/rfmodelsC1FullP.RData");

################################
#do further refinement with Yuxia
################################
load("/home/audris/rfmodelsC1FullP.RData");
fullP = cbind(fullP1,fullP2,fullP3,fullP4,fullP5,fullP6,fullP7,fullP8,fullP9,fullP1)
agg = apply(fullP,1,sum);
table(agg);

       10        11        12        13        14        15        16        17 
256192190       226       217       116        93        71       103       121 
       18        19        20 
      151       186     30575 

sel = agg < 20 & agg > 10
mismatch=data.frame(fr=pairsf$data1$a[pairsf$pairs$id1[sel]],to=pairsf$data1$a[pairsf$pairs$id2[sel]])
mismatch=data.frame(fr=pairsf$data1$a[pairsf$pairs$id1[sel]],to=pairsf$data1$a[pairsf$pairs$id2[sel]],ad=pairsf$pairs$ad[sel],d2vSim=pairsf$pairs$d2vSim[sel],tdz=pairsf$pairs$tdz[sel])
write.table(mismatch,"/home/audris/mismatch.csv",sep=";",quote=F)
################################



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
         0      1
  0 549602      3
  1      7    993

         0      1
  0 548174      6
  1      9   1105

         0      1
  0 549463      2
  1      7   1081

         0      1
  0 551137      3
  1      2   1038

         0      1
  0 550109      3
  1      4   1014

         0      1
  0 549207      3
  1      7   1065

         0      1
  0 549397      0
  1      7   1033

         0      1
  0 547954      5
  1      7   1017

         0      1
  0 548727      2
  1      3   1086

         0      1
  0 549560      0
  1      9   1012

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
         0      1
  0 549601      6
  1      8    990

         0      1
  0 548174      8
  1      9   1103

         0      1
  0 549462      4
  1      8   1079

         0      1
  0 551137      4
  1      2   1037

         0      1
  0 550109      4
  1      4   1013

         0      1
  0 549207      6
  1      7   1062

         0      1
  0 549397      0
  1      7   1033

         0      1
  0 547953      6
  1      8   1016
  
         0      1
  0 548727      5
  1      3   1083
   
         0      1
  0 549560      1
  1      9   1011


#Now disconnect large clusters
                                        #disconnect:
#first create the graph
load(file = "/home/audris/rfmodelsFullP7.RData");
table(fullP7);
        1         2
256193049     31000
#number of clusters
n=dim(pairsf$data1)[1];
gg = make_empty_graph(n, directed = FALSE) # empty graph
gg = set_vertex_attr(g, "id", value = 1:n) 
gg = add_edges(gg, as.vector(t(pairsf$pairs[fullP7==2,1:2]))); 
gg = simplify(gg,remove.multiple = TRUE, remove.loops = TRUE)          
clustg7c <- components(gg, "weak") # get clusters
sort(-table(blocksg7$block))[1:10];
10201  1689  2265  1512   549  3405   650  5575  3890  3084
  -44   -30   -27   -23   -19   -17   -16   -16   -15   -14
length(table(blocksg7$block))
[1] 10835

n=dim(pairsf$data1)[1];
idmatch = paste(pairsf$pairs$id1,pairsf$pairs$id2,sep=";");
# Break root cluster (based on earlier analysis)
fullP7[pairsf$pairs$id1==14783&fullP7==2&pairsf$pairs$id2!=9138&pairsf$pairs$id2!=14783]=1
fullP7[pairsf$pairs$id2==14783&fullP7==2&pairsf$pairs$id1!=9138&pairsf$pairs$id1!=14783]=1
# Break next cluster 3705:
cl3705=pairsf$pairs[match(pairsf$pairs$id1,blocksg7[blocksg7$block==3705,"id"],nomatch=0)>0&fullP7==2,c("id1","id2")]
cl3705=cl3705[cl3705$id1<cl3705$id2,];
all=paste(cl3705$id1,cl3705$id2,sep=";");
all=all[-c(37,39,28, 26,25,23,19,17,13,11,1,2)];
all1=paste(cl3705$id2,cl3705$id1,sep=";");
all1=all1[-c(37,39,28, 26,25,23,19,17,13,11,1,2)];
fullP7[match(idmatch,c(all,all1),nomatch=0)>0]=1;

#create graph
g7 = make_empty_graph(n, directed = FALSE) # empty graph
g7 = set_vertex_attr(g7, "id", value = 1:n) 
g7 = add_edges(g7, as.vector(t(pairsf$pairs[fullP7==2,1:2]))); 
g7 = simplify(g7,remove.multiple = TRUE, remove.loops = TRUE)          
clustg7 <- components(g7, "weak") # get clusters
blocksg7 <- data.frame(id=V(g7)$id, block=clustg7$membership) # block number  
tbg7 = table(blocksg7$block);
big7bl = names(tbg7[tbg7>2]);
bjc = blocksg7[match(blocksg7$block,big7bl,nomatch=0)>0,];
crrf = c();
for (bl in biggbl){
    ids = combn(bjc[bjc$block==bl,"id"],2);
    crrf = rbind(crrf,t(ids));
    crrf = rbind(crrf,t(ids[c(2,1),]));
}
fullP7t = fullP7
#make transitive closure
fullP7t[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0] = 2;
prsc = pairsf$pairs[fullP7t==2,1:2];
fullP7t[match(idmatch,paste(prsc[,2],prsc[,1],sep=";"))>0] = 2;
#write.table(fullP7t, file="/home/audris/fullP7t.csv",row.names=F,col.names=F);
#write.table(blocksg7[,2],file="/home/audris/fullP7tb.csv",row.names=F,col.names=F);

nn=list();
for (bl in names(tbg7[tbg7>9])){
  sl = blocksg7[blocksg7$block==bl,"id"];
  nn[[bl]]=cbind((1:n)[sl],pairsf$data1$a[sl]);
}    



fullP7.c = fullP7;
#now use sadikas input to propagate back to fullP7
reclust = read.csv("RF_CorrectedLabels.csv")
toDisconect = c();
toConnect = c();
for (cl in names(table(reclust$cl))){
  #first disconnect all
  ids = reclust[reclust$cl==cl,"V1"];
  ids = combn(ids,2);
  ids = rbind(t(ids),t(ids[c(2,1),]));
  toDisconect = rbind(toDisconect, ids);
    
  #now reconnect subclusters
  cl1s =  names(table(reclust$l[reclust$cl==cl]));
  for (cl1 in names(table(reclust$l[reclust$cl==cl]))){
    ids = reclust[reclust$cl==cl&reclust$l==cl1,"V1"];
    if (length(ids)>1){
      ids = combn(ids,2);
      ids = rbind(t(ids),t(ids[c(2,1),]));
      toConnect = rbind(toConnect, ids);
    }
  }
}
fullP7.c[match(idmatch,paste(toDisconect[,1],toDisconect[,2],sep=";"),nomatch=0)>0]=1;
fullP7.c[match(idmatch,paste(toConnect[,1],toConnect[,2],sep=";"),nomatch=0)>0]=2;

g7c = make_empty_graph(n, directed = FALSE) # empty graph
g7c = set_vertex_attr(g7c, "id", value = 1:n) 
g7c = add_edges(g7c, as.vector(t(pairsf$pairs[fullP7.c==2,1:2]))); 
g7c = simplify(g7c,remove.multiple = TRUE, remove.loops = TRUE)          
clustg7c <- components(g7c, "weak") # get clusters
blocksg7c <- data.frame(id=V(g7c)$id, block=clustg7c$membership) # block number  
tbg7c = table(blocksg7c$block);
big7cbl = names(tbg7c[tbg7c>2]);
bjc = blocksg7c[match(blocksg7c$block,big7cbl,nomatch=0)>0,];

crrf = c();
for (bl in big7cbl){
    ids = combn(bjc[bjc$block==bl,"id"],2);
    crrf = rbind(crrf,t(ids));
    crrf = rbind(crrf,t(ids[c(2,1),]));
}

#make transitive closure
fullP7.c[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0] = 2;
save(fullP7.c, nn, tbg7c, blocksg7c, file = "/home/audris/rfmodelsFullP7.c.RData");


##################################
# compare to bitergia 
library(readr)
library(igraph)
bitr <- read_delim("../BITERGIA/map2mysqlIds1", delim = ';', col_names = F)
names(bitr) = c("a","id");
bitr$a0=bitr$a;
tba = table(bitr$a)
morRec = data.frame(a="",a0="",id=0);
for (dev in names(tba[tba>1])){
    ids = sort(bitr [ bitr$a == dev, "id"]$id);
    for (i in 2:length(ids)){
        morRec = rbind(morRec, data.frame(a=dev,a0=paste (dev,ids[i],sep=";"), id=ids[i]));
    }
}    
morRec = morRec[-1,]
bitr=rbind(bitr,morRec);
as=read.table("/data/delta/openstack.a.gz",sep=";",quote="",colClasses="character", header=F, strip.white=F)
mb = match(bitr$a, as[,1], nomatch=0);
bitr = bitr[mb>0,]
mb = match(bitr$a, as[,1], nomatch=0);
#nowcreate graphs
tbi = table(bitr$id);
tba = table(bitr$a);
nb=dim(bitr)[1];
bitr$off=1:nb;
gb <- make_empty_graph(nb, directed = FALSE); # empty graph
gb <- set_vertex_attr(gb, "a", value = bitr$a);
gb <- set_vertex_attr(gb, "id", value = bitr$id);
gb <- set_vertex_attr(gb, "i", value = 1:nb);
gn = gb;
for (dev in names(tbi[tbi>1])){
  ids = bitr [ bitr$id == dev, "off"]$off;
  for (i in 2:length(ids)){
      gb <- add_edges(gb, c(ids[1],ids[i])) # candidate edges
  }
}
for (dev in names(tba[tba>1])){
  ids = combn(bitr [ bitr$a == dev, "off"]$off, 2);    
  ids = rbind(t(ids),t(ids[c(2,1),]));
  gn <- add_edges(gn, t(ids));
}

prs=pairsf$pairs[fullP7.c==2,1:2];

prs1 = cbind(pairsf$data1[prs[,1],"a"],pairsf$data1[prs[,2],"a"]);
sel = match(as.vector(unclass(V(gn)$a)),prs1[,1],nomatch=0)>0 & match(as.vector(unclass(V(gn)$a)),prs1[,2],nomatch=0)>0
sel1 = match(prs1[,1],as.vector(unclass(V(gn)$a))[sel],nomatch=0)>0 & match(prs1[,2],as.vector(unclass(V(gn)$a))[sel],nomatch=0)>0
fr=(1:nb)[sel][match(prs1[sel1,1],as.vector(unclass(V(gn)$a))[sel],nomatch=0)];
to=(1:nb)[sel][match(prs1[sel1,2],as.vector(unclass(V(gn)$a))[sel],nomatch=0)];
gn <- add_edges(gn, as.vector(rbind(fr,to)))

clustb <- components(gb, "weak") # get clusters
blocksb <- data.frame(id = V(gb)$i, block=clustb$membership) # block number  
gbc = contract(gb,blocksb[,"block"],vertex.attr.comb=list(i="min"));  

clustn <- components(gn, "weak") # get clusters
blocksn <- data.frame(id = V(gn)$i, block=clustn$membership) # block number  
gnc = contract(gn,blocksn[,"block"],vertex.attr.comb=list(i="min"));  
gnc=simplify(gnc,remove.multiple = TRUE, remove.loops = TRUE)          
gbc=simplify(gbc,remove.multiple = TRUE, remove.loops = TRUE)          
tn = table(blocksn$block)
tb = table(blocksb$block)
badSplit = c();
for (id in names(tn[tn>1])){
    ids = blocksb[match(blocksb[,1],blocksn[blocksn$block==id,1],nomatch=0)>0,2];
    badSplit = c(badSplit,length(table(ids)));
}    
badClump = c();
for (id in names(tn)){
    cl = blocksb[blocksb$id==id,2];
    ids = blocksb[blocksb$block==cl,1];
    badClump = c(badClump,length(table(ids)));
}

table(badSplit>1)
FALSE  TRUE 
  141  1641 


1641/nb
[1] 0.1586427

(sum(badSplit)-length(badSplit))/nb
[1] 0.2488399

table(badClump>1)
FALSE  TRUE 
 4937  1334 

1334/nb
[1] 0.1289637


nb
[1] 10344
length(tn)
[1] 6271


#look at some examples
> V(gn)[19]$a
[1] "Aaron Lee <aaron.lee@rackspace.com>"

pairsf$pairs[pairsf$pairs$id1==49&pairsf$pairs$id2==50,];
pairsf$pairs[pairsf$pairs$id1==49&fullP7.c==2,];
       id1 id2 n         e ln1 fn1        un ifn     fn1.1     ln1.1       nf
768386  49  50 1 0.5548748   1   1 0.3074074   0 0.4333333 0.4777778 -0.60206
       ef lnf      ln1f fnf     fn1f unf ad          tdz d2vSim  gd is_match
768386  0   0 -2.924796   0 -2.79588   0  0 -0.001803372      0 0.5       NA


##################################
# compare to no aliasing traditional
g7c = make_empty_graph(n, directed = FALSE) # empty graph
g7c = set_vertex_attr(g7c, "id", value = 1:n) 
g7c = add_edges(g7c, as.vector(t(pairsf$pairs[fullP7.c==2,1:2]))); 
g7c = simplify(g7c,remove.multiple = TRUE, remove.loops = TRUE)          
clustg7c <- components(g7c, "weak") # get clusters
blocksg7c <- data.frame(id=V(g7c)$id, block=clustg7c$membership) # block number  
tbg7c = table(blocksg7c$block);
big7cbl = names(tbg7c[tbg7c>1]);
bjc = blocksg7c[match(blocksg7c$block,big7cbl,nomatch=0)>0,];
nSplit = c();
for (bl in big7cbl){
    ids = bjc[bjc$block==bl,"id"];
    nSplit = c(nSplit,length(ids));
}    
c(sum(nSplit>0), length(tbg7c), n, sum(nSplit>0)/length(tbg7c),sum(nSplit[nSplit>0])/n)
2956 10950 16007 0.27 0.5

#look at more producrtive group since single commiters can not be aliased
a2nc =fread("/data/delta/Auth2Ncmt",sep=";",quote="",colClasses="character", header=F, strip.white=F);
names(a2nc) = c("n","e","a","nc");
mm = match(pairsf$data1$a, a2nc$a,nomatch=0); 
a2nc=a2nc[mm,]
prs=pairsf$pairs[fullP7.c==2,1:2];

gnc <- make_empty_graph(n, directed = FALSE) # empty graph
gnc <- set_vertex_attr(gnc, "id", value = 1:n) 
gnc <- set_vertex_attr(gnc, "nc", value = as.integer(a2nc$nc));
gnc <- add_edges(gnc, as.vector(t(prs))); # candidate edges


gnc=simplify(gnc,remove.multiple = TRUE, remove.loops = TRUE) ;         
clustnc <- components(gnc, "weak") # get clusters
blocksnc <- data.frame(id = V(gnc)$id, nc=V(gnc)$nc, block = clustnc$membership); # block number  
gncc = contract(gnc,blocksnc[,"block"], vertex.attr.comb=list(id="first",nc=function(x) sum(as.double(x))));
gncc = simplify(gncc,remove.multiple = TRUE, remove.loops = TRUE);
quantile(V(gncc)$nc, c(0,.25, .5, .75, .9, .99, 1))
quantile(V(gncc)$nc, c(0,.25, .5, .75, .9, .99, 1))
       0%       25%       50%       75%       90%       99%      100% 
     1.00      7.00     48.00    317.75   1284.10   7117.40 486617.00 
#errors higher for more productive people
nSplitTop10 = c();
for (id in V(gncc)$id[V(gncc)$nc>1284]){
    ids = bjc[bjc$block==bjc[bjc$id==id,"block"],"id"];
    nSplitTop10 = c(nSplitTop10,length(ids));
}
nSplitTop10[nSplitTop10==0]=1
c(sum(nSplitTop10>1), sum(V(gncc)$nc>1284), sum(nSplitTop10))
[1]  529 1095 2250

 quantile(nSplitTop10,0:10/10)
  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
   1    1    1    1    1    1    2    2    3    4   14 

quantile(c(nSplit, rep(1,sum(tbg7c==1))),0:10/10)
  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
   1    1    1    1    1    1    1    1    2    3   14 

A bit different from the overall situation,
but not much higher fraction with multiple identities.

##################################
# compare to no aliasing (network measures)

a2a=read.table("/data/delta/openstack.a2a.gz",sep=";",quote="",colClasses="character", header=F, strip.white=F,nrow=64301481)
names(a2a)=c("fr","to","n","w");
n=dim(pairsf$data1)[1];
mm = match(a2a$fr,pairsf$data1$a,nomatch=0); 
mm1 = match(a2a$to,pairsf$data1$a,nomatch=0); 

#file induced network
g1 <- make_empty_graph(n, directed = FALSE) # empty graph
g1 <- set_vertex_attr(g1, "id", value = 1:n) 
g1 <- add_edges(g1, as.vector(rbind(mm,mm1))) # candidate edges
g1 = simplify(g1,remove.multiple = TRUE, remove.loops = TRUE);


prs=pairsf$pairs[fullP7.c==2,1:2];
save (prs, fullP7.c, mm, mm1, file="/home/audris/rfmodelsFullP7net.RData")
load ("/home/audris/rfmodelsFullP7net.RData")
g <- make_empty_graph(n, directed = FALSE) # empty graph
g <- set_vertex_attr(g, "id", value = 1:n) 
g <- add_edges(g, as.vector(t(prs))); # candidate edges
g = simplify(g,remove.multiple = TRUE, remove.loops = TRUE)          
clust <- components(g, "weak") # get clusters
blocks <- data.frame(id = V(g)$id, block=clust$membership) # block number  

#now use the identity aliase to collapse nodedes in the file-relationship graph
g1c = contract(g1,blocks[,"block"],vertex.attr.comb=list(id="first"));  
g1c = simplify(g1c,remove.multiple = TRUE, remove.loops = TRUE)

#eigencentrality
g1ecen = eigen_centrality(g1);
g1cecen = eigen_centrality(g1c);

g1cent=centr_degree(g1);
g1ccent=centr_degree(g1c);

#g1tran = c(transitivity(g1),transitivity(g1c));
g1tran = transitivity(g1, type="local")
g1ctran = transitivity(g1c, type="local")

# may take long time
g1con=constraint(g1);
g1ccon=constraint(g1c);

save(g1, g1c, g1cent, g1ccent,g1ecent, g1cecent, g1con, g1ccon,g1tran,g1ctran,  file="/home/audris/rfmodelsCentr.RData")

load(file="/home/audris/rfmodelsCentr.RData")
sel = match(V(g1c)$id,V(g1)$id, nomatch=0);
cor(g1cent$res[sel],g1ccent$res,method="spearman")
[1] 0.8618824
cor(g1con[sel], g1ccon, method="spearman",use="pairwise.complete.obs")
[1] 0.8684937
cor(g1tran[sel], g1ctran, method="spearman",use="pairwise.complete.obs")
[1] 0.8406279

aa0=eigenvector.centrality (g1c,.001)
bb0=eigenvector.centrality (g1,.001)
cor(bb0$vector[sel],aa0$vector,method="spearman")
[1] 0.8690419

aa=rbind(quantile(g1cent$res,0:100/100)/max(g1cent$res),quantile(g1ccent$res,0:1
(aa[2,]/aa[1,])[34]
    33% 
4.79214 

bb= rbind(quantile(g1tran,0:100/100,na.rm=T)/max(g1tran,na.rm=T),quantile(g1ctran,0:100/100,na.rm=T)/max(g1ctran,na.rm=T))
(bb[2,]/bb[1,])[1]
1.240999 

cc=rbind(quantile(g1con,0:100/100,na.rm=T)/max(g1con,na.rm=T),quantile(g1ccon,0:100/100,na.rm=T)/max(g1ccon,na.rm=T));
(cc[2,]/cc[1,])[75]
      74% 
0.4835438 

dd=rbind(quantile(bb0$vector,0:100/100),quantile(aa0$vector,0:100/100,na.rm=T))
(dd[2,]/dd[1,]
(dd[2,]/dd[1,])[25]
     32% 
4.262161 

#look at a subgraph with many commits:
gl = g1;
quantile(as.integer(pairsf$data1$nc),.9)
90% 
    798
sel1 = as.integer(pairsf$data1$nc)>=798
gl = delete.vertices(gl, (1:n)[!sel1]);
glc = g1c;
glc = delete.vertices(glc, (1:length(V(g1c)$id))[match(V(g1c)$id,(1:n)[!sel1],nomatch=0)>0])
gl = simplify(gl,remove.multiple = TRUE, remove.loops = TRUE)
glc = simplify(glc,remove.multiple = TRUE, remove.loops = TRUE)
sel2 = match(V(glc)$id,V(gl)$id, nomatch=0);

cor(centr_degree(gl)$res[sel2], centr_degree(glc)$res,  method="spearman")
[1] 0.8994237


#The homonym resolution is separate procedure, whereby we can
######################################################
# investigate homonym training
pairs <- readRDS("../MODELS_PHASE5/match2345_pairs.RDS");
pairs$pairs$im = as.factor(pairs$pairs$"is_match");
#mm = pairs$pairs$"is_match" == 1;
#nmm=!mm & apply(pairs$pairs[,c("n","e","ln","fn","un","ifn","ln1","fn1")], 1, max) > .8 | pairs$pairs$ad>0|pairs$pairs$tdz>.9|pairs$pairs$d2vSim>0;
#nnmm=!mm & !nmm
vv = 1:dim(pairs$pairs)[1];
eq= c(1817022,2620775,3198135,3823436,4923380,2469101,5117770,429508 ,1674130,2018882,2469100,
      2863731,4667324,5063908,5351827,1674131 ,2566524,3924916,4669669,5028394,5066253,
      1814677,3198134,3305223,4454519,4603843,3647564,3943395,4452175,4667325,1425379,2552916,
      4452174,5026049,5030739,5115306,1814676,2021226,2550570);
fr=c(1117,1771,2183,1632,2265,2265,1221, 666,1900,487,1053,1075,1085,1085,1207,1411,748, 1411, 1221, 1632, 1076, 1148, 748,  774,  775, 774, 1115, 1053, 981,  185, 1093, 1899, 2099, 373, 966, 1094, 184, 2100, 2144, 424, 425, 1450, 1228, 1116,  775, 1117, 1364, 774,  1086, 1900, 1118, 1227, 1227);
to=c(1411, 666, 980,1085, 749, 748,487,1772,1365,1221,2160,1207,1632,1631,1076,1117,2264,1118, 486,  1086, 1207, 528, 2264, 1992, 1992, 775, 1409, 2161, 2183, 373, 1096, 1365, 1225, 184, 1619,1096, 373, 1225, 2255, 229, 230, 1683, 2099, 1410, 1991, 1410, 1899, 1991, 1631, 1364, 1410, 2100, 2099);
for (i in c(1:length(fr))){
  pairs$pairs$im[pairs$pairs$id1==fr[i]&pairs$pairs$id2==to[i]]=1;
  pairs$pairs$im[pairs$pairs$id2==fr[i]&pairs$pairs$id1==to[i]]=1;
}
pp=pairs$pairs[eq,]
pp=pp[pp$im==0,]
for (i in c(1:length(pp$id1))){
  pairs$pairs$im[pairs$pairs$id1==pp$id1[i]&pairs$pairs$id2==pp$id2[i]]=1;
  pairs$pairs$im[pairs$pairs$id2==pp$id1[i]&pairs$pairs$id1==pp$id2[i]]=1;
}
frommtch = cbind(pairs$data1$a[pairs$pairs[pairs$pairs$im==1,"id1"]],pairs$data1$a[pairs$pairs[pairs$pairs$im==1,"id2"]]);
frommtch = frommtch[frommtch[,1]!=frommtch[,2],];
#hopefully corrected

#first fix bad term freqs
cn = read.table("../MODELS_PHASE4/FreqNames200.csv", sep=',', header=T, colClasses="character")
cln = read.table("../MODELS_PHASE4/FreqLastNames200.csv", sep=',', header=T, colClasses="character")
cfn = read.table("../MODELS_PHASE4/FreqFirstNames200.csv", sep=',', header=T, colClasses="character")
cun = read.table("../MODELS_PHASE4/FreqUsernames200.csv", sep=',', header=T, colClasses="character")
ce = read.table("../MODELS_PHASE4/FreqEmail200.csv", sep=',', header=T, colClasses="character")

n=dim(pairs$data1)[1];
idmatch = paste(pairs$pairs$id1,pairs$pairs$id2,sep=";");
pairs$pairs$ef[match(pairs$pairs$id1,(1:n)[match(pairs$data1$e,ce[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$ef[match(pairs$pairs$id2,(1:n)[match(pairs$data1$e,ce[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$nf[match(pairs$pairs$id1,(1:n)[match(pairs$data1$n,cn[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$nf[match(pairs$pairs$id2,(1:n)[match(pairs$data1$n,cn[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$ln1f[match(pairs$pairs$id1,(1:n)[match(pairs$data1$ln1,cln[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$ln1f[match(pairs$pairs$id2,(1:n)[match(pairs$data1$ln1,cln[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$fn1f[match(pairs$pairs$id1,(1:n)[match(pairs$data1$fn1,cfn[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$fn1f[match(pairs$pairs$id2,(1:n)[match(pairs$data1$fn1,cfn[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$unf[match(pairs$pairs$id1,(1:n)[match(pairs$data1$un,cun[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairs$pairs$unf[match(pairs$pairs$id2,(1:n)[match(pairs$data1$un,cun[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
gg = make_empty_graph(n, directed = FALSE) # empty graph
gg = set_vertex_attr(gg, "id", value = 1:n) 
gg = add_edges(gg, as.vector(t(pairs$pairs[pairs$pairs$im==1,1:2]))); 
gg = simplify(gg,remove.multiple = TRUE, remove.loops = TRUE)          
clustgg <- components(gg, "weak") # get clusters
blocksgg <- data.frame(id=V(gg)$id, block=clustgg$membership) # block number  
tbgg = table(blocksgg$block);
biggbl = names(tbgg[tbgg>2]);
bjc = blocksgg[match(blocksgg$block,biggbl,nomatch=0)>0,];
crrf = c();
for (bl in biggbl){
    ids = combn(bjc[bjc$block==bl,"id"],2);
    crrf = rbind(crrf,t(ids));
    crrf = rbind(crrf,t(ids[c(2,1),]));
}
table(pairs$pairs$im[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0])
   0    1 
 130 1648 
pairs$pairs$im[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0] = 1;
#now refit rfC1 to rfC1f
#need to redo since transitive closure added more links
mmC = pairs$pairs$im == 1;
nmmC=!mmC & apply(pairs$pairs[,c("n","e","ln","fn","un","ifn","ln1","fn1")], 1, max) > .8 | pairs$pairs$ad>0|pairs$pairs$tdz>.9|pairs$pairs$d2vSim>0;
nnmmC=!mmC & !nmmC
selC=sample(0:9,sum(mmC),replace=T);
selC1=sample(0:9,sum(nmmC),replace=T);
selC2=sample(0:9,sum(nnmmC),replace=T);
rfC1 = list();
pC1 = list();
for (i in 0:9){
   mm1t = vv[mmC][selC!=i];
   mm1v = vv[mmC][selC==i];
   mm0t = vv[nmmC][selC1!=i];
   #mm0t = c(mm0t, vv[nnmmC][selC2!=i]) #mm0t = c(mm0t, vv[nnmmC&fullP!=pairs$pairs$im])
   mm0v = vv[nmmC][selC1==i];
   mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
   dt=pairs$pairs[c(mm1t,mm0t),c("n","e","ln1","fn1","un","d2vSim", "ad", "tdz", "ifn", "ln1f", "fn1f", "im")];
   dv=pairs$pairs[c(mm1v,mm0v),c("n","e","ln1","fn1","un","d2vSim", "ad", "tdz", "ifn", "ln1f", "fn1f", "im")];
   rfC1[[i+1]] = randomForest(im ~ ., dt, importance=T);
   pC1[[i+1]] = predict(rfC1[[i+1]],dv);
   print (table(pC1[[i+1]],dv$im));
}
         0      1
  0 550490      3
  1      2   1072

         0      1
  0 548863      6
  1      0   1016

         0      1
  0 547974      4
  1      0    954

         0      1
  0 550278      8
  1      1   1070

         0      1
  0 548120      1
  1      0   1109
   
         0      1
  0 549608      1
  1      0   1107
   
         0      1
  0 549424      2
  1      0   1041

         0      1
  0 549028      5
  1      0   1042
   
         0      1
  0 548681      6
  1      0   1067

         0      1
  0 550793      5
  1      0   1163

save(rfC1, pC1, selC, selC1, selC2, file = "/home/audris/rfmodelsC1f.RData");


closeG = function(n, a){
  gg = make_empty_graph(n, directed = FALSE) # empty graph
  gg = set_vertex_attr(gg, "id", value = 1:n) 
  gg = add_edges(gg, as.vector(t(a))); 
  gg = simplify(gg,remove.multiple = TRUE, remove.loops = TRUE)          
  clustgg <- components(gg, "weak") # get clusters
  blocksgg <- data.frame(id=V(gg)$id, block=clustgg$membership) # block number  
  tbgg = table(blocksgg$block);
  biggbl = names(tbgg[tbgg>2]);
  bjc = blocksgg[match(blocksgg$block,biggbl,nomatch=0)>0,];
  crrf = c();
  for (bl in biggbl){
    ids = combn(bjc[bjc$block==bl,"id"],2);
    crrf = rbind(crrf,t(ids));
    crrf = rbind(crrf,t(ids[c(2,1),]));
  }
  crrf;
}

for (i in 0:9){
  mm1v = vv[mmC][selC==i];
  mm0v = vv[nmmC][selC1==i];
  mm0v = c(mm0v, vv[nnmmC][selC2==i])#include the entire validation set for prediction
  dv=pairs$pairs[c(mm1v,mm0v),c("id1","id2","n","e","ln1","fn1","un","d2vSim", "ad", "tdz", "ifn", "ln1f", "fn1f", "im")];
  crrf = closeG (n, dv[pC1[[i+1]]==1,c("id1","id2")]);
  idm = paste(dv[,"id1"],dv[,"id2"],sep=";")
  pC1[[i+1]][match(idm,paste(crrf[,1],crrf[,2],sep=";"),nomatch=0)>0] = 1;               
  print(table(dv$im,pC1[[i+1]]))
}

         0      1
  0 550490      2
  1      3   1072
   
         0      1
  0 548863      0
  1      5   1017
   
         0      1
  0 547974      0
  1      3    955
   
         0      1
  0 550278      1
  1      7   1071
   
         0      1
  0 548120      0
  1      1   1109
   
         0      1
  0 549608      0
  1      1   1107
   
         0      1
  0 549424      0
  1      1   1042
   
         0      1
  0 549028      0
  1      4   1043
   
         0      1
  0 548681      0
  1      5   1068
   
         0      1
  0 550793      0
  1      1   1167



pairsf <- readRDS("../MODELS_PHASE4/FINAL_PAIRS/full_pairs_final.RDS");
n=dim(pairsf$data1)[1];
pairsf$pairs$e[is.na(pairsf$pairs$e)]=0;
pairsf$pairs$un[is.na(pairsf$pairs$un)]=0;
pairsf$pairs$ef[match(pairsf$pairs$id1,(1:n)[match(pairsf$data1$e,ce[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$ef[match(pairsf$pairs$id2,(1:n)[match(pairsf$data1$e,ce[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$nf[match(pairsf$pairs$id1,(1:n)[match(pairsf$data1$n,cn[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$nf[match(pairsf$pairs$id2,(1:n)[match(pairsf$data1$n,cn[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$ln1f[match(pairsf$pairs$id1,(1:n)[match(pairsf$data1$ln1,cln[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$ln1f[match(pairsf$pairs$id2,(1:n)[match(pairsf$data1$ln1,cln[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$fn1f[match(pairsf$pairs$id1,(1:n)[match(pairsf$data1$fn1,cfn[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$fn1f[match(pairsf$pairs$id2,(1:n)[match(pairsf$data1$fn1,cfn[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$unf[match(pairsf$pairs$id1,(1:n)[match(pairsf$data1$un,cun[,2], nomatch=0)>0],nomatch=0)>0] = -10; 
pairsf$pairs$unf[match(pairsf$pairs$id2,(1:n)[match(pairsf$data1$un,cun[,2], nomatch=0)>0],nomatch=0)>0] = -10; 

seq=1:25622405
fullP7 = c()
for (i in 0:9){
 fullP7 = c(fullP7, predict(rfC1[[7]],pairsf$pairs[seq+i*25622405,]));
}
fullP7=fullP7[-256224050];
crrf = closeG (n, pairsf$pairs[fullP7==2,c("id1","id2")]);
idmatch = paste(pairsf$pairs$id1,pairsf$pairs$id2,sep=";");
pairsf$pairs$im=fullP7
pairsf$pairs$im[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"),nomatch=0)>0] = 1;


g7 = make_empty_graph(n, directed = FALSE) # empty graph
g7 = set_vertex_attr(g7, "id", value = 1:n) 
g7 = add_edges(g7, as.vector(t(pairsf$pairs[fullP7==2,1:2]))); 
g7 = simplify(g7,remove.multiple = TRUE, remove.loops = TRUE)          
clustg7 <- components(g7, "weak") # get clusters
blocksg7 <- data.frame(id=V(g7)$id, block=clustg7$membership) # block number  
tbg7 = table(blocksg7$block);



save(fullP7, file = "/home/audris/rfmodelsfullP7f.RData");

big7bl = names(tbg7[tbg7>2]);
bjc = blocksg7[match(blocksg7$block,big7bl,nomatch=0)>0,];
crrf = c();
for (bl in biggbl){
    ids = combn(bjc[bjc$block==bl,"id"],2);
    crrf = rbind(crrf,t(ids));
    crrf = rbind(crrf,t(ids[c(2,1),]));
}
fullP7t = fullP7
#make transitive closure
fullP7t[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0] = 2;
prsc = pairsf$pairs[fullP7t==2,1:2];
fullP7t[match(idmatch,paste(prsc[,2],prsc[,1],sep=";"))>0] = 2;



#select some with 
#get correct fullP7.c, tbg7c, blocksg7c, and pre-final nn
load(file = "/home/audris/rfmodelsFullP7.c.RData");


#Disconnevt these
nn[[6]][grep("vagrant",nn[[6]][,2]),1]
nn[[20]][grep("root",nn[[20]][,2]),1]

#use extra training
correct = c("28","1029","28","1030","27","12530","26","325","26","460","23","24","153","9090","198","7981","562","595","562","596","676","677","1032","1040","1616","1627","1617","1628","14149","7138","664","669","664","670","1615","1617","11293","15297","14409","8357","14409","8358","1029","28","1062","755","1062","758","1063","1066","1064","1065");
prsc = pairsf$pairs[pairsf$pairs$e==1&pairsf$pairs$ef>-2,1:2];
prsc = prsc[prsc$id1<prsc$id2,]
crr = c();
for (i in 1:(length(correct)/2)){
    fr = correct[(i-1)*2+1];
    to = correct[(i-1)*2+2];
    #print( c(fr,to));
    crr = rbind(crr, c(fr,to));
}

mtc = cbind((1:n)[match(frommtch[,1],pairsf$data1$a)], (1:n)[match(frommtch[,2],pairsf$data1$a)])

lookBad=as.integer(names(table(c((1:n)[match(pairsf$data1$e, ce[,2], nomatch=0)>0],
(1:n)[match(pairsf$data1$fn1, cfn[,2], nomatch=0)>0],
(1:n)[match(pairsf$data1$ln1, cln[,2], nomatch=0)>0],
(1:n)[match(pairsf$data1$n, cn[,2], nomatch=0)>0]))));
#get some homonyms
length(lookBad);
frmMtcA= (1:n)[match(pairsf$data1$a,pairs$data1$a, nomatch=0)>0]

look=as.integer(names(table(c(lookBad,crr[,1],crr[,2],prsc$id1[1:500],prsc$id2[1:500],mtc[,1],mtc[,2],nn[[6]][grep("vagrant",nn[[6]][,2]),1],nn[[20]][grep("root",nn[[20]][,2]),1],frmMtcA))))


idmat1 = apply(t(combn(look,2)),1,paste,collapse=";");
id2id = paste(look, look, sep=";");

pairsf1=pairsf$pairs[match(idmatch,c(idmat1,id2id),nomatch=0)>0,];
pairsf1$e[is.na(pairsf1$e)]=0;
pairsf1$un[is.na(pairsf1$un)]=0;
pairsf1$idm=paste(pairsf1$id1,pairsf1$id2,sep=";")
close=apply(pairsf1[,c("n","e","un","ifn","ln1","fn1")], 1, max) > .8 | pairsf1$ad>0|pairsf1$tdz>.9|pairsf1$d2vSim>0


pairsf1$im = rep(0,dim(pairsf1)[1]);
pairsf1$im[match(pairsf1$idm,apply(rbind(crr, as.matrix(prsc[,c("id1","id2")]), mtc),1,paste,collapse=";"),nomatch=0)>0]=1;
#the newly found
pairsf1$im[match(pairsf1$idm,apply(aa,1,paste,collapse=";"),nomatch=0)>0]=1;
pairsf1$im[match(pairsf1$idm,apply(bb,1,paste,collapse=";"),nomatch=0)>0]=1;
pairsf1$im[match(pairsf1$idm,apply(cc,1,paste,collapse=";"),nomatch=0)>0]=1;
pairsf1$im[match(pairsf1$idm,apply(cc1,1,paste,collapse=";"),nomatch=0)>0]=1;
pairsf1$im[match(pairsf1$idm,apply(bbMin,1,paste,collapse=";"),nomatch=0)>0]=0;
pairsf1$im[pairsf1$id1==pairsf1$id2]=1


#fix two errors in manual match
#paste(pairsf$data1$a[pairsf1[pairsf1$im==1&!close,"id1"]],pairsf$data1$a[pairsf1[pairsf1$im==1&!close,"id2"]])
 [1] "Anna <anna.reznikov@nokia.com> AKamyshnikova <akamyshnikova@mirantis.com>"         
 [2] "Anna Reznikov <anna.reznikov@nokia.com> AKamyshnikova <akamyshnikova@mirantis.com>"
 [3] "Athlan-Guyot Sofer <sathlang@redhat.com> sathlan <chem@sathlan.org>"               
 [4] "Chris Krelle <chris.jam.krelle@hpe.com> NoBodyCam <nobodycam@gmail.com>"           
 [5] "Kevin Benton <blak111@gmail.com> Mr. Bojangles <kevin@benton.pub>"                 
 [6] "PavlovAndrey <apavlov@mirantis.com> Andrey Pavlov <andrey-mp@yandex.ru>"           
 [7] "PavlovAndrey <apavlov@mirantis.com> Andrey Pavlov <andrey.mp@gmail.com>"           
 [8] "admin <zhaoxinyu@huawei.com> Jerry Zhao <xyzjerry@gmail.com>"                      
 [9] "e0ne <e0ne@e0ne.info> Ivan Kolodyazhny <ikolodyazhny@mos-docs.vm.mirantis.net>"    
[10] "hayderimran7 <hayderimran7@gmail.com> imran <imran@cloudscaling.com>"              
[11] "hayderimran7 <imran.malik@emc.com> imran <hayderimran7@gmail.com>"                 
[12] "hayderimran7 <imran@cloudscaling.com> imran <hayderimran7@gmail.com>"              
[13] "root <root@unrahul-box.localdomain> Rahul Nair <rahul.unnikrishnan.nair@intel.com>"
[14] "root <root@unrahul-box.localdomain> Rahul Nair <rahulunair@gmail.com>"             
[15] "zemuvier <anesterova@mirantis.com> Alina <zemuvier@gmail.com>"                     
[16] "zemuvier <zemuvier@gmail.com> Alina Nesterova <anesterova@mirantis.com>"           
#pairsf1[pairsf1$im==1&!close,"im"] = 0;

pairsf1$im=as.factor(pairsf1$im);
n1=length(look);
g = make_empty_graph(n1, directed = FALSE) # empty graph
g = set_vertex_attr(g, "id", value = look) 
revl=list();
for (i in 1:n1) revl[[V(g)$id[i]]] = i
edges = pairsf1[pairsf1$im==1,1:2];
for (i in 1:dim(edges)[1]) g = add_edges(g, c(revl[[edges[i,1]]],revl[[edges[i,2]]])); 
g = simplify(g,remove.multiple = TRUE, remove.loops = TRUE)          
clust <- components(g, "weak") # get clusters
blocks <- data.frame(id=V(g)$id, block=clust$membership) # block number  
tb = table(blocks$block);
big = names(tb[tb>2]);
bjc = blocks[match(blocks$block,big,nomatch=0)>0,];
crrf = c();
for (bl in big){
    ids = combn(bjc[bjc$block==bl,"id"],2);
    crrf = rbind(crrf,t(ids));
    crrf = rbind(crrf,t(ids[c(2,1),]));
}
pairsf1$idm = paste(pairsf1$id1,pairsf1$id2,sep=";");
pairsf1$im[match(pairsf1$idm, apply(crrf, 1, paste,collapse=";"))>0] = 1;


close[pairsf1$im == 1] = T;
#inspect, perhaps, jenkins? fit model based on fingerprints alone 
mm = pairsf1$im == 1;
nmm=!mm & close;
nnmm=!mm & !nmm
vv = 1:dim(pairsf1)[1];
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
  vars = c("n","e","ln1","fn1","un","d2vSim", "ad", "tdz", "ifn", "ef","ln1f", "fn1f", "nf","unf", "im");
  vars = c("n","e","ln1","fn1","un","d2vSim", "ad", "ifn", "ef","ln1f", "im");
  dt=pairsf1[c(mm1t,mm0t),vars];
  dv=pairsf1[c(mm1v,mm0v),vars];
  rf[[i+1]] = randomForest(im ~ ., dt, importance=T);
  p[[i+1]] = predict(rf[[i+1]],dv);
  print (table(p[[i+1]],dv$im));
}

         0      1
  0 524564     21
  1      4    530

         0      1
  0 524613     25
  1      2    513

         0      1
  0 524344     24
  1      4    540

         0      1
  0 523640     13
  1      3    575

         0      1
  0 525259     28
  1      1    579

         0      1
  0 525330     18
  1      1    578

         0      1
  0 523879     21
  1      2    534

         0      1
  0 524203     21
  1      2    551

         0      1
  0 524579     12
  1      2    526

         0      1
  0 524290     17
  1      2    570

0      1
  0 523809     22
  1      5    552

         0      1
  0 525530     13
  1      2    509

         0      1
  0 524299     15
  1      2    538

         0      1
  0 524290     21
  1      7    534

         0      1
  0 523995     25
  1      3    537

         0      1
  0 524020     25
  1      6    560

         0      1
  0 523812     22
  1      9    568

         0      1
  0 524988     17
  1      4    535
   
         0      1
  0 525881     12
  1      2    572
   
         0      1
  0 524116     16
  1      3    544


         0      1
  0 524958     14
  1      5    546
   
         0      1
  0 524178     20
  1     14    555
   
         0      1
  0 525153     25
  1     10    569
   
         0      1
  0 524027     11
  1      2    517
   
         0      1
  0 523607     18
  1      5    536
   
         0      1
  0 525025     17
  1     10    525
   
         0      1
  0 524270     18
  1      9    526
   
         0      1
  0 524524     13
  1      5    502
   
         0      1
  0 524578     15
  1     13    547
   
         0      1
  0 524524     13
  1      4    512
> 


fps= c();
fps1= c();
for (i in 0:9){
mm0v = vv[nmm][sel1==i];  mm1v = vv[mm][sel==i];
mm0v = c(mm0v, vv[nnmm][sel2==i]);
#all of these are good for i=0, so the alg sucks, si for larger i
idm10=pairsf1[c(mm1v,mm0v),][p[[i+1]]==0&pairsf1[c(mm1v,mm0v),"im"]==1,c("id1","id2")];
#paste(pairsf$data1$a[idm10[,1]],pairsf$data1$a[idm10[,2]])
#check these two
idm01=pairsf1[c(mm1v,mm0v),][p[[i+1]]==1&pairsf1[c(mm1v,mm0v),"im"]==0,c("id1","id2")];
fps= rbind(fps,idm01)
fps1= rbind(fps1,idm10)
}
#paste(pairsf$data1$a[idm01[,1]],pairsf$data1$a[idm01[,2]]);
#both correct
paste(pairsf$data1$a[fps[,1]],pairsf$data1$a[fps[,2]]);
paste(pairsf$data1$a[fps1[,1]],pairsf$data1$a[fps1[,2]]);
paste(pairsf$data1$a[fps1[,1]],pairsf$data1$a[fps1[,2]])[fps1[,2]>fps1[,1]]

paste(pairsf$data1$a[fps[,1]],pairsf$data1$a[fps[,2]])[fps[,2]!=fps[,1]]

cc2 = rbind(
    cbind(fps[,1],fps[,2])[pairsf$data1$e[fps[,2]]!=pairsf$data1$e[fps[,1]],][-c(2,3,4,6,7,8,9,10,11,13,15),],
    cbind(fps[,1],fps[,2])[pairsf$data1$e[fps[,2]]==pairsf$data1$e[fps[,1]],][-8,]
    );

cc1 = rbind(
    cbind(fps[,1],fps[,2])[pairsf$data1$e[fps[,2]]!=pairsf$data1$e[fps[,1]],][-c(2,3,4,6,7,8,9,10,11,13,15),],
    cbind(fps[,1],fps[,2])[pairsf$data1$e[fps[,2]]==pairsf$data1$e[fps[,1]],][-21,]
    );


cc = cbind(fps[,1],fps[,2])[pairsf$data1$e[fps[,1]]==pairsf$data1$e[fps[,1]],]

bbMin = rbind(
cbind(fps1[,1],fps1[,2])[fps1[,2]<fps1[,1],][c(75,73,69,70,68,60,56,48,47,45,39,31,19,2),],
cbind(fps1[,1],fps1[,2])[fps1[,2]>fps1[,1],][c(90,89,88,83,82,75,72,67,66,62,58,56,55,43),]
)

bb = cbind(fps[,1],fps[,2],paste(pairsf$data1$a[fps[,1]],pairsf$data1$a[fps[,2]]))
bb=bb[-c(6,27,44,49,50),]

aa = cbind(fps[,1],fps[,2],paste(pairsf$data1$a[fps[,1]],pairsf$data1$a[fps[,2]]))
aa=aa[-grep("ewptone",aa[,3]),c(1,2)];


######################################################
# Investigate lsh

library(textreuse);
library(tokenizers);
library(igraph)
b <- 90
m <- 360
# create the minhash function
minhash <- minhash_generator(n = m, seed = 02082018)
docs <- pairsf$data1$a;
names(docs) <- 1:n;
corpus <- TextReuseCorpus(text = docs, # dataset
                          tokenizer = tokenize_character_shingles, n=5, simplify=TRUE, # n = 3, simplify = TRUE, # shingles
                          progress = TRUE, # quietly
                          keep_tokens = TRUE, # store shingles
                          minhash_func = minhash, skip_short = F) # use minhash

buckets <- lsh(corpus, bands = b, progress = TRUE)
#as =read.table("/home/audris/auth/authors.00.1",sep=";",quote="",colClasses="character", header=F, strip.white=F,comment.char="")
#names(as)=c("un","fn","ln","n","e","a")
pairsf <- readRDS("../MODELS_PHASE4/RDSFiles/full_pairs.RDS");
as=data.frame(as=pairsf$data1[,"a"])
rm(pairsf);
n=dim(as)[1];
as$id = 1:n;
docs <- apply(as,1,function(x) paste(x[1],collapse=""));
names(docs) <- as$id;

b <- 360
m <- 720
# create the minhash function
minhash <- minhash_generator(n = m, seed = 02082028)
corpus <- TextReuseCorpus(text = docs, # dataset
                          tokenizer = tokenize_character_shingles, n=4, simplify=TRUE, # n = 3, simplify = TRUE, # shingles
                          progress = TRUE, # quietly
                          keep_tokens = TRUE, # store shingles
                          minhash_func = minhash, skip_short = F) # use minhash
#lsh_threshold(h = m, b = 200)
#lsh_probability(h = m, b = 90, s=.5)

buckets <- lsh(corpus, bands =  b, progress = TRUE)
candidates <- lsh_candidates(buckets)

#evaluate how well these setse intersect
Compare with fullP7.c

4 360 720
quantile(table(candidates[,1]),0:10/100)
 0%  1%  2%  3%  4%  5%  6%  7%  8%  9% 10% 
  1   3   6   8  11  13  16  19  21  24  27 
   0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
   1.0   27.0   73.0  169.8  402.0  865.0 1785.6 3067.0 4482.8 6232.0 8985.0 

save(candidates,buckets, file="/home/audris/lshCandidates.4.360.720.02082028.RData")


0%  1%  2%  3%  4%  5%  6%  7%  8%  9% 10% 
    1   4   7   9  12  15  19  22  24  28  31

  0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
 1.0   31.0   73.0  139.0  256.0  509.5 1155.0 2239.5 3291.0 4721.0 8406.0 
save(candidates, file="/home/audris/lshCandidates.4.360.720.02082018.RData")

4 180 720
quantile(table(candidates[,1]),0:10/10)
  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
   1    1    2    3    6   12   23   51  126  363 2060 


3 360 1080
    0%   0.1%   0.2%   0.3%   0.4%   0.5%   0.6%   0.7%   0.8%   0.9%     1% 
 1.000  4.000  8.000 11.000 13.000 15.000 17.000 19.000 21.040 23.045 26.000 
quantile(table(candidates[,1]),0:10/10)
  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
   1  168  338  542  851 1346 2190 3272 4458 6173 9729 

3 360 720
quantile(table(candidates[,1]),0:10/10)
  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
   1  168  338  542  851 1346 2190 3272 4458 6173 9729 
qantile(table(candidates[,1]),0:10/1000)
    0%   0.1%   0.2%   0.3%   0.4%   0.5%   0.6%   0.7%   0.8%   0.9%     1% 
 1.000  4.000  8.000 11.000 13.000 15.000 17.000 19.000 21.040 23.045 26.000 
save(candidates, file="/home/audris/lshCandidates.3.360.720.RData")


3 90 720 
0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
   1    1    1    1    1    1    1    2    2    4   42 


save(candidates, file="/home/audris/lshCandidates.3.180.720.RData")
180 720 3
    0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
   1.0    2.0    4.0    7.0   12.0   20.0   34.0   60.0  117.0  291.5 1933.0 

90 360 5
quantile(table(candidates[,1]),0:10/10)
  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
   1    1    1    2    2    4    6   13   27   71  466 

90 360 3
quantile(table(candidates[,1]),0:10/10)
  0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
   1    1    3    5    9   15   26   47   98  263 1918 

sort(-table(candidates[,1]))[1:20]
#lsh_jaccard <- lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)
g <- make_empty_graph(n, directed = FALSE) # empty graph
g <- add_edges(g, as.vector(t(candidates[, 1:2]))) # candidate edges
g <- set_vertex_attr(g, "id", value = candidates[, 1]) # add id
                                        # get custers, these are the blocks
clust <- components(g, "weak") # get clusters
blocks <- data.frame(id = V(g)$id, # record id
                     block = clust$membership) # block number  
sort(table(blocks$block))
head(blocks)



#look at the distribution
g1cent=centr_degree(g1);
g1ccent=centr_degree(g1c);
aa=rbind(quantile(g1cent$res,0:100/100)/max(g1cent$res),quantile(g1ccent$res,0:100/100)/max(g1ccent$res));
(aa[2,]/aa[1,])[34]
     33% 
4.793007 

g1ecen = eigen_centrality(g1);
g1cecen = eigen_centrality(g1c);
cg1=constraint(g1);
cg1c=constraint(g1c);
c(transitivity(g1),transitivity(g1c));
[1] 0.7925802 0.8214282

cc=rbind(quantile(cg1,0:100/100,na.rm=T)/max(cg1,na.rm=T),quantile(cg1c,0:100/100,na.rm=T)/max(cg1c,na.rm=T));
(cc[2,]/cc[1,])[75]
      74% 
    0.4798219


#look at the top 1% of the nodes



#################################################################
# Comapare with bagging
cut -d, -f16 ../MODELS_PHASE4/FullOSPrediction.csv | grep -v p1 | sed 's/"//g;s/L/1/;s/N/0/' | gzip > /home/audris/p1.gz
p1=read.table("/home/audris/p1.gz",colClasses="integer",nrows=256224049)
p1=p1$V1;
                                        #Do transitive closure
n=dim(pairsf$data1)[1];
idmatch = paste(pairsf$pairs$id1,pairsf$pairs$id2,sep=";");

gg = make_empty_graph(n, directed = FALSE) # empty graph
gg = set_vertex_attr(gg, "id", value = 1:n) 
gg = add_edges(gg, as.vector(t(pairsf$pairs[p1==1,1:2]))); 
gg = simplify(gg,remove.multiple = TRUE, remove.loops = TRUE)          
clustgg <- components(gg, "weak") # get clusters
blocksgg <- data.frame(id=V(gg)$id, block=clustgg$membership) # block number  
tbgg = table(blocksgg$block);
biggbl = names(tbgg[tbgg>2]);
bjc = blocksgg[match(blocksgg$block,biggbl,nomatch=0)>0,];
crrf = c();
for (bl in biggbl){
    ids = combn(bjc[bjc$block==bl,"id"],2);
    crrf = rbind(crrf,t(ids));
    crrf = rbind(crrf,t(ids[c(2,1),]));
}
table(p1[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0])
p1[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0] = 1;
prsc = pairsf$pairs[p1==1,1:2];
p1[match(idmatch,paste(prsc[,2],prsc[,1],sep=";"))>0] = 1;
write.table(p1, file="/home/audris/p1t.csv",row.names=F,col.names=F);
write.table(blocksgg[,2],file="/home/audris/p1tb.csv",row.names=F,col.names=F);


#######################
# The full model can be corrected as well

correct = c("28","1029","28","1030","27","12530","26","325","26","460","23","24","153","9090","198","7981","562","595","562","596","676","677","1032","1040","1616","1627","1617","1628","14149","7138","664","669","664","670","1615","1617","11293","15297","14409","8357","14409","8358","1029","28","1062","755","1062","758","1063","1066","1064","1065");
prsc=pairsf$pairs[p1+1!=fullP7&pairsf$pairs$e==1,1:2];
b=cbind(prsc$id1,prsc$id2,pairsf$data1[prsc$id1,"a"],pairsf$data1[prsc$id2,"a"])
b=b[b[,1]<b[,2],]
vv = as.vector(t(b[-grep("devnull",b[,3]),1:2]))
correct=c(as.integer(correct), as.integer(vv[!is.na(vv)]));
fullP7.c = fullP7;
crr = c();
for (i in 1:(length(correct)/2)){
    fr = correct[(i-1)*2+1];
    to = correct[(i-1)*2+2];
    #print( c(fr,to));
    crr = rbind(crr, c(fr,to));
}
idmatch = paste(pairsf$pairs$id1,pairsf$pairs$id2,sep=";");
fullP7.c[match(idmatch,paste(crr[,1],crr[,2],sep=";"))>0]=2
ga = make_empty_graph(n, directed = FALSE) # empty graph
ga = set_vertex_attr(ga, "id", value = 1:n) 
ga = add_edges(ga, as.vector(t(pairsf$pairs[fullP7.c==2,1:2]))); 
ga = simplify(ga,remove.multiple = TRUE, remove.loops = TRUE)          
#gacl = cliques(ga,min=3);

clustga <- components(ga, "weak") # get clusters
blocksga <- data.frame(id=V(ga)$id, block=clustga$membership) # block number  
tbga = table(blocksga$block);
bigbl = names(tbga[tbga>2]);
bjc = blocksga[match(blocksga$block,bigbl,nomatch=0)>0,];
crrf = c();
for (bl in bigbl){
    ids = combn(bjc[bjc$block==bl,"id"],2);
    crrf = rbind(crrf,t(ids));
    crrf = rbind(crrf,t(ids[c(2,1),]));
}
table(fullP7.c[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0])
fullP7.c[match(idmatch,paste(crrf[,1],crrf[,2],sep=";"))>0] = 2;


prsc=pairsf$pairs[fullP7.c==2,1:2];
fullP7.c[match(idmatch,paste(prsc[,2],prsc[,1],sep=";"))>0] = 2;

prsc1=pairsf$pairs[p1+1<fullP7.c,];
prsc1 = prsc1[prsc1$id1<prsc1$id2,];
prsc1$a2a=paste(pairsf$data1[prsc1$id1,"a"],pairsf$data1[prsc1$id2,"a"],sep=";")
write.table(prsc1,file="/home/audris/validate1.csv",sep="|")

prsc2=pairsf$pairs[p1+1>fullP7.c,];
prsc2 = prsc2[prsc2$id1<prsc2$id2,];
prsc2$a2a=paste(pairsf$data1[prsc2$id1,"a"],pairsf$data1[prsc2$id2,"a"],sep=";")
write.table(prsc2,file="/home/audris/validate2.csv",sep="|")
#################################################################



n=dim(pairsf$data1)[1];
calcMeas(n,pairsf$pairs[fullP2==2,1:2]);

ss=is.na(pairsf$pairs$un)
pairsf$pairs$un[ss] = 0
ss=is.na(pairsf$pairs$e)
pairsf$pairs$e[ss] = 0
fullP7[is.na(fullP7)]=predict(rfC1[[7]],pairsf$pairs[is.na(fullP7),]);

n=dim(pairsf$data1)[1];
calcMeas(n,pairsf$pairs[fullP7==2,1:2]);
           n          nid     nid+/nid           n+           n1 
1.600700e+04 1.081500e+04 2.696255e-01 8.108000e+03 7.899000e+03 
#look at the productivity, connectivity



# Clumping/splitting

    

n=dim(pairs$data1)[1];
prs=pairs$pairs[pairs$pairs$im==1,1:2];
calcMeas(n,prs)
[1] 2297.0000000   48.0000000    0.9545024

  
prs=pairs$pairs[][pairs$pairs$im==1,1:2];

  
calcMeas = function (n,prs){
 g <- make_empty_graph(n, directed = FALSE) # empty graph
 g <- set_vertex_attr(g, "id", value = 1:n) 
 g <- add_edges(g, as.vector(t(prs))) # candidate edges
 clust <- components(g, "weak") # get clusters
 blocks <- data.frame(id = V(g)$id, # record id
                     block = clust$membership) # block number  
 aa = sort(table(blocks$block))
 res = c(n,length(aa),length(aa[aa>1])/length(aa),sum(aa[aa>1]), sum(aa[aa==1]));
 names(res)=c("n","nid","nid+/nid","n+","n1");
 res;
}

#Compare against the golden
library(textreuse);
library(tokenizers);
library(igraph)

as =read.table("/home/audris/auth/authors.00.1",sep=";",quote="",colClasses="character", header=F, strip.white=F,comment.char="")
names(as)=c("un","fn","ln","n","e","a")
b <- 90
m <- 360
# create the minhash function
minhash <- minhash_generator(n = m, seed = 02082018)
n=dim(as)[1];
as$id = 1:n;
docs <- apply(as,1,function(x) paste(x[6],collapse=""));
names(docs) <- as$id;
corpus <- TextReuseCorpus(text = docs, # dataset
                          tokenizer = tokenize_character_shingles, n=5, simplify=TRUE, # n = 3, simplify = TRUE, # shingles
                          progress = TRUE, # quietly
                          keep_tokens = TRUE, # store shingles
                          minhash_func = minhash, skip_short = F) # use minhash
lsh_threshold(h = m, b = 90)
lsh_probability(h = m, b = 90, s=.5)

buckets <- lsh(corpus, bands = b, progress = TRUE)
candidates <- lsh_candidates(buckets)
#lsh_jaccard <- lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)
g <- make_empty_graph(n, directed = FALSE) # empty graph
g <- add_edges(g, as.vector(t(candidates[, 1:2]))) # candidate edges
g <- set_vertex_attr(g, "id", value = dat$id) # add id
                                        # get custers, these are the blocks
clust <- components(g, "weak") # get clusters
blocks <- data.frame(id = V(g)$id, # record id
                     block = clust$membership) # block number  
sort(table(blocks$block))



head(blocks)
#compare with bagging


pairsf <- readRDS("../MODELS_PHASE4/full_pairs.RDS");
bm0 <- readRDS("../MODELS_PHASE4/BaggingModels/Model_bag10_match2345.RDS");

pBM0 = predict(bm0$model,pairs$pairs);
table(pBM0, pairs$pairs$im)
pBM0       0       1
   L       8    5489
   N 5493432      96

table(pBM0, pairs$pairs$"is_match")
pBM0       0       1
   L      10    5487
   N 5493500      28



pairs$data1[pairs$pairs$id1[(pBM0=="L") != pairs$pairs$im,"a"]]



  

#evaluate errors
library(data.table);
library(igraph);
pairsf <- readRDS("../MODELS_PHASE4/full_pairs.RDS");
load(file = "/home/audris/rfmodelsC1FullP.RData");
a2nc =fread("/data/delta/Auth2Ncmt",sep=";",quote="",colClasses="character", header=F, strip.white=F);
names(a2nc) = c("n","e","a","nc");
mm = match(pairsf$data1$a, a2nc$a,nomatch=0); 
a2nc=a2nc[mm,]
prs=pairsf$pairs[fullP7==2,1:2];
n=dim(pairsf$data1)[1];
g <- make_empty_graph(n, directed = FALSE) # empty graph
g <- set_vertex_attr(g, "id", value = 1:n) 
g <- set_vertex_attr(g, "nc", value = as.integer(a2nc$nc));
g <- add_edges(g, as.vector(t(prs))); # candidate edges


g=simplify(g,remove.multiple = TRUE, remove.loops = TRUE) ;         
save(a2nc,g,file="/home/audris/rfmodelsA2NC.RData");
library(data.table);
library(igraph);
load("/home/audris/rfmodelsA2NC.RData");
clust <- components(g, "weak") # get clusters
blocks <- data.frame(id = V(g)$id, nc=V(g)$nc, block = clust$membership); # block number  
g <- set_vertex_attr(g, "nc", value = as.double(a2nc$nc));
gc = contract(g,blocks[,"block"], vertex.attr.comb=list(id="first",nc=function(x) sum(as.double(x))));
gc = simplify(gc,remove.multiple = TRUE, remove.loops = TRUE);
#now compare distribution of nc for g and gc
data.frame(id = V(gc)$id, nc=V(gc)$nc)[1:5,];

bsize=table(blocks$block);
gc <- set_vertex_attr(gc, "bsize", value = bsize[blocks$block[V(gc)$id]]);

res=data.frame(id = V(gc)$id, nc=V(gc)$nc,bs=V(gc)$bsize)
tapply(res$nc,res$bs,mean)
 tapply(res$nc,res$bs,mean)
         1          2          3          4          5          6          7 
  420.7449   635.6432  1173.2512   980.7562  5593.5565  1436.2857  1249.8667 
         8          9         10         11         12         13         14 
 2807.4615  3562.9091  2086.6667  6051.0000  1089.0000 90782.0000  2067.0000 
        15         16         17         19         23         27         30 
  489.0000 14170.5000   338.0000   168.0000   931.0000 21742.0000  8290.0000 
        35         51 
  728.0000 54597.0000 
> tapply(res$nc,res$bs,length)
   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
7899 1822  613  242  115   49   30   13   11    3    3    3    1    1    1    2 
  17   19   23   27   30   35   51 
   1    1    1    1    1    1    1 
> tapply(res$nc,res$bs,median)
      1       2       3       4       5       6       7       8       9      10 
   26.0   121.5   269.0   421.0   534.0   496.0   659.5  1261.0  1635.0   742.0 
     11      12      13      14      15      16      17      19      23      27 
 5301.0   930.0 90782.0  2067.0   489.0 14170.5   338.0   168.0   931.0 21742.0 
     30      35      51 
 8290.0   728.0 54597.0 




a2a=read.table("/data/delta/openstack.a2a.gz",sep=";",quote="",colClasses="character", header=F, strip.white=F,nrow=64301481)
names(a2a)=c("fr","to","n","w");
a2a2=a2a[as.integer(a2a$n)>2,];
a2a4=a2a2[as.integer(a2a2$n)>4,];

n=dim(pairsf$data1)[1];
mm = match(a2a$fr,pairsf$data1$a,nomatch=0); 
mm1 = match(a2a$to,pairsf$data1$a,nomatch=0); 
mm2 = match(a2a2$fr,pairsf$data1$a,nomatch=0); 
mm21 = match(a2a2$to,pairsf$data1$a,nomatch=0); 
mm4 = match(a2a4$fr,pairsf$data1$a,nomatch=0); 
mm41 = match(a2a4$to,pairsf$data1$a,nomatch=0); 
prs=pairsf$pairs[fullP7==2,1:2]



g1 <- make_empty_graph(n, directed = FALSE) # empty graph
g1 <- set_vertex_attr(g1, "id", value = 1:n) 
g1 <- add_edges(g1, as.vector(rbind(mm,mm1))) # candidate edges

g2 <- make_empty_graph(n, directed = FALSE) # empty graph
g2 <- set_vertex_attr(g2, "id", value = 1:n) 
g2 <- add_edges(g2, as.vector(rbind(mm2,mm21))) # candidate edges

g4 <- make_empty_graph(n, directed = FALSE) # empty graph
g4 <- set_vertex_attr(g4, "id", value = 1:n) 
g4 <- add_edges(g4, as.vector(rbind(mm4,mm41))) # candidate edges

g1=simplify(g1,remove.multiple = TRUE, remove.loops = TRUE)          
g2=simplify(g2,remove.multiple = TRUE, remove.loops = TRUE)          
g4=simplify(g4,remove.multiple = TRUE, remove.loops = TRUE)          


g <- make_empty_graph(n, directed = FALSE) # empty graph
g <- set_vertex_attr(g, "id", value = 1:n) 
g <- add_edges(g, as.vector(t(prs))); # candidate edges
g=simplify(g,remove.multiple = TRUE, remove.loops = TRUE)          
clust <- components(g, "weak") # get clusters
blocks <- data.frame(id = V(g)$id, block= clust$membership) # block number  

g1c = contract(g1,blocks[,"block"],vertex.attr.comb=list(id="first"));  
g1c = simplify(g1c,remove.multiple = TRUE, remove.loops = TRUE)

g1cent=centr_degree(g1);
g1ccent=centr_degree(g1c);
g1con=constraint(g1);
save(g, g1, g1c, g1cent, g1ccent, g1con, file="/home/audris/rfmodelsCentr.RData")
g1ccon=constraint(g1c);
save(g, g1, g1c, g1cent, g1ccent, g1con, g1ccon, file="/home/audris/rfmodelsCentr.RData")
g1tran = c(transitivity(g1),transitivity(g1c));
save(g1tran, file="/home/audris/rfmodelsCentrTrans.RData")
g1ecent=eigen_centrality(g1);
g1cecent=eigen_centrality(g1c);
save(g1tran, g1ecent, g1cecent, file="/home/audris/rfmodelsTrans.RData")


aa=rbind(quantile(g1cent$res,0:100/100)/max(g1cent$res),quantile(g1ccent$res,0:100/100)/max(g1ccent$res));
cc=rbind(quantile(g1con,0:100/100,na.rm=T)/max(g1con,na.rm=T),quantile(g1ccon,0:100/100,na.rm=T)/max(g1ccon,na.rm=T));
ec=rbind(quantile(g1ecent$vector,0:100/100,na.rm=T)/max(g1ecent$vector,na.rm=T),quantile(g1cecent$vector,0:100/100,na.rm=T)/max(g1cecent$vector,na.rm=T));
g1tran
[1] 0.7925802 0.8214282
(cc[1,]/cc[2,])[75]
     74% 
2.084107 
(aa[2,]/aa[1,])[33]
     32% 
4.759115 
(ec[2,]/ec[1,])[33]
     32% 
4.354957 

#Unrelated stuff


b[14]="09 19 57"
for m in ${!b[@]}; do for l in $(echo ${b[$m]}); do gunzip -c $base.$m.olist.$l.gz; done; done | wc -l &
for m in ${!b[@]}; do for l in $(echo ${b[$m]}); do rm $base.$m.$l.*; done; done  &
for m in ${!b[@]}; do for l in $(echo ${b[$m]}); do gunzip -c $base.$m.olist.$l.gz; done; done | split -l 300586 -d -a 4 --filter='gzip > $FILE.gz' - $base.74.olist. &
ls -f $base.74.olist.*.gz | cut -d\. -f4 | while read i; do ii=$(echo $i|sed 's/^0*//;s/^$/0/'); j=$(($ii % 64)); k=$(($ii/64+74)); mv $base.74.olist.$i.gz $base.$k.olist.$j.gz; done    


http://localhost:3000/2015/UT26-15D/Daily Photos/UT26-15D_02_14_2016 (2).JPG;"['x: 0.5234375', 'y: 0.0823529411765', 'width: 0.1953125', 'height: 0.164705882353']";scavenging;0.81577694
db.tags.insert( {
        "user" : ObjectId("5984676ae94fb4bc9c04bc64"),
        "location" : "[{\"type\":\"rect\",\"geometry\":{\"x\":0.5234375,\"y\":0.0823529411765,\"width\":0.1953125,\"height\":0.164705882353},\"style\":{}}]",
        "image" : "http://localhost:3000/2015/UT26-15D/Daily Photos/UT26-15D_02_14_2016 (2).JPG",
        "tag" : "VALIDATE scavenging 0.81577694",
        "created" : ISODate("2018-04-15T10:31:00.000Z"),
        "__v" : 0,
})


cat /home/audris/mapped_scav.csv | perl -ane 'chop();@x=split(/;/);$cc=$x[1]; $cc=~s/'"'"'//g;$cc=~s/\[x/location":"[{\\"type\\":\\"rect\\",\\"geometry\\":{\\"x\\"/;$cc=~s/ y:/ \\"y\\":/;$cc=~s/(width|height)/\\"$1\\"/g;$cc=~s/\]/},\\"style\\":{}}]/;print "db.tags.insert({ \"user\" : ObjectId(\"5984676ae94fb4bc9c04bc64\"), $cc, \"image\" : \"$x[0]\", \"tag\" : \"validate $x[2] $x[3]\",\"created\" : ISODate(\"2018-04-15T10:31:00.000Z\"),\"__v\" : 0,})\n";' > /home/audris/mapped_scav.ins
cat /home/audris/mapped_purge.csv | perl -ane 'chop();@x=split(/;/);$cc=$x[1]; $cc=~s/'"'"'//g;$cc=~s/\[x/location":"[{\\"type\\":\\"rect\\",\\"geometry\\":{\\"x\\"/;$cc=~s/ y:/ \\"y\\":/;$cc=~s/(width|height)/\\"$1\\"/g;$cc=~s/\]/},\\"style\\":{}}]/;print "db.tags.insert({ \"user\" : ObjectId(\"5984676ae94fb4bc9c04bc64\"), $cc, \"image\" : \"$x[0]\", \"tag\" : \"validate $x[2] $x[3]\",\"created\" : ISODate(\"2018-04-15T10:31:00.000Z\"),\"__v\" : 0,})\n";' > /home/audris/mapped_purge.ins
cat /home/audris/mapped_mold.csv | perl -ane 'chop();@x=split(/;/);$cc=$x[1]; $cc=~s/'"'"'//g;$cc=~s/\[x/location":"[{\\"type\\":\\"rect\\",\\"geometry\\":{\\"x\\"/;$cc=~s/ y:/ \\"y\\":/;$cc=~s/(width|height)/\\"$1\\"/g;$cc=~s/\]/},\\"style\\":{}}]/;print "db.tags.insert({ \"user\" : ObjectId(\"5984676ae94fb4bc9c04bc64\"), $cc, \"image\" : \"$x[0]\", \"tag\" : \"validate $x[2] $x[3]\",\"created\" : ISODate(\"2018-04-15T10:31:00.000Z\"),\"__v\" : 0,})\n" if $x[3]>.9;' > /home/audris/mapped_mold.ins



##############################################
Do Chris stuff

```
ssh titan
cd     csc229/scratch/audris1/id
```

the files may be removed
```
rm -rf .git
git clone gh:ssc-oscar/titan .
cc -O3 -o jwt jarowinklerm.c 
scp -p da4:/data/update/cprojects.as1 .
```

the locally installed modules may be removed
```
module load r/3.3.2x
R --no-save
install.packages('RecordLinkage')
```

edit r.pbs as neede: nodes/tasks (tasks=nodes*16)
rm auth1/*

prepare cprojects.as1 on da4
```
gunzip -c ../cprojects.as | grep -v '^$'| perl -I /da3_data/lookup/ -ane 'use cmt;chop();$a=$_;@x=git_signature_parse($a); $fn=$x[0];$ln=$x[0];$fn=~s/\s.*//;$ln=~s/\s*$//;$ln=~s/.*\s//;$u=$x[1];$u=~s/\@.*//;print "$u;$x[0];$fn;$ln;$x[1];$a\n"' | gzip > ../cprojects.as1
```

Prepare data for r to read tasks = records/400
```
rm auth1/*
zcat cprojects.as1 > cprojects.as
split -n l/4096 cprojects.as auth1/a.
qsub r.pbs 
```

#############
Chris

zcat as.11 > as.11.txt

#Chris stuff
n=4096
0-1
2-20
21-320
321-620
621-920
921-1220
1221-1520
1521-1820
1821-2048


#####
Try shorter lists
head -$(($n*438)) as.11.txt > as.11.txt.$(($n*438))


#############
Full
echo $((16140709/320/16))
3152.5

fr=0
to=1
p=3152
n=$(($p*16))
rr=$(($p/2))
mkdir  auth$n
rm auth$n/a.*
split -n r/$rr as.11.txt auth$n/a.
mkdir ${n}_${fr}-$to/
rm ${n}_${fr}-$to/*
sed "s/__OUT__/${n}_${fr}-$to/;s/__FROM__/$fr/;s/__TO__/$to/;s/auth1/auth$n/;s/__READERS__/$rr/" tst.r > tst.${n}_${fr}-$to.r
sed "s/tst.r/tst.${n}_${fr}-$to.r/;s/walltime=02:00/walltime=06:00/;s/nodes=2/nodes=$p/;s/THREADS=32/THREADS=$n/" r.pbs > r${n}_${fr}-$to.pbs
qsub r${n}_${fr}-$to.pbs




    {'x-per-page': '99',
        'vary': 'Accept-Encoding, Origin',
        'ratelimit-observed': '2',
        'x-request-id': '70e08cf1-87aa-4dbc-9b63-cd66ca03c68f',
        'x-content-type-options': 'nosniff',
        'x-prev-page': '',
        'x-total': '510406',
        'ratelimit-resettime': 'Sun, 19 May 2018 18:38:36 GMT',
        'x-runtime': '21.441289',
        'etag': 'W/"4f51921106409ee6f3ec9b944a8f4d2c"',
        'cache-control': 'max-age=0, private, must-revalidate',
        'transfer-encoding': 'chunked',
        'ratelimit-reset': '1526755116',
        'link': '<https://gitlab.com/api/v4/projects?archived=false&membership=false&order_by=created_at&owned=false&page=2&per_page=99&private_token=nCmwHw8owdsTfxkB9Nzs&simple=false&sort=desc&starred=false&statistics=false&with_custom_attributes=false&with_issues_enabled=false&with_merge_requests_enabled=false>;rel="next",
                 <https://gitlab.com/api/v4/projects?archived=false&membership=false&order_by=created_at&owned=false&page=1&per_page=99&private_token=nCmwHw8owdsTfxkB9Nzs&simple=false&sort=desc&starred=false&statistics=false&with_custom_attributes=false&with_issues_enabled=false&with_merge_requests_enabled=false>; rel="first",
                 <https://gitlab.com/api/v4/projects?archived=false&membership=false&order_by=created_at&owned=false&page=5156&per_page=99&private_token=nCmwHw8owdsTfxkB9Nzs&simple=false&sort=desc&starred=false&statistics=false&with_custom_attributes=false&with_issues_enabled=false&with_merge_requests_enabled=false>; rel="last"',
        'x-next-page': '2',
        'date': 'Sat, 19 May 2018 18:37:36 GMT',
        'x-total-pages': '5156',
        'x-frame-options': 'SAMEORIGIN',
        'ratelimit-remaining': '598',
        'content-encoding': 'gzip',
        'strict-transport-security': 'max-age=31536000',
        'server': 'nginx',
        'ratelimit-limit': '600',
        'x-page': '1',
        'content-type': 'application/json'}



# do for Chris   
#First fit the model
pairsf <- readRDS("../MODELS_PHASE4/RDSFiles/full_pairs.RDS");
pairs <- readRDS("../MODELS_PHASE5/match2345_pairs.RDS");

pairs$pairs$im = as.factor(pairs$pairs$"is_match");
vv = 1:dim(pairs$pairs)[1];
eq= c(1817022,2620775,3198135,3823436,4923380,2469101,5117770,429508 ,1674130,2018882,2469100,
     2863731,4667324,5063908,5351827,1674131 ,2566524,3924916,4669669,5028394,5066253,
      1814677,3198134,3305223,4454519,4603843,3647564,3943395,4452175,4667325,1425379,2552916,
       4452174,5026049,5030739,5115306,1814676,2021226,2550570);
fr=c(1117,1771,2183,1632,2265,2265,1221, 666,1900,487,1053,1075,1085,1085,1207,1411,748, 1411, 1221, 1632, 1076, 1148, 748,  774,  775, 774, 1115, 1053, 981,  185, 1093, 1899, 2099, 373, 966, 1094, 184, 2100, 2144, 424, 425, 1450, 1228, 1116,  775, 1117, 1364, 774,  1086, 1900, 1118, 1227, 1227);
to=c(1411, 666, 980,1085, 749, 748,487,1772,1365,1221,2160,1207,1632,1631,1076,1117,2264,1118, 486,  1086, 1207, 528, 2264, 1992, 1992, 775, 1409, 2161, 2183, 373, 1096, 1365, 1225, 184, 1619,1096, 373, 1225, 2255, 229, 230, 1683, 2099, 1410, 1991, 1410, 1899, 1991, 1631, 1364, 1410, 2100, 2099);
for (i in c(1:length(fr))){
 pairs$pairs$im[pairs$pairs$id1==fr[i]&pairs$pairs$id2==to[i]]=1;
 pairs$pairs$im[pairs$pairs$id2==fr[i]&pairs$pairs$id1==to[i]]=1;
}
pp=pairs$pairs[eq,]
pp=pp[pp$im==0,]
for (i in c(1:length(pp$id1))){
  pairs$pairs$im[pairs$pairs$id1==pp$id1[i]&pairs$pairs$id2==pp$id2[i]]=1;
  pairs$pairs$im[pairs$pairs$id2==pp$id1[i]&pairs$pairs$id1==pp$id2[i]]=1;
}

frommtch = cbind(pairs$data1$a[pairs$pairs[pairs$pairs$im==1,"id1"]],pairs$data1$a[pairs$pairs[pairs$pairs$im==1,"id2"]]);
frommtch = frommtch[frommtch[,1]!=frommtch[,2],];
n=dim(pairs$data1)[1];
idmatch = paste(pairs$pairs$id1,pairs$pairs$id2,sep=";");

## zero out false (common names)
cn = read.table("FreqNames200.csv", sep=',', header=T, colClasses="character")
cfn = read.table("FreqFirstNames200.csv", sep=',', header=T, colClasses="character")
cln = read.table("FreqLastNames200.csv", sep=',', header=T, colClasses="character")
cun = read.table("FreqUsernames200.csv", sep=',', header=T, colClasses="character")
ce = read.table("../MODELS_PHASE4/FreqEmail200.csv", sep=',', header=T, colClasses="character")


library(data.table);
frq = fread("/home/audris/as.11.txt",sep=";",quote="",colClasses="character", header=F, strip.white=F);
names(frq) = c("un","n","fn","ln","e","a");
frq$badn = match(unlist(frq$n), cn$n, nomatch=0)>0
frq$badfn = match(unlist(frq$fn), cfn$fn, nomatch=0)>0
frq$badln = match(unlist(frq$ln), cln$ln, nomatch=0)>0
frq$badun = match(unlist(frq$un), cun$un, nomatch=0)>0
frq$bade = match(unlist(frq$e), ce$e, nomatch=0)>0

#namec <- table(frq$n)
#emailc <- table(frq$e)
lnamep <- table(frq$ln)
fnamep <- table(frq$fn)
unamep <- table(frq$un)
pairs$data1$lnf = log10(1/lnamep[match(pairs$data1$ln,names(lnamep),nomatch=0)])
pairs$data1$fnf = log10(1/fnamep[match(pairs$data1$fn,names(fnamep),nomatch=0)])
pairs$data1$unf = log10(1/unamep[match(pairs$data1$un,names(unamep),nomatch=0)])
pairs$pairs$fnf = as.vector(outer(pairs$data1$fnf, pairs$data1$fnf, "+"))
pairs$pairs$lnf = as.vector(outer(pairs$data1$lnf, pairs$data1$lnf, "+"))
pairs$pairs$unf = as.vector(outer(pairs$data1$unf, pairs$data1$unf, "+"))

n=dim(pairsf$data1)[1];
removeHom = function (zz){
n=dim(zz$data1)[1];
id = (1:n)[match(zz$data1$n, cn$n, nomatch=0)>0];
zz$pairs$n[match(zz$pairs$id1, id, nomatch=0)>0] = 0;
zz$pairs$n[match(zz$pairs$id2, id, nomatch=0)>0] = 0;
zz$pairs$nf[match(zz$pairs$id1, id, nomatch=0)>0] = -20;
zz$pairs$nf[match(zz$pairs$id2, id, nomatch=0)>0] = -20;

id = (1:n)[match(zz$data1$fn, cfn$fn, nomatch=0)>0];
zz$pairs$fn[match(zz$pairs$id1, id, nomatch=0)>0] = 0;
zz$pairs$fn[match(zz$pairs$id2, id, nomatch=0)>0] = 0;
zz$pairs$fn1f[match(zz$pairs$id1, id, nomatch=0)>0] = -20;
zz$pairs$fn1[match(zz$pairs$id2, id, nomatch=0)>0] = -20;

id = (1:n)[match(zz$data1$ln, cln$ln, nomatch=0)>0];
zz$pairs$ln[match(zz$pairs$id1, id, nomatch=0)>0] = 0;
zz$pairs$ln[match(zz$pairs$id2, id, nomatch=0)>0] = 0;
zz$pairs$ln1f[match(zz$pairs$id1, id, nomatch=0)>0] = -20;
zz$pairs$ln1f[match(zz$pairs$id2, id, nomatch=0)>0] = -20;

id = (1:n)[match(zz$data1$un, cun$un, nomatch=0)>0];
zz$pairs$un[match(zz$pairs$id1, id, nomatch=0)>0] = 0;
zz$pairs$un[match(zz$pairs$id2, id, nomatch=0)>0] = 0;
zz$pairs$unf[match(zz$pairs$id1, id, nomatch=0)>0] = -20;
zz$pairs$unf[match(zz$pairs$id2, id, nomatch=0)>0] = -20;

id = (1:n)[match(zz$data1$e, cun$e, nomatch=0)>0];
zz$pairs$e[match(zz$pairs$id1, id, nomatch=0)>0] = 0;
zz$pairs$e[match(zz$pairs$id2, id, nomatch=0)>0] = 0;
zz$pairs$ef[match(zz$pairs$id1, id, nomatch=0)>0] = -20;
zz$pairs$ef[match(zz$pairs$id2, id, nomatch=0)>0] = -20;
}

removeHom(pairsf)
removeHom(pairs)

clean = function(zz){
  zz$e[is.na(zz$e)] = 0;
  zz$fn[is.na(zz$fn)] = 0;
  zz$ln[is.na(zz$ln)] = 0;
  zz$n[is.na(zz$n)] = 0;
  zz$un[is.na(zz$un)] = 0;
}
clean(pairs$pairs);

close= apply(pairs$pairs[,c("n","e","ln","fn","un","ifn")], 1, max) > .8 | pairs$pairs$d2vSim>0;
preds = c("n","e","ln","fn","un", "ifn", "lnf", "fnf", "unf", "d2vSim", "im");
dd=pairs$pairs[close,preds];
rfCr = randomForest(im ~ ., dd, importance=T);
prfCr = predict(rfCr,pairs$pairs[,preds]);
table(prfCr,pairs$pairs$im)

prfCr       0       1
    0 5493392      24
    1       0    5609

save(rfCr, file = "/home/audris/rfCr.RData");


mm = pairs$pairs$"is_match" == 1;
nmm=!mm & apply(pairs$pairs[,c("n","e","ln","fn","un","ifn","ln1","fn1")], 1, max) > .8 | pairs$pairs$ad>0|pairs$pairs$tdz>.9|pairs$pairs$d2vSim>0;
nnmm=!mm & !nmm
vv = 1:dim(pairs$pairs)[1];
sel=sample(0:4,sum(mm),replace=T);
sel1=sample(0:4,sum(nmm),replace=T);
sel2=sample(0:4,sum(nnmm),replace=T);
rfCR2 = list();
pCR2=list();
for (i in 0:4){
  mm1t = vv[mm][sel!=i];
  mm1v = vv[mm][sel==i];
  mm0t = vv[nmm][sel1!=i];
  mm0v = vv[nmm][sel1==i];
  mm0v = c(mm0v, vv[nnmm][sel2==i])
  dt=pairs$pairs[c(mm1t,mm0t),preds];
  dv=pairs$pairs[c(mm1v,mm0v),preds];
  rfCR2[[i+1]] = randomForest(im ~ ., dt, importance=T);
  pCR2[[i+1]] = predict(rfCR2[[i+1]],dv);
  print (table(pCR2[[i+1]],dv$im));
}

          0       1
  0 1099498       5
  1       0    2100

          0       1
  0 1099157       6
  1       5    2067

          0       1
  0 1098077       2
  1       2    2120
   
          0       1
  0 1100059       9
  1       2    2013

################
crl = fread("/home/audris/4096.L.txt",sep=";",quote="",colClasses="character", header=F, strip.white=F);
names(crl) = c("a", "ch", "o", "off");
mm=match(unlist(crl[,1]),unlist(frq[,6]),nomatch=0);
matches = frq[mm,];
select = match(matches$ln,names(lnamep),nomatch=0);
matches$lnf = rep (0,dim(matches)[1])
matches$lnf[select>0] = log10(1/lnamep[select])
select = match(matches$fn,names(fnamep),nomatch=0);
matches$fnf = rep (0,dim(matches)[1])
matches$fnf[select>0] = log10(1/fnamep[select])
select = match(matches$un,names(unamep),nomatch=0);
matches$unf = rep (0,dim(matches)[1])
matches$unf[select>0] = log10(1/unamep[select])
matches$ch = crl$ch [mm>0];
matches$o = crl$o [mm>0];

names(matches)
#save(matches, file = "/home/audris/matches.RData");
write.table(matches[,c("a","fnf","lnf","unf","badfn","badln","badun","ch","o")], file = "/home/audris/matches.csv",row.names=F,sep=";",quote=F);




#now calculate predictions

extra = read.table("/home/audris/0.p.csv.bad", sep=";",quote="",header=T);
names(extra) = c("ch0","o0","ch1","o1","a0","a1","d2v0","n", "e","ln","fn","un","ifn","fnf","lnf", "unf","d2vSim");
extra$im = rep(0,dim(extra)[1])
dd=pairs$pairs[close,preds];
dd = rbind(dd, extra[,preds])

rfCr1 = randomForest(im ~ ., dd, importance=T);
prfCr1 = predict(rfCr1,pairs$pairs[,preds]);
table(prfCr1,pairs$pairs$im)
prfCr1       0       1
     0 5493392      23
     1       0    5610
table(predict(rfCr1,extra[,preds]));
   0    1 
2128    0 


cut -d\; -f1-4,7- ~/0.p.csv > ~/0.p.csv1
extra1 = read.table("/home/audris/0.p.csv1", sep=";",quote="",header=T);
names(extra1) = c("ch0","o0","ch1","o1","d2v0","n", "e","ln","fn","un","ifn","fnf","lnf", "unf","d2vSim");
extra1$im = rep(0,dim(extra1)[1])
extra1$im[extra1$e==1] = 1;
extra1p = extra1[,preds];

dd = rbind(dd, extra1p);
#clean(dd);

rfCr2 = randomForest(im ~ ., dd, importance=T);
prfCr2 = predict(rfCr2,pairs$pairs[,preds]);
table(prfCr2,pairs$pairs$im)
prfCr2       0       1
     0 5493392      23
     1       0    5610

table(predict(rfCr2,extra[,preds]));
   0    1
2128    0

table(predict(rfCr2,extra1p),extra1p$im);

      0   1
  0 794   0
  1   0  98


save(rfCr2, file = "/home/audris/rfCr2.RData");

extra2 = read.table("/home/audris/0.p.csv", sep=";",quote="",header=T,comment.char="");

####################################################################################################



#
for i in {0..4095}; do cat 4096_0-1/outL.$i | awk 'BEGIN {i=1}{print $0";'$i';"i++}'; done | awk -F\; 'BEGIN {i=0}{print $1";"$3";"$4";"i++}' | gzip > 4096.L



seq 0 16 4079 |while read i; do sed "s/NNN/$i/" extr.pbs > extr1.pbs; qsub extr1.pbs; done

seq 0 16 1024 |while read i; do sed "s/NNN/$i/" pred.pbs > pred.$i.pbs; sed "s/NNN/$i/" pred.r > pred.$i.r; qsub pred.$i.pbs; done



################################
# Compare with the Bogdan's algorithm
#make transitive closure
load("/home/audris/rfmodelsFullP7.c.RData")


cat /data/play/cbogart/openstack/aliasmap_bogdanv.json | grep : | sed 's/^\s*"//;s/": "/;/;s/",\s*$//' > bogdan.map

x=bogd$a[is.na(match(bogd$a,pairsf$data1$autf))]
> x
[1] "Chung Chih"
[2] "Xian Dong"
[3] "Matt Dietz and John Yolo Perkins <matt.dietz@rackspace.com"
[4] "OTSUKA"
[5] "Li"

cat /data/play/cbogart/openstack/aliasmap_bogdanv.json | grep : | grep 'Matt Dietz and John '
    "Matt Dietz and John Yolo Perkins <matt.dietz@rackspace.com": "Cerberus <matt.dietz@rackspace.com>",

pairsf$data1$autf[grep ("Matt Dietz", pairsf$data1$autf)]
[1] "Matt Dietz <matt.dietz@rackspace.com>"
[2] "Matt Dietz <matthew.dietz@gmail.com>"
[3] "Matt Dietz and John Yolo Perkins <matt.dietz@rackspace.com, john.perkins@rackspace.com>"

[1] "Xian Dong, Meng <mengxd@cn.ibm.com>"     
[2] "Xian Dong, Meng <mengxiandong@gmail.com>"

bogd <-read.table("/home/audris/bogdan.map", sep=';',quote="",colClasses=c("character","character"))
names(bogd) = c("a","id");
pairsf$data1$autf = utf8_encode(pairsf$data1$a);

#pairsf$data1$autf[grep ("Ulrik", pairsf$data1$autf)]
mm=match(bogd$a,pairsf$data1$autf);
mm1=match(bogd$id,pairsf$data1$autf);
res = cbind(mm,mm1)
res = res[!is.na(apply(res,1,sum)),];


library(readr)
library(igraph)
n=dim(pairsf$data1)[1]
library(readr)
library(igraph)
gbo <- make_empty_graph(n, directed = FALSE); # empty graph
gbo <- set_vertex_attr(gbo, "a", value = pairsf$data1$a);
gbo <- set_vertex_attr(gbo, "id", value = 1:n);
gbo <- add_edges(gbo, as.vector(t(res)));  
clustgbo <- components(gbo, "weak") # get clusters
blocksgbo <- data.frame(id=V(gbo)$id, block=clustgbo$membership) # block number  


g <- make_empty_graph(n, directed = FALSE); # empty graph
g <- set_vertex_attr(g, "a", value = pairsf$data1$a);
g <- set_vertex_attr(g, "id", value = 1:n);
g=simplify(g,remove.multiple = TRUE, remove.loops = TRUE)          
lnk=fullP7.c==2
lnk1= pairsf$pairs[lnk,1:2]

g <- add_edges(g, as.vector(t(lnk1)));
clustg <- components(g, "weak") # get clusters
blocksg <- data.frame(id=V(g)$id, block=clustg$membership) # block number  

tng = table(blocksg$block)
tbo = table(blocksgbo$block)
badSplitBo = c();
for (id in names(tng[tng>1])){
    ids = blocksgbo[match(blocksgbo[,1],blocksg[blocksg$block==id,1],nomatch=0)>0,2];
    badSplitBo = c(badSplitBo,length(table(ids)));
}    
badClumpBo = c();
for (id in names(tbo)){
    cl = blocksgbo[blocksgbo$block==id,1];
    mm = blocksg[match(cl, blocksg$id),2];
    ids = blocksg[mm,2];
    badClumpBo = c(badClumpBo,length(table(ids)));
}
table(badSplitBo>1)
FALSE  TRUE 
  977  1979 

table(badClumpBo>1)
FALSE  TRUE 
14298    21 

#see match with manual 
#manNodes = names(table(c(pairs$data1$a[pairs$pairs[pairs$pairs$im==1,1]],
#    pairs$data1$a[pairs$pairs[pairs$pairs$im==1,2]])));
nm = dim(pairs$data1)[1];
gm <- make_empty_graph(nm, directed = FALSE); # empty graph
gm <- set_vertex_attr(gm, "a", value = pairs$data1$a);
gm <- set_vertex_attr(gm, "id", value = 1:nm);
gm=simplify(gm,remove.multiple = TRUE, remove.loops = TRUE);          
amatch=paste(pairs$data1$a[pairs$pairs$id1],pairs$data1$a[pairs$pairs$id2], sep = ";");
amatchf=paste(pairsf$data1$a[pairsf$pairs$id1],pairsf$data1$a[pairsf$pairs$id2], sep = ";");
P7.c = fullP7.c[match(amatch,amatchf)] == 2;
lnk1m = pairs$pairs[P7.c,1:2];
gm <- add_edges(gm, as.vector(t(lnk1m)));
clustgm <- components(gm, "weak"); # get clusters
blocksgm <- data.frame(id=V(gm)$id, block=clustgm$membership); # block number  

gt <- make_empty_graph(nm, directed = FALSE); # empty graph
gt <- set_vertex_attr(gt, "a", value = pairs$data1$a);
gt <- set_vertex_attr(gt, "id", value = 1:nm);
gt <- simplify (gt,remove.multiple = TRUE, remove.loops = TRUE);
gt <- add_edges (gt, as.vector(t(pairs$pairs[pairs$pairs$im==1,1:2])));
clustgt <- components(gt, "weak"); # get clusters
blocksgt <- data.frame(id=V(gt)$id, block=clustgt$membership); # block number  

nm = dim(pairs$data1)[1];
gb <- make_empty_graph(nm, directed = FALSE); # empty graph
gb <- set_vertex_attr(gb, "a", value = pairs$data1$a);
gb <- set_vertex_attr(gb, "id", value = 1:nm);
pairs$data1$autf = utf8_encode(pairs$data1$a);
#pairsf$data1$autf[grep ("Ulrik", pairsf$data1$autf)]
mm=match(bogd$a,pairs$data1$autf);
mm1=match(bogd$id,pairs$data1$autf);
res = cbind(mm,mm1)
res = res[!is.na(apply(res,1,sum)),];
gb=simplify(gb,remove.multiple = TRUE, remove.loops = TRUE);
gb <- add_edges (gb, as.vector(t(res)));
clustgb <- components(gb, "weak"); # get clusters
blocksgb <- data.frame(id=V(gb)$id, block=clustgb$membership); # block number  

tn = table(blocksgt$block);
tb = table(blocksgb$block);
tm = table(blocksgm$block);

badSplitB = c();
for (id in names(tn[tn>1])){
    ids = blocksgb[match(blocksgb[,1],blocksgt[blocksgt$block==id,1],nomatch=0)>0,2];
    badSplitB = c(badSplitB,length(table(ids)));
}    
badClumpB = c();
for (id in names(tb)){
    cl = blocksgb[blocksgb$block==id,1];
    mm = blocksgt[match(cl, blocksgt$id),2];
    ids = blocksgt[mm,2];
    badClumpB = c(badClumpB,length(table(ids)));
}
badSplitR = c();
for (id in names(tn[tn>1])){
    ids = blocksgm[match(blocksgm[,1],blocksgt[blocksgt$block==id,1],nomatch=0)>0,2];
    badSplitR = c(badSplitR,length(table(ids)));
}    
badClumpR = c();
for (id in names(tm)){
    cl = blocksgm[blocksgm$block==id,1];
    mm = blocksgt[match(cl, blocksgt$id),2];
    ids = blocksgt[mm,2];
    badClumpR = c(badClumpR,length(table(ids)));
}
table(badSplitB>1)
FALSE  TRUE 
  201   806 

table(badSplitR>1)
FALSE  TRUE 
  992    15 

table(badClumpB>1)
FALSE  TRUE 
 2089     1 

table(badClumpR>1)
FALSE  TRUE 
 1054     8 



# Do cross-rate reliability






