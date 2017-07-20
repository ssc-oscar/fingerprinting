a = read.csv('a.ALL',sep = ';',header= F)
head(a)
kmeans(a[,-1],12)
data.frame(a[,1],clusts = kmeans(a[,-1],12))
data.frame(a[,1],clusts = kmeans(a[,-1],12)$cluster)
data.frame(name=a[,1],clusts = kmeans(a[,-1],12)$cluster)
data.frame(name=a[,1],clusts = kmeans(a[,-1],12)$cluster)
ks <- kmeans(a[,-1],12)
ks
data.frame(name=a[,1],clusts = ks$cluster)
table(ks$cluster)

