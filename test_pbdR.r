source('reader2.r')
suppressMessages(library(pbdMPI,quietly = TRUE))
suppressMessages(library(pbdDMAT,quietly = TRUE))
suppressMessages(library(pmclust,quietly = TRUE))
library(data.table)

init.grid()

#df <- read.csv('doc2vec_vectors.txt', header = FALSE)
#df <- read.csv('sample.txt', header = FALSE)
#data <- read.csv.ddmatrix("sample.txt ", sep = ";", nrows=20, ncols=201, header=TRUE, bldim=4, num.rdrs=2, ICTXT =0)

#tags <- df[,1]
#df[1] <- NULL
#distMat <- as.ddmatrix(df)
#print (data)
#row.names(df) <- tags #Set row index as tags
#print (row.names(df[c(1,101)]))
#print (df[1:5,1:2])
#finalize()

start.time <- Sys.time()
#data_dim <- 2
#fn <- paste0('TEST_DATA/a.0',comm.rank())
#x <- fread(fn, header = F, sep = ';',colClasses=c("character",rep("numeric",data_dim)))

global_rows <- 108
ncols <- 2

ret_val <- reader('TEST_DATA','a.0',global_rows,ncols)
barrier()

end.time <- Sys.time()
t1 <- end.time - start.time
comm.print ("TIME TO READ")
comm.print (t1)

tags <- ret_val$labels
x <- ret_val$data
comm.print (x)
#nn = x[,1];
#x = as.matrix(x[,-1]);
#row.names(x) = nn;
#comm.print(x)

#nr = allreduce(nrow(x))
#nc = allreduce(ncol(x))
#comm.print(paste("the overall matrix row is:", nr));
#comm.print(paste("the overall matrix col is:", nc));

ret.kms <- pkmeans(x, K = 12)
comm.print ("RESULTS: ret.kms <- pkmeans(x, K = 12)")
comm.print (ret.kms)

#a = data.frame(.pmclustEnv$Z.spmd);
comm.print (".pmclustEnv$CONTROL")
comm.print (.pmclustEnv$CONTROL)
comm.print (".pmclustEnv$PARAM")
comm.print (.pmclustEnv$PARAM)
#comm.print(a);

a1 <- do.call('rbind',allgather(.pmclustEnv$Z.spmd))
b1 <- data.frame(a1)
comm.print(b1)
all_tags <- do.call('rbind',allgather(tags))
comm.print(all_tags)
#results <- do.call('cbind',b1, tags)
#comm.print(results)

#res <- do.call('rbind',allgather(x))
#resNames <- do.call('c',allgather(nn));
#comm.print("resNames")
#comm.print(resNames)
#comm.print(row.names(res)[1:10]);
#comm.print(c(eval(parse(text=resNames[[1]])), eval(parse(text=resNames[[2]]))))

#dist.res <- comm.dist(res, return.type = "gbd")
#comm.print(paste("the overall dist matrix is:", dim(dist.res)));
#dist.ret.kms <- pkmeans(dist.res, K = 3)
#comm.print (dist.ret.kms)
saveRDS(b1,"clusterResults")
#saveRDS(ret.kms,"clusterResults")
#data <- do.call('rbind',allgather(x))
#all_data <- data.frame(data)
#comm.print(data)
#saveRDS(all_data,"data")
finalize()
	
if (FALSE){
if (comm.rank() == 0){
  x <- fread('sample.txt', header = F, sep = ',')
  tags <- x[,1]
  x[ , c(1) := NULL]
} else {
  x <- NULL
}
print (x)

for (i in 1:comm.size()){
  fn <- paste0('s.0',comm.rank())
  comm.print(fn)
  x <- fread(fn, header = F, sep = ',')
  tags <- x[,1]
  x[ , c(1) := NULL]
  #x1 <- as.ddmatrix(x)
  comm.print (x)
  comm.print (paste0('Comm.size = ', comm.size()))
}

#y1 <- gather(x1)
#y1 <- do.call("rbind", allgather(x1))
#comm.print("Gathered")
#comm.print(y1)

#dist.x1.common <- comm.dist(x1)
dist.X.gbd <- comm.dist(x, return.type = "gbd")
dist.X.df <- do.call("rbind", allgather(dist.X.gbd))
comm.print(dist.X.df)

}

