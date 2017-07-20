source('reader.r')
suppressMessages(library(pmclust,quietly = TRUE))

init.grid()

topNProb <- function (x, n){
  oo = order(-x);
  tot = sum(x);
  return (x[oo[1:n]]/tot)
}

topNClust <- function (x, n){
  oo = order(-x);
  return (oo[1:n]);
}

nthCl <- function (x, n){
  oo = order(-x);
  return (oo[n]);
}

#Define dimensions of data
global_rows <- 2338516
nrows <- 2000
global_cols <- 200

start.time <- Sys.time()
ret_val <- reader('model1.0','vector_model1.0.part',global_rows,nrows,global_cols)
barrier()
read.time <- Sys.time()
t1 <- read.time - start.time
comm.print ("---------TIME TO READ---------")
comm.print (t1)

comm.set.seed(123, diff = TRUE)
tags <- ret_val$labels
t <- do.call('rbind',allgather(tags))
X.dmat <- ret_val$data
###Set number of clusters
K <- 1000000

PARAM.org <- set.global.dmat(K = K)

PARAM.org <- initial.em.dmat(PARAM.org)

PARAM.new <- em.step.dmat(PARAM.org)

em.update.class.dmat()
mb.print(PARAM.new, .pmclustEnv$CHECK)

### Get results.
N.CLASS <- get.N.CLASS.dmat(K)

end.time <- Sys.time()
t2 <- end.time - start.time
comm.print ("---------TIME TO CLUSTER---------")
comm.print (t2)

#comm.print('RESULTS')
#comm.print(paste0("# of class:", N.CLASS, "\n"))

# dont gather ddmatrix !
#a1 <- do.call('rbind',allgather(.pmclustEnv$Z.dmat))

comm.print (.pmclustEnv$Z.dmat)

row <- 1
col <- 2

a1=apply(.pmclustEnv$Z.dmat, row, nthCl, 1)
a2=as.matrix(a1)
a3=cbind(t,a2)
comm.print(a3)

b1=apply(.pmclustEnv$Z.dmat, row, topNClust, 4)
b2 = as.matrix(t(b1))
b3=cbind(t,b2)
comm.print(b3)

t1=apply(.pmclustEnv$Z.dmat, row, topNProb, 4)
t2=as.matrix(t(t1))
t3=cbind(t,t2)
comm.print(t3)

finalize()
