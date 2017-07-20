source('reader2.r')
suppressMessages(library(pmclust,quietly = TRUE))

init.grid()
global_rows <- 107
global_cols <- 2
start.time <- Sys.time()
ret_val <- reader('TEST_DATA','a.0',global_rows,global_cols)
barrier()
read.time <- Sys.time()
t1 <- read.time - start.time
comm.print (t1)

#Clustering
comm.set.seed(123, diff = TRUE)
tags <- ret_val$labels
X.dmat <- ret_val$data
K <- 2

#comm.print ('TAGS:')
#comm.print (tags)
t <- do.call('rbind',allgather(tags))
#comm.print (t,nrow=107)

PARAM.org <- set.global.dmat(K = K)
comm.print("2")
#comm.print(PARAM.org)
#comm.print(.pmclustEnv$CONTROL)

PARAM.org <- initial.em.dmat(PARAM.org)
comm.print("3")
#comm.print(PARAM.org)
#comm.print(.pmclustEnv$CONTROL)

PARAM.new <- em.step.dmat(PARAM.org)
comm.print("4")
#comm.print(PARAM.new)
#comm.print(.pmclustEnv$CONTROL)

em.update.class.dmat()
#mb.print(PARAM.new, .pmclustEnv$CHECK)
comm.print("5")

PARAM.new <- em.step.dmat(PARAM.org)
em.update.class.dmat()
comm.print("6")

PARAM.new <- em.step.dmat(PARAM.org)
em.update.class.dmat()
comm.print("7")
#mb.print(PARAM.new, .pmclustEnv$CHECK)

### Get results.
N.CLASS <- get.N.CLASS.dmat(K)
end.time <- Sys.time()
t2 <- end.time - start.time
comm.print (t2)

comm.print('RESULTS')
comm.print(paste0("# of class:", N.CLASS, "\n"))

# dont gather ddmatrix !
#a1 <- do.call('rbind',allgather(.pmclustEnv$Z.dmat))

comm.print (.pmclustEnv$Z.dmat)

topNProb <- function (x, n){
  oo = order(-x);
  tot = sum(x);
  return (x[oo[1:n]]/tot)
  #return (list('cl'=oo[1:n], 'prob'=x[oo[1:n]]));
}

topNClust <- function (x, n){
  oo = order(-x);
  return (oo[1:n]);
}

nthCl <- function (x, n){
  oo = order(-x);
  #tot = sum(x);
  return (oo[n]);
}

probability <- 1
row <- 1
col <- 2
a1=apply(.pmclustEnv$Z.dmat, row, nthCl, probability)
a2=as.matrix(a1)
a3=cbind(t,a2)
comm.print(a3,nrow=107)

b1=apply(.pmclustEnv$Z.dmat, row, topNClust, 4)
b2 = as.matrix(t(b1))
b3=cbind(t,b2)
comm.print(b3,nrow=107)

t1=apply(.pmclustEnv$Z.dmat, row, topNProb, 4)
t2=as.matrix(t(t1))
#comm.print(t2)
t3=cbind(t,t2)
comm.print(t3,nrow=107)
#ret.mb1 <- pmclust(ret_val$data, K = 9, algorithm = 'em.dmat')

finalize()
