#dataDistributionCOSINE.r 

#source('read_to_gbd.r')
suppressMessages(library(pmclust,quietly = TRUE))
suppressMessages(library(pbdDMAT,quietly = TRUE))
library(pbdMPI)
library(pbdIO)

init.grid()


library(data.table)

#Param filename specifies the split files including path
#Param nrows is the total number of rows (sum of rows across all data_files)
#Param ncols is the total number of columns across the entire dataset (sum of cols across all data files)
#Reads and distributes the parts  of data files across multiple threads 
#Number of files = Number of processors
#Run with reader('TEST_DATA','a.0',110,18)

reader <- function(filepath,filename,total_rows,ncols){
        nprocs <- comm.size()
        nrows = ceiling(total_rows/nprocs)
        if (comm.rank() == nprocs - 1){
           nrows = total_rows - (nprocs-1)*nrows 
        }
        data_dim <- ncols
        dim_full <- c(total_rows, ncols)
        MYCTXT <- 2
        blacs_ <- base.blacs(MYCTXT)
        if (comm.rank() < 10){
                fn <- paste0(filepath,'/',filename,'0',comm.rank())
        }else if (comm.rank() >= 10 && comm.rank() < 100){
                fn <- paste0(filepath,'/',filename,comm.rank())
        }else{
                fn <- paste0(filepath,'/',filename,comm.rank())
        }

        df <- fread(fn, header = F, sep = ';', colClasses=c("character",rep("numeric",data_dim)))
        tags = df[,1]
        df = df[,-1]

        mdf <- as.matrix(df)
        ldim <- dim(mdf)
        if (nrows != ldim[1]){
            print(paste("error: wrong input:", nrows, ldim))
        }
        dist_mdf <- new("ddmatrix", Data=mdf, dim=dim_full, ldim=ldim, bldim=ldim, ICTXT=MYCTXT)
        comm.print("Distributed Dense Matrix Dimensions:")
        comm.print(dist_mdf)
        return (list(labels=tags,data=dist_mdf))
}

comm.quantile <- function(x, probs=seq(0, 1, 0.25), na.rm=FALSE, names=TRUE,
                          verbose=0) {
  N <- allreduce(length(x)) - allreduce(sum(is.na(x)))
  probs <- sort(probs)
  np <- length(probs)
  
  ## TODO is there a native R function for this?
  format_perc <- function(x, digits = max(2L, getOption("digits")),
                          probability = TRUE, use.fC = length(x) < 100, ...) {
    if (length(x)) {
      if (probability)
        x <- 100 * x
      paste0(if (use.fC)
        formatC(x, format = "fg", width = 1, digits = digits)
        else format(x, trim = TRUE, digits = digits, ...), "%")
    }
    else character(0)
  }
  q_names <- format_perc(probs)
  
  f.quant <- function(q, prob) {
    sm = allreduce(sum(x <= q, na.rm=TRUE), op="sum");
    #comm.print (c(sm, q, prob));
    sm/N - prob
  }
  
  q_val <- NULL
  q_lo <- allreduce(min(x, na.rm=na.rm), op="min")
  q_hi <- allreduce(max(x, na.rm=na.rm), op="max")
  if(verbose) comm.cat(comm.rank(), "All quant lo hi:", q_lo, q_hi, "\n")
  for(i in seq(1, np)) {
    ## loop over all and use previous for bounds
    ## TODO are there better bounds?    
    if(i > 1) q_lo <- q_val[i - 1]
    if(verbose > 1) {
         comm.cat(comm.rank(), "; lo hi:", q_lo, q_hi, "\n")
         comm.cat(comm.rank(), "; i:", i, "; prb:", probs[i], "\n")
    }
    if (probs[i] <= 1.0/N || q_lo == q_hi){
       q_val <- c(q_val, q_lo);
    }else{
      sm = allreduce(sum(x <= probs[i], na.rm=TRUE), op="sum");
      #comm.print (c(i, probs[i], sm))
      if (sm == 0){
        q_val <- c(q_val, q_lo);
      }else{
        q <- uniroot(f.quant, c(q_lo, q_hi), prob=probs[i])
        q_val <- c(q_val, q$root)
      }
    }
  }
  names(q_val) <- q_names
  q_val
}


global_rows <- 6
ncols <- 3

ret_val <- reader('SD','a.',global_rows,ncols)

barrier();

tags <- ret_val$labels
x <- ret_val$data
#x <- ret_val$Xgbd

start.time <- Sys.time();

#normalize x

nr=sqrt(rowSums(x*x));
#nrm = as.matrix(nr);
#comm.print(nrm);
#xm = as.matrix(x);
#comm.print(xm);
x1 = x/as.vector(nr);
rs = rowSums(x1*x1);
#rsm = as.matrix(rs);
#comm.print(rsm);

#calculate distance

dO <- x1 %*% t(x1);
#dOm = as.matrix(dO);
#comm.print("dOm");
#comm.print(dOm);
barrier();


lv = as.vector(submatrix(dO));
qt = comm.quantile(lv,probs=1-c(1:5/500,2:5/50,2:5/10),verbose=0);
#qt1 = comm.quantile(as.vector(dOm),probs=1-c(1:5/500,2:5/50,2:5/10));
comm.print(qt);
#comm.print(qt1);

myMult = function (dO, q, nn){
  d = dO;
  d_sub <- submatrix(d);
  d_sub[d_sub<q-1e-15] = 0;
  d_sub[d_sub>0] = 1/log(global_rows);
  d@Data <- d_sub;
  dd = d %*% t(d);
  for (mlt in 1:nn){
    dd = dd %*% t(d);
  }
  dd1 = dd %*% t(d);
  sdd1 = submatrix(dd1)
  sdd1[sdd1>0] = 1;
  dd1@Data = sdd1;
  sdd = submatrix(dd);
  sdd[sdd>0] = 1;
  dd@Data = sdd;
  RS <- rowSums(dd);
  RS1 <- rowSums(dd1);
  dif = sum(abs(as.matrix(RS-RS1)));
  nOne = allreduce(sum(submatrix(RS1) == 1), op="sum");
  nMax = allreduce(max(submatrix(RS1)), op="max");
  return(c(nMax, nOne, dif));
}

for (off in 1:length(qt)){
  res = myMult (dO, qt[off]-1e-15, 10); 
  comm.print(c(qt[off],res))
  #compare dd1 and dd if converged
  if (res[3] > 1e-15){
    #increase number of multiplications
    comm.print("did not converge");
    #res = myMult (dO, qt[off]-1e-15, 20);
  }
  #apply to swap cluster sizes
}


end.time <- Sys.time()
t <- end.time - start.time
comm.print(t)
#Normalize
#Use quantiles

