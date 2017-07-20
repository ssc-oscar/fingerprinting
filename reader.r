suppressMessages(library(pbdMPI,quietly = TRUE))
suppressMessages(library(pbdDMAT,quietly = TRUE))
library(data.table)
library(stringr)

#Param filepath specifies the relative filepath
#Param filename specifies the split filenames
#Param nrows is the total number of rows (sum of rows across all data_files)
#Param ncols is the total number of columns across the entire dataset (sum of cols across all data files)
#Reads and distributes the parts  of data files across multiple threads 
#Number of files = Number of processors
#Run with reader('TEST_DATA','a.0',110,18)
#Handles <10,000 files

reader <- function(filepath,filename,total_rows,nrows,ncols){
	nprocs <- comm.size()
	
	comm.print("Num Procs")
        comm.print(nprocs) 
	#nrows = ceiling(total_rows/nprocs)
	comm.print("Nrows")
        comm.print(nrows)
 
        if (comm.rank() == nprocs - 1){
           nrows = total_rows - (nprocs-1)*nrows 
        }
	data_dim <- ncols
	dim_full <- c(total_rows, ncols)
	MYCTXT <- 2
	blacs_ <- base.blacs(MYCTXT)
	if (comm.rank()<10){
		fn <- paste0('gunzip -c ',filepath,'/',filename,'000',comm.rank(),'.gz')
	}else if(comm.rank()>=10 && comm.rank()<100){
		fn <- paste0('gunzip -c ',filepath,'/',filename,'00',comm.rank(),'.gz')
	}else if(comm.rank()>=100 && comm.rank()<1000){
		fn <- paste0('gunzip -c ',filepath,'/',filename,'0',comm.rank(),'.gz')
	}else{
		fn <- paste0('gunzip -c ',filepath,'/',filename,comm.rank(),'.gz')
	}
	#fn <- paste0(filepath,'/',filename,comm.rank())
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


