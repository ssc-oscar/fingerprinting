suppressMessages(library(pbdMPI,quietly = TRUE))
suppressMessages(library(pbdDMAT,quietly = TRUE))
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
	fn <- paste0(filepath,'/',filename,comm.rank())
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


