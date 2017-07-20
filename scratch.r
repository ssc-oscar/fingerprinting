suppressMessages(library(pbdMPI,quietly = TRUE))
suppressMessages(library(pbdDMAT,quietly = TRUE))
suppressMessages(library(pmclust,quietly = TRUE))
library(data.table)

init.grid()

linecount <- function(file)
{
  return (length(readLines(file)))
}

#Param filename specifies the split files including path
#Param nrows is the total number of rows (sum of rows across all data_files)
#Param ncols is the total number of columns across the entire dataset (sum of cols across all data files)
#Reads and distributes the parts  of data files across multiple threads 
#Number of files = Number of processors
#Run with reader('TEST_DATA','a.0',110,18)

reader <- function(filepath,filename,nrows,ncols){
	#Count files in dir()
	filecount = 0
	for (x in dir(filepath)){ 
		filecount = filecount + 1 
	}
	comm.print (filecount)
	nprocs <- comm.size()
	data_dim <- ncols
	total_ncols <- ncols*filecount
	dim_full <- c(nrows, total_ncols)
	MYCTXT <- 2
	blacs_ <- base.blacs(MYCTXT)

	fn <- paste0(filepath,'/',filename,comm.rank())
	df <- fread(fn, header = F, sep = ';', colClasses=c("character",rep("numeric",data_dim)))
	tags = df[,-1]

	mdf <- as.matrix(tags)
	ldim <- dim(mdf)
	dist_mdf <- new("ddmatrix", Data=mdf, dim=dim_full, ldim=ldim, bldim=ldim, ICTXT=MYCTXT)
	comm.print(dist_mdf)

	comm.print("Sum of all elements in Column 1 in matrix")
	s1 <- allreduce(sum(mdf[,1]))
	comm.print (s1)
	comm.print("Sum of all elements in Column 1 in ddmatrix")
	s2 <- allgather(sum(dist_mdf[,1]))
	comm.print (s2)

	finalize()
}

reader('TEST_DATA','a.0',110,2)

data_dim <- 2
ncols <- 18
nrows <- 110
dim_full <- c(nrows, ncols)
MYCTXT <- 2
#tmpbl <- c(nlines, ncols)
blacs_ <- base.blacs(MYCTXT)

fn <- paste0('TEST_DATA/a.0',comm.rank())
x <- fread(fn, header = F, sep = ';', colClasses=c("character",rep("numeric",data_dim)))
nn = x[,-1]

mx <- as.matrix(nn)
ldim <- dim(mx)
s1 <- allreduce(sum(mx[,1]))
comm.print (s1)

dx <- new("ddmatrix", Data=mx, dim=dim_full, ldim=ldim, bldim=ldim, ICTXT=MYCTXT)
comm.print(dx)
#dx <- pbdDMAT::redistribute(dx=dx, bldim=4, ICTXT=MYCTXT)
#comm.print(dx)
#dx <- as.ddmatrix(mx)
comm.print("Sum of all elements in Column 1 in ddmatrix")
s2 <- allgather(sum(dx[,1]))
comm.print (s2)

#row <- allreduce(nrow(dx1))
#col <- allreduce(ncol(dx1))
#comm.print(paste("the overall matrix row is:", row));
#comm.print(paste("the overall matrix col is:", col));
#comm.print(allreduce(ncol(dx)))
finalize()
