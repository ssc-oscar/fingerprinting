suppressMessages(library(pbdMPI,quietly = TRUE))
suppressMessages(library(pbdDMAT,quietly = TRUE))
suppressMessages(library(pmclust,quietly = TRUE))
library(data.table)

init()

barrier()
start.time <- Sys.time()
comm.print(start.time)

data_dim <- 200

dyn_count_csv = read.csv("dynamic_count_of_files", header=FALSE)

sum = 0
id = 1
c_sum_list = list(dyn_count_csv[1, 1])
for ( i in 1:ncol(dyn_count_csv) ){
	sum = sum + dyn_count_csv[1, i]
	c_sum_list[[id]] =  sum
	id = id + 1
}

interested_id = 0
for ( i in 1:length(c_sum_list) ){
	if ( comm.rank() <= c_sum_list[i] ) {
		dir_part = i -1
		interested_id = i
		break
	}
}

init.grid()
#dir_part = as.integer(comm.rank()/47)

if ( ( comm.rank()%%dyn_count_csv[1, interested_id] ) == 0 ) {
	file_part = 0
} else {
	file_part = comm.rank()%%dyn_count_csv[1, interested_id]
}

if (file_part < 10){
    fn <- paste0('gunzip -c model1.',dir_part,'/vector_model1.',dir_part,'.part00',file_part,'.gz')
}else if (file_part >=10 & file_part< 100){
    fn <- paste0('gunzip -c model1.',dir_part,'/vector_model1.',dir_part,'.part0',file_part,'.gz')
}

x <- fread(fn, header = F, sep = ';',colClasses=c("character",rep("numeric",data_dim)))

barrier()
nn = x[-1]
#DROP THE FIRST COLUMN OF TAGS
dx <- as.ddmatrix(x[,-1])
sm <- sum(dx)

end.time <- Sys.time()
time <- end.time - start.time
comm.print(time)

comm.print(sm)
nr = allreduce(nrow(x))
nc = allreduce(ncol(x))
comm.print(paste("the overall matrix row is:", nr));
comm.print(paste("the overall matrix col is:", nc));

finalize()
	

