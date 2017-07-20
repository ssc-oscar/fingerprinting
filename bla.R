dir_part = 0
first_flag = 1
file_part = 0

dyn_count_csv = read.csv("dynamic_count_of_files", header=FALSE)
dyn_count = split(dyn_count_csv, seq(nrow(dyn_count_csv)) )
print (dyn_count_csv[1, 2])
dyn_count = split(dyn_count_csv, seq(nrow(dyn_count_csv)) )
print (ncol(dyn_count_csv))

sum = 0
id = 1
c_sum_list = list(dyn_count_csv[1, 1])
for ( i in 1:ncol(dyn_count_csv) ) {
	sum = sum + dyn_count_csv[1, i]
	c_sum_list[[id]] =  sum
	id = id + 1
}
print (sum)
print (c_sum_list)

for ( i in 1:length(c_sum_list) ) {
	if ( 95 <= c_sum_list[i] ) {
		dir_part = i -1
		break
	}
}

print (dir_part)

#for ( comm_rank in 0:((47*2)+48-1) ){

#	if ( ( comm_rank%%47 ) == 0 ) {
#		file_part = 0
#	}

#	dir_part = as.integer(comm_rank/47)


#	print (paste ( dir_part, file_part ) )
#	file_part = file_part + 1

#}
