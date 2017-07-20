setwd('TEST_DATA')
tsumV2 = 0
tsumV3 = 0
for (x in dir()){ 
	filesumV2 <- sum(read.csv(x, sep = ';',header= FALSE)$V2)
	filesumV3 <- sum(read.csv(x, sep = ';',header= FALSE)$V3)
	tsumV2 = tsumV2 + filesumV2
	tsumV3 = tsumV3 + filesumV3
}

print (tsumV2)
print (tsumV3)
