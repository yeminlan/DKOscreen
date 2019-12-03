library(plyr)

d <- read.csv('raw_data/key.csv',header=T,stringsAsFactors=F)
DR <- "AATTTCTACTCTTGTAGAT"

combinations <- as.data.frame(expand.grid(d$key,d$key))
colnames(combinations) <- c("key1","key2")

ds <- d
colnames(ds) <- paste0(colnames(d),"1")
combinations <- join(combinations,ds)
colnames(ds) <- paste0(colnames(d),"2")
combinations <- join(combinations,ds)

combinations$seq <- paste0(combinations$sequence1,DR,combinations$sequence2)
combinations$sequence1 <- NULL
combinations$sequence2 <- NULL
combinations$guide <- paste0(combinations$key1,'_',combinations$key2)

write.csv(combinations,'raw_data/combinations.csv',row.names=F)

for(i in 1:dim(combinations)[1]){
  write(paste0(">",combinations$guide[i]),file="raw_data/combinations.fa",append=TRUE)
  write(combinations$seq[i],file="raw_data/combinations.fa",append=TRUE)
}
