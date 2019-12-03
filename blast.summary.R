library(plyr)
library(data.table)

d <- read.csv("../raw_data/combinations.csv",header=T,stringsAsFactors=F)
d$seq <- NULL

files <- dir(pattern=".parse.csv")
for(i in files){
  tmp <- read.csv(i,stringsAsFactors=FALSE)
  tmp <- as.data.frame(table(tmp$hit))
  colnames(tmp) <- c("guide",gsub(".parse.csv","",i))
  d <- join(d,tmp)
  rm(tmp)
}
rm(i,files)
d[is.na(d)] <- 0

write.csv(d,'blast.summary.csv',row.names=F)

