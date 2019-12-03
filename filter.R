library(ggplot2)
library(reshape2)
library(plyr)
library(data.table)

## read data for columns of selected replicate
data <- read.csv('../summary.csv',header=T)
data$gene1 <- gsub("-.*","",data$name1)
data$gene2 <- gsub("-.*","",data$name2)
data <- subset(data,select=c("key1","key2","gene1","gene2","type1","type2","log2fc.rep12"))
colnames(data) <- c("guide1.key","guide2.key","guide1.gene","guide2.gene","guide1.type","guide2.type","observed")
data <- subset(data,!is.na(observed))

## read key
key <- read.csv('../key.csv',header=T)
key$gene <- gsub("-.*","",key$name)
key <- subset(key,select=c("key","gene","type"))

## remove keys that have <4 observed values for both neg_key and key_neg
tmp1 <- as.data.frame(table(data$guide1.key[data$guide2.type=="negative"]),stringsAsFactors=F)
tmp1 <- tmp1$Var1[tmp1$Freq>=4]
tmp2 <- as.data.frame(table(data$guide2.key[data$guide1.type=="negative"]),stringsAsFactors=F)
tmp2 <- tmp2$Var1[tmp2$Freq>=4]
tmp <- intersect(tmp1,tmp2)
key <- subset(key,key %in% tmp)
rm(tmp,tmp1,tmp2)

## remove data accordingly
data <- subset(data, (guide1.key %in% key$key) & (guide2.key %in% key$key) )

## set observed value threshold
data$observed[data$observed < -log2(100)] <- -log2(100)

## write 
write.csv(data,'data.filter.csv',row.names=F)
write.csv(key,'key.filter.csv',row.names=F)

