library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
prefix <- args[1]
mismatches <- as.numeric(args[2])

d <- read.table(paste0("blast/",prefix,".txt"),header=F,sep='\t')
d <- d[,c(2,4,5,6)]
colnames(d) <- c("hit","align.length","mismatch","indel")

## filter hits that have more than 4nt unaligned ends or 1nt insertion
ideal.length <- median(d$align.length)
d <- subset(d, align.length>=(ideal.length-4) & align.length<=(ideal.length+1) )

## compute total.miss
d$unalign.length <- ideal.length - d$align.length
d$unalign.length[d$align.length>ideal.length] <- 0
#
d$total.miss <- d$mismatch + d$indel + d$unalign.length

## filter hits with more than required mismatches
d <- subset(d,total.miss <= mismatches)

## write to csv file
write.csv(d,paste0("blast/",prefix,".parse.csv"),row.names=F)

