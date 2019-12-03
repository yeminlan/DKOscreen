library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
prefix <- args[1]

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
d$total.miss[d$total.miss>=10] <- ">=10"
d$total.miss <- factor(d$total.miss,c(as.character(0:9),">=10"))

## write to csv file
write.csv(d,paste0("blast/",prefix,".csv"),row.names=F)

## plot histogram
ds <- as.data.frame(table(d$total.miss))
colnames(ds) <- c("total.miss","count")

pdf(paste0("blast/",prefix,".pdf"),height=6,width=6)
ggplot(ds,aes(total.miss,count,label=count)) +
  geom_bar(fill="black",col="black",stat="identity") +
  geom_text(vjust=-0.5,size=2) +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Reads") + xlab("Total Mismatches") +
  labs(title=paste0(prefix,": ",dim(d)[1]," aligned in total")) + 
  theme(plot.title = element_text(size=10))
dev.off()


