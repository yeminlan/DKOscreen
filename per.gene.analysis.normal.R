library(MASS)
library(ggplot2)
library(ggfortify)
library(reshape2)
library(plyr)
library(data.table)

data <- read.csv('data.filter.csv',header=T,stringsAsFactors=F)
key <- read.csv('key.filter.csv',header=T,stringsAsFactors=F)

## remove gene_pair that have less than 8 observed values (i.e. each gene has at least 2 guides)
for(i in 1:dim(data)[1]){
  data$gene_pair[i] <- paste(sort(c(data$guide1.gene[i],data$guide2.gene[i])),collapse="-")
}
tmp <- as.data.frame(table(data$gene_pair))
data <- data[!(data$gene_pair %in% tmp$Var1[tmp$Freq<8]),]
data$gene_pair <- NULL
rm(tmp,i)

## remove key accordingly
key <- subset(key,key %in% c(data$guide1.key,data$guide2.key))

## collapse key to gene
gene <- as.data.frame(unique(subset(key,select=c("gene","type"))))
rm(key)

## ignore position info in data
data$guide1.key <- NULL
data$guide2.key <- NULL
for(i in 1:dim(data)[1]){
  if( order(c(data$guide1.gene[i],data$guide2.gene[i]))[1]==2 ){
    tmp <- data$guide1.gene[i]
    data$guide1.gene[i] <- data$guide2.gene[i]
    data$guide2.gene[i] <- tmp
    tmp <- data$guide1.type[i]
    data$guide1.type[i] <- data$guide2.type[i]
    data$guide2.type[i] <- tmp
  }
}
rm(tmp,i)

##############

method <- "normal"
workdir <- paste0("per.gene.analysis.",method)
system(paste0("mkdir ",workdir))

## 
for(i in 1:dim(gene)[1]){
  ds <- subset(data,(guide1.gene==gene$gene[i] & guide2.type=="negative")|(guide1.type=="negative" & guide2.gene==gene$gene[i]))
  if(dim(ds)[1]>=2){
    set.seed(0)
    fit <- fitdistr(ds$observed, densfun=method)
    gene$gene.mean[i] <- fit$estimate[1]
    gene$gene.sd[i] <- fit$estimate[2]
  }
}
rm(i,ds,fit)

## collapse data by gene
## text version of "observed"
data.bak <- aggregate(observed~., data, paste, collapse=",")
## numeric version of "observed"
data <- aggregate(observed~., data, cbind)

##
for(i in 1:dim(data)[1]){
  guide1 <- subset(gene,gene==data$guide1.gene[i],select=c("gene.mean","gene.sd"))
  data$guide1.mean[i] <- guide1[[1]]
  data$guide1.sd[i] <- guide1[[2]]
  #
  guide2 <- subset(gene,gene==data$guide2.gene[i],select=c("gene.mean","gene.sd"))
  data$guide2.mean[i] <- guide2[[1]]
  data$guide2.sd[i] <- guide2[[2]]
  #
  data$expected.mean[i] <- as.numeric(data$guide1.mean[i]+data$guide2.mean[i])
  data$expected.sd[i] <- as.numeric(sqrt(data$guide1.sd[i]^2+data$guide2.sd[i]^2))
  #
  data$observed.mean[i] <- mean(data$observed[[i]])
  data$observed.sd[i] <- sd(data$observed[[i]])
  #
  data$pvalue[i] <- ks.test(data$observed[[i]], "pnorm", mean=data$expected.mean[i], sd=data$expected.sd[i])$p.value
}
rm(i,guide1,guide2)
#
data$observed <- data.bak$observed
rm(data.bak)
#
data$qvalue <- p.adjust(data$pvalue,method="fdr")
#
write.csv(data,paste0(workdir,"/data.csv"),row.names=F)
write.csv(gene,paste0(workdir,"/gene.csv"),row.names=F)

## 
data.sig <- subset(data,qvalue<0.05)
for(i in 1:dim(data.sig)[1]){
  p <- ggplot(data=data.frame(x=seq(-20, 10, 0.01)), aes(x=x)) + 
    stat_function(aes(colour="LINE1"), fun=dnorm, args=list(mean=data.sig$guide1.mean[i],sd=data.sig$guide1.sd[i])) +
    stat_function(aes(colour="LINE2"), fun=dnorm, args=list(mean=data.sig$guide2.mean[i],sd=data.sig$guide2.sd[i])) +
    stat_function(aes(colour="LINE3"), fun=dnorm, args=list(mean=data.sig$expected.mean[i],sd=data.sig$expected.sd[i])) +
    stat_function(aes(colour="LINE4"), fun=dnorm, args=list(mean=data.sig$observed.mean[i],sd=data.sig$observed.sd[i])) +
    theme_bw(base_size=14) + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
    xlab("Values") + ylab("Distribution") +
    labs(subtitle=paste0("pvalue = ",formatC(data.sig$pvalue[i],digits=2,format="e"))) +
    theme(legend.position="bottom") +
    scale_colour_manual("", values=c(LINE1="yellow",LINE2="lightblue",LINE3="black",LINE4="red"),
                        labels = c("guide1","guide2","expected","observed")) 
  pdf(paste0(workdir,"/plot.",data.sig$guide1.gene[i],'_',data.sig$guide2.gene[i],".pdf"),height=4,width=8)
  print(p)
  dev.off()
}
rm(i,p)

