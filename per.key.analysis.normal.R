library(MASS)
library(ggplot2)
library(ggfortify)
library(reshape2)
library(plyr)
library(data.table)

data <- read.csv('data.filter.csv',header=T)
key <- read.csv('key.filter.csv',header=T)
#gene <- as.data.frame(unique(subset(key,select=c("gene","type"))))

##############

method <- "normal"
workdir <- paste0("per.key.analysis.",method)
system(paste0("mkdir ",workdir))

## 
for(i in 1:dim(key)[1]){
  ds <- subset(data,guide1.key==key$key[i] & guide2.type=="negative")
  if(dim(ds)[1]>=2){
    set.seed(0)
    fit <- fitdistr(ds$observed, densfun=method)
    key$key_neg.mean[i] <- fit$estimate[1]
    key$key_neg.sd[i] <- fit$estimate[2]
  }
  #
  ds <- subset(data,guide1.type=="negative" & guide2.key==key$key[i])
  if(dim(ds)[1]>=2){
    set.seed(0)
    fit <- fitdistr(ds$observed, densfun=method)
    key$neg_key.mean[i] <- fit$estimate[1]
    key$neg_key.sd[i] <- fit$estimate[2]
  }
}
rm(i,ds,fit)

##
for(i in 1:dim(data)[1]){
  guide1_neg <- subset(key,key==data$guide1.key[i],select=c("key_neg.mean","key_neg.sd"))
  data$guide1_neg.mean[i] <- guide1_neg[[1]]
  data$guide1_neg.sd[i] <- guide1_neg[[2]]
  #
  neg_guide2 <- subset(key,key==data$guide2.key[i],select=c("neg_key.mean","neg_key.sd"))
  data$neg_guide2.mean[i] <- neg_guide2[[1]]
  data$neg_guide2.sd[i] <- neg_guide2[[2]]
  #
  data$expected.mean[i] <- as.numeric(data$guide1_neg.mean[i]+data$neg_guide2.mean[i])
  data$expected.sd[i] <- as.numeric(sqrt(data$guide1_neg.sd[i]^2+data$neg_guide2.sd[i]^2))
  #
  data$pvalue[i] <- ks.test(data$observed[i], "pnorm", mean=data$expected.mean[i], sd=data$expected.sd[i])$p.value
}
rm(i,guide1_neg,neg_guide2)
write.csv(data,paste0(workdir,"/data.csv"),row.names=F)
write.csv(key,paste0(workdir,"/key.csv"),row.names=F)

## 
data.sig <- subset(data,pvalue<0.01)
for(i in 1:dim(data.sig)[1]){
  p <- ggplot(data=data.frame(x=seq(-20, 10, 0.05)), aes(x=x)) + 
    stat_function(aes(colour="LINE1"), fun=dnorm, args=list(mean=data.sig$guide1_neg.mean[i],sd=data.sig$guide1_neg.sd[i])) +
    stat_function(aes(colour="LINE2"), fun=dnorm, args=list(mean=data.sig$neg_guide2.mean[i],sd=data.sig$neg_guide2.sd[i])) +
    stat_function(aes(colour="LINE3"), fun=dnorm, args=list(mean=data.sig$expected.mean[i],sd=data.sig$expected.sd[i])) +
    geom_vline(xintercept=data.sig$observed[i],col="red") +
    theme_bw(base_size=14) + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
    xlab("Observed (in red)") + ylab("Distribution") +
    labs(subtitle=paste0("pvalue = ",formatC(data.sig$pvalue[i],digits=2,format="e"))) +
    theme(legend.position="bottom") +
    scale_colour_manual("", values=c(LINE1="yellow",LINE2="lightblue",LINE3="black"),
                        labels = c("guide1_neg","neg_guide2","expected")) 
  pdf(paste0(workdir,"/plot.",data.sig$guide1.key[i],'_',data.sig$guide2.key[i],".pdf"),height=4,width=8)
  print(p)
  dev.off()
}
rm(i,p)

