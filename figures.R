library(MASS)
library(ggplot2)
library(ggfortify)
library(reshape2)
library(plyr)
library(data.table)
library(ggrepel)

###### fig1.positional.difference.pdf ######
pdf('fig1.positional.difference.pdf',height=4,width=8)
## 
key <- read.csv('per.key.analysis.normal/key.csv')
#
key$positional.diff <- key$neg_key.mean-key$key_neg.mean
#
ggplot(key,aes(positional.diff)) + 
  geom_histogram(breaks=seq(-2.6,2.6,by=0.1),fill="white",col="black") +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Pairs") + xlab("Positional Difference (Value[neg_key] - Value[key_neg])") +
  coord_cartesian(ylim=c(0,15)) +
  labs(subtitle="All guides")
ggplot(subset(key,type %in% c("experimental","positive")),aes(positional.diff)) + 
  geom_histogram(breaks=seq(-2.6,2.6,by=0.1),fill="white",col="black") +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Pairs") + xlab("Positional Difference (Value[neg_key] - Value[key_neg])") +
  coord_cartesian(ylim=c(0,12)) +
  labs(subtitle="All experimental+positive guides")
ggplot(subset(key,type %in% "experimental"),aes(positional.diff)) + 
  geom_histogram(breaks=seq(-2.6,2.6,by=0.1),fill="white",col="black") +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Pairs") + xlab("Positional Difference (Value[neg_key] - Value[key_neg])") +
  coord_cartesian(ylim=c(0,12)) +
  labs(subtitle="All experimental guides")
ggplot(subset(key,type %in% "positive"),aes(positional.diff)) + 
  geom_histogram(breaks=seq(-2.6,2.6,by=0.1),fill="white",col="black") +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Pairs") + xlab("Positional Difference (Value[neg_key] - Value[key_neg])") +
  coord_cartesian(ylim=c(0,12)) +
  labs(subtitle="All positive guides")
dev.off()
rm(key)

###### fig1b.positional.difference.accumulative.pdf ######
pdf('fig1b.positional.difference.accumulative.pdf',height=6,width=6)
## 
key <- read.csv('per.key.analysis.normal/key.csv')
#
key$positional.diff <- key$neg_key.mean-key$key_neg.mean
#
tmp <- subset(key,type %in% c("experimental","positive"))
tmp <- tmp[order(tmp$positional.diff),]
tmp$acuumulative.perc <- (dim(tmp)[1]:1)/dim(tmp)[1]*100
thres <- 100*(mean(tmp$positional.diff>0))
ggplot(tmp,aes(acuumulative.perc,positional.diff)) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=thres) +
  geom_line(size=1,col="red") +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Positional Difference (Value[neg_key] - Value[key_neg])") + xlab("Accumulative % of Guides") +
  labs(subtitle="All experimental+positive guides")
tmp <- subset(key,type %in% "experimental")
tmp <- tmp[order(tmp$positional.diff),]
tmp$acuumulative.perc <- (dim(tmp)[1]:1)/dim(tmp)[1]*100
thres <- 100*(mean(tmp$positional.diff>0))
ggplot(tmp,aes(acuumulative.perc,positional.diff)) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=thres) +
  geom_line(size=1,col="red") +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Positional Difference (Value[neg_key] - Value[key_neg])") + xlab("Accumulative % of Guides") +
  labs(subtitle="All experimental guides")
dev.off()
rm(key)

###### fig1c.positional.difference.scatterplot.pdf ######
pdf('fig1c.positional.difference.scatterplot.pdf',height=6,width=8)
## 
key <- read.csv('per.key.analysis.normal/key.csv')
key$label <- paste(key$gene,key$key,sep='_')
#
tmp <- subset(key,type %in% c("experimental","positive"))
ggplot(tmp,aes(key_neg.mean,neg_key.mean,col=type)) + 
  geom_abline(slope=1,intercept=0) +
  geom_point(size=1) +
  theme_bw(base_size=14) +
  scale_color_manual(values=c("red","blue")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("key_neg") + xlab("neg_key") +
  labs(subtitle="All experimental+positive guides") +
  coord_cartesian(xlim=c(-6,2.5),ylim=c(-6,2.5))
ggplot(tmp,aes(key_neg.mean,neg_key.mean,col=type)) + 
  geom_abline(slope=1,intercept=0) +
  geom_point(size=1) +
  theme_bw(base_size=14) +
  scale_color_manual(values=c("red","blue")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("key_neg") + xlab("neg_key") +
  labs(subtitle="All experimental+positive guides") +
  geom_text_repel(aes(label=label),col="black",size=1) +
  coord_cartesian(xlim=c(-6,2.5),ylim=c(-6,2.5))
tmp <- subset(key,type %in% "experimental")
ggplot(tmp,aes(key_neg.mean,neg_key.mean,col=type)) + 
  geom_abline(slope=1,intercept=0) +
  geom_point(size=1) +
  theme_bw(base_size=14) +
  scale_color_manual(values=c("red")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("key_neg") + xlab("neg_key") +
  labs(subtitle="All experimental guides") +
  coord_cartesian(xlim=c(-6,2.5),ylim=c(-6,2.5))
ggplot(tmp,aes(key_neg.mean,neg_key.mean,col=type)) + 
  geom_abline(slope=1,intercept=0) +
  geom_point(size=1) +
  theme_bw(base_size=14) +
  scale_color_manual(values=c("red")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("key_neg") + xlab("neg_key") +
  labs(subtitle="All experimental guides") +
  geom_text_repel(aes(label=label),col="black",size=1) +
  coord_cartesian(xlim=c(-6,2.5),ylim=c(-6,2.5))
dev.off()
rm(tmp,key)

###### fig1d.positional.difference.accumulative.pdf ######
pdf('fig1d.positional.difference.accumulative.pdf',height=6,width=6)
## 
key <- read.csv('per.key.analysis.normal/data.csv')
d1 <- subset(key, guide1.type %in% c("experimental","positive") & guide2.type=="negative", select=c("guide1.key","guide2.key","guide1.gene","observed"))
colnames(d1) <- c("key","neg","gene","key_neg")
d2 <- subset(key, guide2.type %in% c("experimental","positive") & guide1.type=="negative", select=c("guide2.key","guide1.key","observed"))
colnames(d2) <- c("key","neg","neg_key")
key <- join(d1,d2,type="inner")
rm(d1,d2)
key$label <- paste0(key$gene,"_",key$key)
#
key$positional.diff <- key$neg_key-key$key_neg
#
tmp <- key
tmp <- tmp[order(tmp$positional.diff),]
tmp$acuumulative.perc <- (dim(tmp)[1]:1)/dim(tmp)[1]*100
thres <- 100*(mean(tmp$positional.diff>0))
ggplot(tmp,aes(acuumulative.perc,positional.diff)) + 
  geom_hline(yintercept=0) +
  geom_vline(xintercept=thres) +
  geom_line(size=1,col="red") +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Positional Difference (Value[neg_key] - Value[key_neg])") + xlab("Accumulative % of Guides") +
  labs(subtitle="All experimental+positive guides")
dev.off()
rm(key,tmp,thres)

###### fig1e.positional.difference.scatterplot.pdf ######
pdf('fig1e.positional.difference.scatterplot.pdf',height=8,width=8)
## 
key <- read.csv('per.key.analysis.normal/data.csv')
d1 <- subset(key, guide1.type %in% c("experimental","positive") & guide2.type=="negative", select=c("guide1.key","guide2.key","guide1.gene","observed"))
colnames(d1) <- c("key","neg","gene","key_neg")
d2 <- subset(key, guide2.type %in% c("experimental","positive") & guide1.type=="negative", select=c("guide2.key","guide1.key","observed"))
colnames(d2) <- c("key","neg","neg_key")
key <- join(d1,d2,type="inner")
rm(d1,d2)
key$label <- paste0(key$gene,"_",key$key)
#
tmp <- key
s <- paste0("Pearson cor=",format(cor(tmp$key_neg,tmp$neg_key),digits=4),', p-val<2.2e-16')
ggplot(tmp,aes(key_neg,neg_key)) + 
  geom_point(size=1,col="grey") +
  geom_abline(slope=1,intercept=0,col="red") +
  theme_bw(base_size=14) +
  scale_color_manual(values=c("red","blue")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("key_neg") + xlab("neg_key") +
  labs(subtitle=paste0("All experimental+positive guides (",s,")")) +
  #geom_text_repel(aes(label=label),col="black",size=1) +
  coord_cartesian(xlim=c(-7.5,5),ylim=c(-7.5,5))
ggplot(tmp,aes(key_neg,neg_key)) + 
  geom_point(size=1,col="grey") +
  geom_abline(slope=1,intercept=0,col="red") +
  theme_bw(base_size=14) +
  scale_color_manual(values=c("red","blue")) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("key_neg") + xlab("neg_key") +
  labs(subtitle=paste0("All experimental+positive guides (",s,")")) +
  geom_text_repel(data=subset(tmp,abs(key_neg-neg_key)>3),aes(label=label),col="black",size=1) +
  coord_cartesian(xlim=c(-7.5,5),ylim=c(-7.5,5))
dev.off()
rm(tmp,key)

###### fig1f.positional.difference.scatterplot.pdf ######
pdf('fig1f.positional.difference.scatterplot.pdf',height=8,width=8)
## 
key <- read.csv('per.key.analysis.normal/data.csv')
d1 <- subset(key, guide1.type %in% c("experimental","positive") & guide2.type=="negative", select=c("guide1.key","guide2.key","guide1.gene","observed"))
colnames(d1) <- c("key","neg","gene","key_neg")
d2 <- subset(key, guide2.type %in% c("experimental","positive") & guide1.type=="negative", select=c("guide2.key","guide1.key","observed"))
colnames(d2) <- c("key","neg","neg_key")
key <- join(d1,d2,type="inner")
rm(d1,d2)
key$label <- paste0(key$gene,"_",key$key)
#
tmp <- key
for(i in unique(tmp$neg)){
  s <- paste0("negative guide ",i)
  p <- ggplot(tmp,aes(key_neg,neg_key,col=(tmp$neg==i))) + 
    geom_point(size=1) +
    geom_abline(slope=1,intercept=0,col="red") +
    theme_bw(base_size=14) +
    scale_color_manual(values=c("grey","red")) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
    ylab("key_neg") + xlab("neg_key") +
    labs(subtitle=paste0("All experimental+positive guides (",s,")")) +
    theme(legend.position="none") +
    #geom_text_repel(aes(label=label),col="black",size=1) +
    coord_cartesian(xlim=c(-7.5,5),ylim=c(-7.5,5))
  print(p)
}
dev.off()
rm(tmp,key,i,p,s)

###### fig2.observed.expected.difference.with_positive_guides.v1.pdf ######
## read data
data <- read.csv('per.gene.analysis.normal/data.csv',header=T)
## prepare label and diff
data$label <- paste0(data$guide1.gene,"_",data$guide2.gene)
data$diff <- data$expected.mean-data$observed.mean
## prepare color
data$color <- 'others'
data$color[data$qvalue<0.05 & data$diff>2.5] <- "qval<0.05 & \nlog2_diff>2.5"
data$color[data$guide1.type=="negative" & data$guide2.type=="negative"] <- "neg_neg"
data$color <- factor(data$color,levels=c("others","neg_neg","qval<0.05 & \nlog2_diff>2.5"))
## compute pearson corr and save into string 's'
c <- cor.test(data$observed.mean,data$expected.mean,method="pearson")
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
pdf('fig2.observed.expected.difference.with_positive_guides.v1.pdf',height=8,width=10)
ggplot(data,aes(expected.mean,observed.mean,fill=color)) + 
  geom_point(size=4,shape=21,col="black",alpha=1) +
  scale_fill_manual(values=c("grey","grey","red")) +
  #geom_point(data=data[data$color=="others",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="neg_neg",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="qval<0.05",],size=4,shape=21,col="black",fill="red",alpha=1) +
  geom_smooth(method="lm", formula=y~x, col="black", linetype="dashed", fill=NA, alpha=0.1, size=0.5) + #fitting line
  #geom_abline(intercept=0,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_abline(intercept=-2,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_vline(xintercept=0,col="grey80") +
  #geom_hline(yintercept=0,col="grey80") +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-11,5),ylim=c(-11,5)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  #geom_text_repel(data=subset(data,(diff>2.5)&(qvalue<0.05)),aes(label=label),show.legend=F,size=3,col="black") +  #label selected datapoints
  #theme(legend.position="none") +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
dev.off()
rm(s,data)

###### fig2.observed.expected.difference.with_positive_guides.v2.pdf ######
## read data
data <- read.csv('per.gene.analysis.normal/data.csv',header=T)
## prepare label and diff
data$label <- paste0(data$guide1.gene,"_",data$guide2.gene)
data$diff <- data$expected.mean-data$observed.mean
## prepare color
data$color <- 'others'
data$color[data$qvalue<0.05 & data$diff>2.5] <- "qval<0.05 & \nlog2_diff>2.5"
data$color[data$guide1.type=="negative" & data$guide2.type=="negative"] <- "neg_neg"
data$color <- factor(data$color,levels=c("others","neg_neg","qval<0.05 & \nlog2_diff>2.5"))
## compute pearson corr and save into string 's'
c <- cor.test(data$observed.mean,data$expected.mean,method="pearson")
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
pdf('fig2.observed.expected.difference.with_positive_guides.v2.pdf',height=8,width=10)
ggplot(data,aes(expected.mean,observed.mean,fill=color)) + 
  geom_point(size=4,shape=21,col="black",alpha=1) +
  scale_fill_manual(values=c("grey","grey","red")) +
  #geom_point(data=data[data$color=="others",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="neg_neg",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="qval<0.05",],size=4,shape=21,col="black",fill="red",alpha=1) +
  geom_smooth(method="lm", formula=y~x, col="black", linetype="dashed", fill=NA, alpha=0.1, size=0.5) + #fitting line
  #geom_abline(intercept=0,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_abline(intercept=-2,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_vline(xintercept=0,col="grey80") +
  #geom_hline(yintercept=0,col="grey80") +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-11,5),ylim=c(-11,5)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_text_repel(data=subset(data,(diff>2.5)&(qvalue<0.05)),aes(label=label),show.legend=F,size=3,col="black") +  #label selected datapoints
  #theme(legend.position="none") +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
dev.off()
rm(s,data)

###### fig2.observed.expected.difference.without_positive_guides.v1.pdf ######
## read data
data <- read.csv('per.gene.analysis.normal/data.csv',header=T)
## remove all pairs with a positive guide
data <- subset(data, guide1.type!="positive" & guide2.type!="positive" )
## prepare label and diff
data$label <- paste0(data$guide1.gene,"_",data$guide2.gene)
data$diff <- data$expected.mean-data$observed.mean
## prepare color
data$color <- 'others'
data$color[data$qvalue<0.05 & data$diff>2.5] <- "qval<0.05 & \nlog2_diff>2.5"
data$color[data$guide1.type=="negative" & data$guide2.type=="negative"] <- "neg_neg"
data$color <- factor(data$color,levels=c("others","neg_neg","qval<0.05 & \nlog2_diff>2.5"))
## compute pearson corr and save into string 's'
c <- cor.test(data$observed.mean,data$expected.mean,method="pearson")
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
#pdf('fig2.observed.expected.difference.without_positive_guides.v1.pdf',height=8,width=10)
setEPS()
postscript('fig2.observed.expected.difference.without_positive_guides.v1.eps',height=8,width=10)
ggplot(data,aes(expected.mean,observed.mean,fill=color)) + 
  geom_point(size=4,shape=21,col="black",alpha=1) +
  scale_fill_manual(values=c("grey","#828DFC","red")) +
  #geom_point(data=data[data$color=="others",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="neg_neg",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="qval<0.05",],size=4,shape=21,col="black",fill="red",alpha=1) +
  geom_smooth(method="lm", formula=y~x, col="black", linetype="dashed", fill=NA, alpha=0.1, size=0.5) + #fitting line
  #geom_abline(intercept=0,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_abline(intercept=-2,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_vline(xintercept=0,col="grey80") +
  #geom_hline(yintercept=0,col="grey80") +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-8,5),ylim=c(-8,5)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  #geom_text_repel(data=subset(data,(diff>2.5)&(qvalue<0.05)),aes(label=label),show.legend=F,size=3,col="black") +  #label selected datapoints
  #theme(legend.position="none") +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
dev.off()
rm(s,data)

###### fig2.observed.expected.difference.without_positive_guides.v2.pdf ######
## read data
data <- read.csv('per.gene.analysis.normal/data.csv',header=T)
## remove all pairs with a positive guide
data <- subset(data, guide1.type!="positive" & guide2.type!="positive" )
## prepare label and diff
data$label <- paste0(data$guide1.gene,"_",data$guide2.gene)
data$diff <- data$expected.mean-data$observed.mean
## prepare color
data$color <- 'others'
data$color[data$qvalue<0.05 & data$diff>2.5] <- "qval<0.05 & \nlog2_diff>2.5"
data$color[data$guide1.type=="negative" & data$guide2.type=="negative"] <- "neg_neg"
data$color <- factor(data$color,levels=c("others","neg_neg","qval<0.05 & \nlog2_diff>2.5"))
## compute pearson corr and save into string 's'
c <- cor.test(data$observed.mean,data$expected.mean,method="pearson")
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
pdf('fig2.observed.expected.difference.without_positive_guides.v2.pdf',height=8,width=10)
ggplot(data,aes(expected.mean,observed.mean,fill=color)) + 
  geom_point(size=4,shape=21,col="black",alpha=1) +
  scale_fill_manual(values=c("grey","grey","red")) +
  #geom_point(data=data[data$color=="others",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="neg_neg",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="qval<0.05",],size=4,shape=21,col="black",fill="red",alpha=1) +
  geom_smooth(method="lm", formula=y~x, col="black", linetype="dashed", fill=NA, alpha=0.1, size=0.5) + #fitting line
  #geom_abline(intercept=0,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_abline(intercept=-2,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_vline(xintercept=0,col="grey80") +
  #geom_hline(yintercept=0,col="grey80") +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-8,5),ylim=c(-8,5)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_text_repel(data=subset(data,(diff>2.5)&(qvalue<0.05)),aes(label=label),show.legend=F,size=3,col="black") +  #label selected datapoints
  #theme(legend.position="none") +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
dev.off()
rm(s,data)

###### fig2.observed.expected.difference.without_positive_guides.v3.pdf ######
## read data
data <- read.csv('per.gene.analysis.normal/data.csv',header=T)
## remove all pairs with a positive guide
data <- subset(data, guide1.type!="positive" & guide2.type!="positive" )
## prepare label and diff
data$label <- paste0(data$guide1.gene,"_",data$guide2.gene)
data$diff <- data$expected.mean-data$observed.mean
## prepare color
data$color <- 'others'
data$color[data$qvalue<0.05 & data$diff>2.5] <- "qval<0.05 & \nlog2_diff>2.5"
data$color[data$qvalue<0.05 & data$diff< -2.5] <- "qval<0.05 & \nlog2_diff<-2.5"
data$color[data$guide1.type=="negative" & data$guide2.type=="negative"] <- "neg_neg"
data$color <- factor(data$color,levels=c("others","neg_neg","qval<0.05 & \nlog2_diff>2.5","qval<0.05 & \nlog2_diff<-2.5"))
## compute pearson corr and save into string 's'
c <- cor.test(data$observed.mean,data$expected.mean,method="pearson")
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
pdf('fig2.observed.expected.difference.without_positive_guides.v3.pdf',height=8,width=10)
ggplot(data,aes(expected.mean,observed.mean,fill=color)) + 
  geom_point(size=4,shape=21,col="black",alpha=1) +
  scale_fill_manual(values=c("grey","grey","red","blue")) +
  #geom_point(data=data[data$color=="others",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="neg_neg",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="qval<0.01",],size=4,shape=21,col="black",fill="red",alpha=1) +
  geom_smooth(method="lm", formula=y~x, col="black", linetype="dashed", fill=NA, alpha=0.1, size=0.5) + #fitting line
  #geom_abline(intercept=0,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_abline(intercept=-2,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_vline(xintercept=0,col="grey80") +
  #geom_hline(yintercept=0,col="grey80") +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-8,5),ylim=c(-8,5)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_text_repel(data=subset(data,(abs(diff)>2.5)&(qvalue<0.05)),aes(label=label),show.legend=F,size=3,col="black") +  #label selected datapoints
  #theme(legend.position="none") +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
dev.off()
rm(s,data)

###### fig2.observed.expected.difference.without_positive_guides.v4.pdf ######
## read data
data <- read.csv('per.gene.analysis.normal/data.csv',header=T)
## remove all pairs with a positive guide
data <- subset(data, guide1.type!="positive" & guide2.type!="positive" )
## prepare label and diff
data$label <- paste0(data$guide1.gene,"_",data$guide2.gene)
data$diff <- data$expected.mean-data$observed.mean
## prepare color
data$color <- 'others'
data$color[data$qvalue<0.05 & data$diff>2.5] <- "qval<0.05 & \nlog2_diff>2.5"
data$color[data$guide1.type=="negative" & data$guide2.type=="negative"] <- "neg_neg"
data$color <- factor(data$color,levels=c("others","neg_neg","qval<0.05 & \nlog2_diff>2.5"))
## compute pearson corr and save into string 's'
c <- cor.test(data$observed.mean,data$expected.mean,method="pearson")
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
pdf('fig2.observed.expected.difference.without_positive_guides.v4.pdf',height=8,width=10)
ggplot(data,aes(expected.mean,observed.mean,fill=color)) + 
  geom_point(size=4,shape=21,col="black",alpha=1) +
  scale_fill_manual(values=c("grey","grey","red")) +
  #geom_point(data=data[data$color=="others",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="neg_neg",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="qval<0.05",],size=4,shape=21,col="black",fill="red",alpha=1) +
  geom_smooth(method="lm", formula=y~x, col="black", linetype="dashed", fill=NA, alpha=0.1, size=0.5) + #fitting line
  geom_abline(intercept=-2.5,slope=1,col="black",linetype="dashed",size=0.5) +
  #geom_abline(intercept=-2,slope=1,col="black",linetype="dotted",size=0.5) +
  #geom_vline(xintercept=0,col="grey80") +
  #geom_hline(yintercept=0,col="grey80") +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-8,5),ylim=c(-8,5)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  #geom_text_repel(data=subset(data,(diff>2.5)&(qvalue<0.05)),aes(label=label),show.legend=F,size=3,col="black") +  #label selected datapoints
  #theme(legend.position="none") +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
dev.off()
rm(s,data)

###### fig2.observed.expected.difference.without_positive_guides.v5.pdf ######
## read data
data <- read.csv('per.gene.analysis.normal/data.csv',header=T)
## remove all pairs with a positive guide
data <- subset(data, guide1.type!="positive" & guide2.type!="positive" )
## prepare label and diff
data$label <- paste0(data$guide1.gene,"_",data$guide2.gene)
data$diff <- data$expected.mean-data$observed.mean
## prepare color
data$color <- 'others'
data$color[data$qvalue<0.05 & data$diff>2.5] <- "qval<0.05 & \nlog2_diff>2.5"
data$color[data$guide1.type=="negative" & data$guide2.type=="negative"] <- "neg_neg"
data$color <- factor(data$color,levels=c("others","neg_neg","qval<0.05 & \nlog2_diff>2.5"))
## compute pearson corr and save into string 's'
c <- cor.test(data$observed.mean,data$expected.mean,method="pearson")
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
pdf('fig2.observed.expected.difference.without_positive_guides.v5.pdf',height=8,width=10)
ggplot(data,aes(expected.mean,observed.mean,fill=color)) + 
  geom_point(size=4,shape=21,col="black",alpha=1) +
  scale_fill_manual(values=c("grey","grey","red")) +
  #geom_point(data=data[data$color=="others",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="neg_neg",],size=4,shape=21,col="black",fill="grey",alpha=0.5) +
  #geom_point(data=data[data$color=="qval<0.05",],size=4,shape=21,col="black",fill="red",alpha=1) +
  #geom_smooth(method="lm", formula=y~x, col="black", linetype="dashed", fill=NA, alpha=0.1, size=0.5) + #fitting line
  geom_abline(intercept=-2.5,slope=1,col="black",linetype="dashed",size=0.5) +
  geom_abline(intercept=0,slope=1,col="black",linetype="dashed",size=0.5) +
  #geom_vline(xintercept=0,col="grey80") +
  #geom_hline(yintercept=0,col="grey80") +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-8,5),ylim=c(-8,5)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  #geom_text_repel(data=subset(data,(diff>2.5)&(qvalue<0.05)),aes(label=label),show.legend=F,size=3,col="black") +  #label selected datapoints
  #theme(legend.position="none") +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
dev.off()
rm(s,data)

###### fig3.observed.expected.linear_regression.pdf ######
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
## remove all pairs with a positive guide
data <- subset(data, guide1.type!="positive" & guide2.type!="positive" )
## compute lm and residual
data.lm = lm(observed~expected.mean, data=data) 
data$lmresidual = rstandard(data.lm)
## prepare color
data$color <- 'others'
data$color[data$lmresidual < (mean(data$lmresidual)-2*sd(data$lmresidual)) ] <- "buffer"
data$color[data$lmresidual > (mean(data$lmresidual)+2*sd(data$lmresidual)) ] <- "synergy"
## compute pearson corr and save into string 's'
c <- cor.test(data$observed,data$expected.mean,method="pearson")
if(c$p.value==0){c$p.value <- 2.2e-16}
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
pdf('fig3.observed.expected.linear_regression.pdf',height=8,width=10)
ggplot(data,aes(expected.mean,observed,col=color)) + 
  geom_point(size=2,alpha=0.5) +
  scale_color_manual(values=c("blue","grey","red")) +
  geom_abline(intercept=data.lm$coefficients[1],slope=data.lm$coefficients[2],col="black",linetype="dashed",size=0.5) +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-9,6),ylim=c(-9,6)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
dev.off()
rm(s,data,data.lm)

###### fig3.observed.expected.linear_regression.v2.pdf ######
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
## remove all pairs with a positive guide
data <- subset(data, guide1.type!="positive" & guide2.type!="positive" )
## compute lm and residual
data.lm = lm(observed~expected.mean, data=data) 
data$lmresidual = rstandard(data.lm)
## prepare color
data$color <- 'others'
data$color[data$lmresidual < (mean(data$lmresidual)-2*sd(data$lmresidual)) ] <- "buffer"
data$color[data$lmresidual > (mean(data$lmresidual)+2*sd(data$lmresidual)) ] <- "synergy"
## compute pearson corr and save into string 's'
c <- cor.test(data$observed,data$expected.mean,method="pearson")
if(c$p.value==0){c$p.value <- 2.2e-16}
s <- formatC(c$p.value, digits=1, format = "e")
s <- paste0("Pearson r = ",format(round(c$estimate,2),nsmall=2),", p < ",s)
rm(c)
## plot
pdf('fig3.observed.expected.linear_regression.v2.pdf',height=8,width=10)
ggplot(data,aes(expected.mean,observed,col=color)) + 
  geom_point(size=2,alpha=0.5) +
  scale_color_manual(values=c("blue","grey","red")) +
  geom_abline(intercept=data.lm$coefficients[1],slope=data.lm$coefficients[2],col="black",linetype="dashed",size=0.5) +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-9,6),ylim=c(-9,6)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Observed") + xlab("Expected") +
  labs(subtitle=s)
lines <- c((mean(data$lmresidual)-2*sd(data$lmresidual)),
           mean(data$lmresidual),
           (mean(data$lmresidual)+2*sd(data$lmresidual)))
ggplot(data,aes(expected.mean,lmresidual,col=color)) + 
  geom_point(size=2,alpha=0.5) +
  scale_color_manual(values=c("blue","grey","red")) +
  geom_hline(yintercept=lines,col="black",linetype="dashed",size=0.5) +
  theme_bw(base_size=14) +
  coord_cartesian(xlim=c(-9,6),ylim=c(-6,6)) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  ylab("Residual") + xlab("Expected") +
  labs(subtitle="")
dev.off()
rm(s,data,data.lm,lines)

###### fig4.Brd9_Jmjd6.jitterplot.pdf ######
#
guide1 <- 'Brd9'
guide2 <- 'Jmjd6'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
d2 <- data.frame(value=c(mean(d$value[d$group=="guide1"]),mean(d$value[d$group=="guide2"]),
                         mean(d$value[d$group=="expected.mean"]),mean(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Brd9_Jmjd6.jitterplot.pdf',height=8,width=6)
ggplot(d,aes(group,value,col=group)) + 
  geom_jitter(alpha=0.5,width=0.2,shape=20,size=3) +
  scale_color_manual(values=c("grey50","black","blue","red")) +
  geom_crossbar(data=d2, aes(ymin=value, ymax=value),size=0.5,col="black",width=0.5) +
  theme_bw(base_size=14) +
  #geom_hline(yintercept=0,col="black") +
  geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(subtitle=paste0(guide1,'-',guide2))
dev.off()
#
rm(guide1,guide2,d,d2)

###### fig4.Brd9_Jmjd6.boxplot.pdf ######
#
guide1 <- 'Brd9'
guide2 <- 'Jmjd6'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
d2 <- data.frame(value=c(mean(d$value[d$group=="guide1"]),mean(d$value[d$group=="guide2"]),
                         mean(d$value[d$group=="expected.mean"]),mean(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Brd9_Jmjd6.boxplot.pdf',height=8,width=6)
ggplot(d,aes(group,value,col=group)) + 
  geom_boxplot(outlier.shape=NA,width=0.5) +
  scale_color_manual(values=c("grey50","black","blue","red")) +
  theme_bw(base_size=14) +
  #geom_hline(yintercept=0,col="black") +
  geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(subtitle=paste0(guide1,'-',guide2))
dev.off()
#
rm(guide1,guide2,d,d2)

###### fig4.Brd9_Jmjd6.barplot.pdf ######
#
guide1 <- 'Brd9'
guide2 <- 'Jmjd6'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
#
pval <- format(c(wilcox.test(d4$value,d1$value)$p.value,
                 wilcox.test(d4$value,d2$value)$p.value,
                 wilcox.test(d4$value,d3$value)$p.value),scientific=T,digits=3)
subtitle <- paste0('p-values for guild1/guide2/expected are ',pval[1],'/',pval[2],'/',pval[3])
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
se <- function(x) sqrt(var(x)/length(x))
d2 <- data.frame(value=c(median(d$value[d$group=="guide1"]),median(d$value[d$group=="guide2"]),
                         median(d$value[d$group=="expected.mean"]),median(d$value[d$group=="observed"])),
                 se=c(se(d$value[d$group=="guide1"]),se(d$value[d$group=="guide2"]),
                      se(d$value[d$group=="expected.mean"]),se(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
d2$lower <- d2$value-d2$se
d2$upper <- d2$value+d2$se
d2$group <- factor(d2$group,c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Brd9_Jmjd6.barplot.pdf',height=8,width=6)
ggplot(d2,aes(group,value,fill=group)) + 
  geom_bar(stat='identity',width=0.5) +
  geom_point(col="grey20") +
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.3,col="grey20") +
  scale_fill_manual(values=c("grey50","black","blue","red")) +
  theme_bw(base_size=14) +
  geom_hline(yintercept=0,col="black") +
  #geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(title=paste0(guide1,'-',guide2),subtitle=subtitle) + 
  theme(plot.subtitle=element_text(size=10))
dev.off()
#
rm(guide1,guide2,d,d2)

###### fig4.Jmjd6_Moz.jitterplot.pdf ######
#
guide1 <- 'Jmjd6'
guide2 <- 'Moz'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
d2 <- data.frame(value=c(mean(d$value[d$group=="guide1"]),mean(d$value[d$group=="guide2"]),
                         mean(d$value[d$group=="expected.mean"]),mean(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Jmjd6_Moz.jitterplot.pdf',height=8,width=6)
ggplot(d,aes(group,value,col=group)) + 
  geom_jitter(alpha=0.5,width=0.2,shape=20,size=3) +
  scale_color_manual(values=c("grey50","black","blue","red")) +
  geom_crossbar(data=d2, aes(ymin=value, ymax=value),size=0.5,col="black",width=0.5) +
  theme_bw(base_size=14) +
  #geom_hline(yintercept=0,col="black") +
  geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(subtitle=paste0(guide1,'-',guide2))
dev.off()
#
rm(guide1,guide2,d,d2)

###### fig4.Jmjd6_Moz.boxplot.pdf ######
#
guide1 <- 'Jmjd6'
guide2 <- 'Moz'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
d2 <- data.frame(value=c(mean(d$value[d$group=="guide1"]),mean(d$value[d$group=="guide2"]),
                         mean(d$value[d$group=="expected.mean"]),mean(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Jmjd6_Moz.boxplot.pdf',height=8,width=6)
ggplot(d,aes(group,value,col=group)) + 
  geom_boxplot(outlier.shape=NA,width=0.5) +
  scale_color_manual(values=c("grey50","black","blue","red")) +
  theme_bw(base_size=14) +
  #geom_hline(yintercept=0,col="black") +
  geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(subtitle=paste0(guide1,'-',guide2))
dev.off()
#
rm(guide1,guide2,d,d2)

###### fig4.Jmjd6_Moz.barplot.pdf ######
#
guide1 <- 'Jmjd6'
guide2 <- 'Moz'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
#
pval <- format(c(wilcox.test(d4$value,d1$value)$p.value,
                 wilcox.test(d4$value,d2$value)$p.value,
                 wilcox.test(d4$value,d3$value)$p.value),scientific=T,digits=3)
subtitle <- paste0('p-values for guild1/guide2/expected are ',pval[1],'/',pval[2],'/',pval[3])
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
se <- function(x) sqrt(var(x)/length(x))
d2 <- data.frame(value=c(median(d$value[d$group=="guide1"]),median(d$value[d$group=="guide2"]),
                         median(d$value[d$group=="expected.mean"]),median(d$value[d$group=="observed"])),
                 se=c(se(d$value[d$group=="guide1"]),se(d$value[d$group=="guide2"]),
                      se(d$value[d$group=="expected.mean"]),se(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
d2$lower <- d2$value-d2$se
d2$upper <- d2$value+d2$se
d2$group <- factor(d2$group,c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Jmjd6_Moz.barplot.pdf',height=8,width=6)
ggplot(d2,aes(group,value,fill=group)) + 
  geom_bar(stat='identity',width=0.5) +
  geom_point(col="grey20") +
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.3,col="grey20") +
  scale_fill_manual(values=c("grey50","black","blue","red")) +
  theme_bw(base_size=14) +
  geom_hline(yintercept=0,col="black") +
  #geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(title=paste0(guide1,'-',guide2),subtitle=subtitle) + 
  theme(plot.subtitle=element_text(size=10))
dev.off()
#
rm(guide1,guide2,d,d2)

###### fig4.Brpf1_Jmjd6.jitterplot.pdf ######
#
guide1 <- 'Brpf1'
guide2 <- 'Jmjd6'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
d2 <- data.frame(value=c(mean(d$value[d$group=="guide1"]),mean(d$value[d$group=="guide2"]),
                         mean(d$value[d$group=="expected.mean"]),mean(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Brpf1_Jmjd6.jitterplot.pdf',height=8,width=6)
ggplot(d,aes(group,value,col=group)) + 
  geom_jitter(alpha=0.5,width=0.2,shape=20,size=3) +
  scale_color_manual(values=c("grey50","black","blue","red")) +
  geom_crossbar(data=d2, aes(ymin=value, ymax=value),size=0.5,col="black",width=0.5) +
  theme_bw(base_size=14) +
  #geom_hline(yintercept=0,col="black") +
  geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(subtitle=paste0(guide1,'-',guide2))
dev.off()
#
rm(guide1,guide2,d,d2)

###### fig4.Brpf1_Jmjd6.boxplot.pdf ######
#
guide1 <- 'Brpf1'
guide2 <- 'Jmjd6'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
d2 <- data.frame(value=c(mean(d$value[d$group=="guide1"]),mean(d$value[d$group=="guide2"]),
                         mean(d$value[d$group=="expected.mean"]),mean(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Brpf1_Jmjd6.boxplot.pdf',height=8,width=6)
ggplot(d,aes(group,value,col=group)) + 
  geom_boxplot(outlier.shape=NA,width=0.5) +
  scale_color_manual(values=c("grey50","black","blue","red")) +
  theme_bw(base_size=14) +
  #geom_hline(yintercept=0,col="black") +
  geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(subtitle=paste0(guide1,'-',guide2))
dev.off()
#
rm(guide1,guide2,d,d2)

###### fig4.Brpf1_Jmjd6.barplot.pdf ######
#
guide1 <- 'Brpf1'
guide2 <- 'Jmjd6'
## read data
data <- read.csv('per.key.analysis.normal/data.csv',header=T)
#
d1 <- subset(data, (guide1.gene==guide1 & guide2.type=="negative") | (guide2.gene==guide1 & guide1.type=="negative") )
d1 <- subset(d1,select=c(observed))
colnames(d1) <- c("value")
d1$group <- "guide1"
d2 <- subset(data, (guide1.type=="negative" & guide2.gene==guide2) | (guide2.type=="negative" & guide1.gene==guide2) )
d2 <- subset(d2,select=c(observed))
colnames(d2) <- c("value")
d2$group <- "guide2"
d3 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d3 <- subset(d3,select=c(expected.mean))
colnames(d3) <- c("value")
d3$group <- "expected.mean"
d4 <- subset(data, (guide1.gene==guide1 & guide2.gene==guide2) | (guide2.gene==guide1 & guide1.gene==guide2) )
d4 <- subset(d4,select=c(observed))
colnames(d4) <- c("value")
d4$group <- "observed"
d <- rbind(d1,d2,d3,d4)
#
pval <- format(c(wilcox.test(d4$value,d1$value)$p.value,
                 wilcox.test(d4$value,d2$value)$p.value,
                 wilcox.test(d4$value,d3$value)$p.value),scientific=T,digits=3)
subtitle <- paste0('p-values for guild1/guide2/expected are ',pval[1],'/',pval[2],'/',pval[3])
rm(d1,d2,d3,d4)
d$group <- factor(d$group,c("guide1","guide2","expected.mean","observed"))
##
se <- function(x) sqrt(var(x)/length(x))
d2 <- data.frame(value=c(median(d$value[d$group=="guide1"]),median(d$value[d$group=="guide2"]),
                         median(d$value[d$group=="expected.mean"]),median(d$value[d$group=="observed"])),
                 se=c(se(d$value[d$group=="guide1"]),se(d$value[d$group=="guide2"]),
                      se(d$value[d$group=="expected.mean"]),se(d$value[d$group=="observed"])),
                 group=c("guide1","guide2","expected.mean","observed"))
d2$lower <- d2$value-d2$se
d2$upper <- d2$value+d2$se
d2$group <- factor(d2$group,c("guide1","guide2","expected.mean","observed"))
##
pdf('fig4.Brpf1_Jmjd6.barplot.pdf',height=8,width=6)
ggplot(d2,aes(group,value,fill=group)) + 
  geom_bar(stat='identity',width=0.5) +
  geom_point(col="grey20") +
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.3,col="grey20") +
  scale_fill_manual(values=c("grey50","black","blue","red")) +
  theme_bw(base_size=14) +
  geom_hline(yintercept=0,col="black") +
  #geom_hline(yintercept=mean(d$value[d$group=="expected.mean"]),col="blue",linetype="dotted",size=0.5) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  ylab("Performance") + xlab("") +
  labs(title=paste0(guide1,'-',guide2),subtitle=subtitle) + 
  theme(plot.subtitle=element_text(size=10))
dev.off()
#
rm(guide1,guide2,d,d2)

######  ######
