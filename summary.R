library(plyr)
library(data.table)
library(ggplot2)

d <- read.csv("blast/blast.summary.csv",header=T,stringsAsFactors=F)

d$`DKOscreen_d0_rep1.norm_to_10M` <- d$`DKOscreen_d0_rep1`*1000000/sum(d$`DKOscreen_d0_rep1`)
d$`DKOscreen_p12_rep1.norm_to_10M` <- d$`DKOscreen_p12_rep1`*1000000/sum(d$`DKOscreen_p12_rep1`)
d$log2fc.rep1 <- log2( (d$`DKOscreen_p12_rep1.norm_to_10M`+1)/d$`DKOscreen_d0_rep1.norm_to_10M` )
d$log2fc.rep1[d$`DKOscreen_d0_rep1.norm_to_10M`<50] <- NA

d$`DKOscreen_d0_rep2.norm_to_10M` <- d$`DKOscreen_d0_rep2`*1000000/sum(d$`DKOscreen_d0_rep2`)
d$`DKOscreen_p12_rep2.norm_to_10M` <- d$`DKOscreen_p12_rep2`*1000000/sum(d$`DKOscreen_p12_rep2`)
d$log2fc.rep2 <- log2( (d$`DKOscreen_p12_rep2.norm_to_10M`+1)/d$`DKOscreen_d0_rep2.norm_to_10M` )
d$log2fc.rep2[d$`DKOscreen_d0_rep2.norm_to_10M`<50] <- NA

d$`DKOscreen_d0_rep12` <- d$`DKOscreen_d0_rep1` + d$`DKOscreen_d0_rep2`
d$`DKOscreen_p12_rep12` <- d$`DKOscreen_p12_rep1` + d$`DKOscreen_p12_rep2`
d$`DKOscreen_d0_rep12.norm_to_10M` <- d$`DKOscreen_d0_rep12`*1000000/sum(d$`DKOscreen_d0_rep12`)
d$`DKOscreen_p12_rep12.norm_to_10M` <- d$`DKOscreen_p12_rep12`*1000000/sum(d$`DKOscreen_p12_rep12`)
d$log2fc.rep12 <- log2( (d$`DKOscreen_p12_rep12.norm_to_10M`+1)/d$`DKOscreen_d0_rep12.norm_to_10M` )
d$log2fc.rep12[d$`DKOscreen_d0_rep12.norm_to_10M`<50] <- NA

write.csv(d,'summary.csv',row.names=F)

pdf('summary.pdf',height=6,width=6)
ggplot(d, aes(log2fc.rep1,log2fc.rep2)) +
  geom_point(size=0.25, col="red", alpha=0.5) +
  #geom_smooth(method="lm", formula=y~x, col="black", fill="red", alpha=0.1, size=0.5) +
  geom_abline(slope=1) +
  theme_bw(base_size=14) +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(legend.position="none") +
  coord_cartesian(xlim=c(-9,5),ylim=c(-9,5)) 
dev.off()  



