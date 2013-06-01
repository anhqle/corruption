rm(list=ls())
library(ggplot2)
library(reshape2)
library(plyr)

load("gov_clean.RData")

# Compare wgi of regions
a <- ddply(wgi[complete.cases(subset(wgi, select=c('region','year','cc_est'))),]
           , c("region", "year"), summarize, mean=mean(cc_est))

p <- ggplot(a, aes(x=year, y=mean, group=region))
p + geom_line() + geom_text(data=subset(a, year==2011), aes(label=region)) +
  scale_y_continuous(breaks=seq(-2.5, 2.5, by=0.5)) + geom_line(aes)
p + geom_line(aes(color=region))

# Plot the highest variance countries

wgi <- add.rank(wgi, "cc_est")
wgi.change <- find.change(subset(wgi, ADB==1 & asia==1), "cc_est")

plot1 <- wgip + geom_smooth(data=subset(wgi, country %in% wgi.change[[1]]), se=FALSE) + 
  geom_smooth(data=subset(wgi, country=="Vanuatu",), se=FALSE, col='red') +
  geom_text(data=subset(wgi, year==2011 & country %in% wgi.change[[1]]), 
            aes(label=country), hjust=0.75) +
  ylab("Control of Corruption")

plot2 <- wgip + geom_smooth(data=subset(wgi, country %in% wgi.change[[1]]), aes(y=rank), se=FALSE) + 
  geom_smooth(data=subset(wgi, country=="Vanuatu",), aes(y=rank), se=FALSE, col='red') +
  geom_text(data=subset(wgi, year==2011 & country %in% wgi.change[[1]]), 
            aes(y=rank, label=country), hjust=0.75) +
  ylab("Control of Corruption (Ranking)") + 
  ylim(c( max(wgi$rank, na.rm=TRUE), min(wgi$rank, na.rm=TRUE) ))

pdf("./graph/change_in_corruption.pdf", width=12, height=5)
grid.arrange(plot1, plot2, ncol=2)
dev.off()
