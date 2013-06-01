rm(list=ls())
try(detach("package:Hmisc", unload=TRUE), silent=TRUE)
library(ggplot2)
library(reshape2)
library(plyr)
source("visual_func.R")
load("gov_clean.RData")

#### Change in TI ####

ti$year <- with(ti, as.numeric(levels(year)[year]))
ti <- subset(ti, year<=2011)
ti <- add.rank(ti, "ti")
find.change(subset(ti, ADB==1), "ti")

# Number of country for each year
ddply(ti, c("year"), summarize, no.country=length(country))

#### WGI (more comparable over time) ####

wgi <- add.rank(wgi, "cc_est")
wgi.change <- find.change(subset(wgi, ADB==1 & asia==1), "cc_est")

wgip <- ggplot(data=wgi, aes(x=year, y=cc_est, group=country))
wgip + geom_line(data=subset(wgi, asia==1))
wgip + geom_line(data=subset(wgi, ADB==1))
wgip + geom_line(data=subset(wgi, country %in% wgi.change[[1]])) +
  geom_text(data=subset(wgi, year==2011 & country %in% wgi.change[[1]]), aes(label=country))

plot1 <- wgip + geom_smooth(data=subset(wgi, country %in% wgi.change[[1]]), se=FALSE) + 
  geom_smooth(data=subset(wgi, country=="Vanuatu",), se=FALSE, col='red') +
  geom_text(data=subset(wgi, year==2011 & country %in% wgi.change[[1]]), 
            aes(label=country), hjust=0.75) +
  ylab("Control of Corruption")
ggsave("wgi_corruption.pdf", path="./graph/")

plot2 <- wgip + geom_smooth(data=subset(wgi, country %in% wgi.change[[1]]), aes(y=rank), se=FALSE) + 
  geom_smooth(data=subset(wgi, country=="Vanuatu",), aes(y=rank), se=FALSE, col='red') +
  geom_text(data=subset(wgi, year==2011 & country %in% wgi.change[[1]]), 
            aes(y=rank, label=country), hjust=0.75) +
  ylab("Control of Corruption (Ranking)") + 
  ylim(c( max(wgi$rank, na.rm=TRUE), min(wgi$rank, na.rm=TRUE) ))

pdf("./graph/change_in_corruption.pdf", width=12, height=5)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

wgip(data=subset(wgi, country %in% wgi.change[[2]]))

a <- ddply(wgi[complete.cases(subset(wgi, select=c('region','year','cc_est'))),]
      , c("region", "year"), summarize, mean=mean(cc_est))
p <- ggplot(a, aes(x=year, y=mean, group=region))
p + geom_line() + geom_text(data=subset(a, year==2011), aes(label=region)) +
  scale_y_continuous(breaks=seq(-2.5, 2.5, by=0.5))
p + geom_line(aes(color=region))