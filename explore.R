rm(list=ls())
library(ggplot2)
library(reshape2)
library(plyr)

load("gov_clean.RData")

#### Change in TI ####

ti$year <- with(ti, as.numeric(levels(year)[year]))
ti <- subset(ti, year<=2011)
ti <- ddply(ti, c("year"), transform, rank=rank(-ti, ties.method=c("first")))
ti <- ddply(ti, c("country"), transform, change.ti=c(NA, diff(ti)))
ti <- ddply(ti, c("country"), transform, change.rank=c(NA, diff(rank)))
# Identify countries with high variance in score and in rank
ti.var <- ddply(subset(ti, ADB==1), c('country'), summarize, variance=var(ti))
hi.change <- with(ti.var, country[variance>=quantile(variance, 0.75)])
ti.var.rank <- ddply(subset(ti, ADB==1), c('country'), summarize, var.rank=var(rank))
hi.change.rank <- with(ti.var.rank, country[var.rank>=quantile(var.rank, 0.75)])
# Number of country for each year
ddply(ti, c("year"), summarize, no.country=length(country))

#### WGI (more comparable over time) ####

wgip <- ggplot(data=wgi, aes(x=year, y=cc_est))
wgip + geom_line(data=subset(wgi, asia==1), aes(y=cc_est))

with(wgi, as.numeric(region))

group.plot <- function(data, group) {
  geom_line(data=subset(data, region==group))
}
wgip + geom_line(data=subset(wgi, region=='africa'))

ddply(wgi[complete.cases(subset(wgi, select=c('region','year','cc_est'))),]
      , c("region", "year"), summarize, mean=mean(cc_est))