rm(list=ls())
library(ggplot2)
library(reshape2)
library(plyr)

load("wb.RData")
load("ti99_12.RData")

base.plot <- function(data, x, y) {
  ggplot(data=data, aes_string(x=x)) +
    geom_text(aes_string(y=y, label="countrycode"), size=3) +
    geom_smooth(aes_string(y=y))
}

#### Change in TI ####
ti$year <- with(ti, year)
ti <- subset(ti, year<=2011)
ti <- ddply(ti, c("year"), transform, rank=rank(-ti, ties.method=c("first")))
ti <- ddply(ti, c("country"), transform, change=c(NA, diff(ti)))
ti.var <- ddply(subset(ti, ADB==1), c('country'), summarize, variance=var(ti))
hi.change <- with(ti.var, country[variance>=quantile(variance, 0.75)])
ti.var.rank <- ddply(subset(ti, ADB==1), c('country'), summarize, var.rank=var(rank))
hi.change.rank <- with(ti.var.rank, country[var.rank>=quantile(var.rank, 0.75)])

ddply(ti, c("year"), summarize, no.country=length(country))


ti.plot <- ggplot(data=subset(ti, ADB==1), aes(x=year, y=ti, group=country))
ti.plot + geom_line()
ti.plot + geom_line() + 
  geom_text(data=subset(ti, ADB==1 & year==2011), 
            aes(label=country), size=4)
ti.plot + geom_line() + geom_smooth(aes(group=1), size=2, se=2)
ti.plot + geom_boxplot(aes(group=year))
ti.plot + geom_boxplot(aes(group=year)) + geom_line(col='lightblue')

change.plot <- ggplot(data=subset(ti, ADB==1), aes(x=year, y=change, group=country))

change.plot + geom_line() + 
  geom_text(data=subset(ti, ADB==1 & year==2011), 
            aes(label=country), size=4) +
  geom_line(data=subset(ti, country=="Indonesia" & year<=2011), col='red')

change.plot + geom_line(data=subset(ti, country %in% hi.change))
ti.plot + geom_line(data=subset(ti, country %in% hi.change)) +
  geom_text(data=subset(ti, country %in% hi.change & year==2011), aes(label=country)) +
  ylim(0,10)

rank.plot <- ggplot(data=subset(ti, ADB==1), aes(x=year, y=rank, group=country))
rank.plot + geom_line() + geom_smooth(aes(group=1))
rank.plot + geom_line(data=subset(ti, country %in% hi.change.rank)) +
  geom_text(data=subset(ti, country %in% hi.change.rank & year==2011), aes(label=country)) +
  scale_y_reverse() +
  geom_smooth(aes(group=1))

ggplot(data=ti, aes(year, rank)) + geom_smooth(data=subset(ti, ADB==1), aes(group=1))

base.plot(d05, "ti_05", "log(gdppp_05)")

d11.full <- merge(d11, 
            subset(d.article, select=c('countrycode', 'mad1900', 'mad1820', 'mad1700', 'asia')),
                  by='countrycode')
base.plot(d11.full, "ti_11", "log(mad1900)")
base.plot(d11.full, "ti_11", "log(mad1820)")
base.plot(d11.full, "ti_11", "log(mad1700)")

base.plot(d11, "ti_11", "log(gdppp_11)") + 
  geom_text(data=subset(d11.full, asia==1), aes(y=log(gdppp_11), label=countrycode), col='red', size=3)

ggplot(data=d11, aes(x=ti_11, y=ti.std_11)) + geom_point()