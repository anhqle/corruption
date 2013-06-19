rm(list=ls())
try(detach("package:Hmisc", unload=TRUE), silent=TRUE)
library(ggplot2)
library(reshape2)
library(plyr)
library(maps)
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


p <- ggplot(data=na.omit(wgi[, c('region', 'year', 'country', 'cc_est')]),
                         aes(year, cc_est, group=country))
p + geom_line(aes(col=region)) + facet_wrap( ~ region)

p2 <- ggplot(data=temp, aes(x=cc_est))
p2 + geom_histogram() + facet_wrap( ~ region)
p2 + geom_freqpoly(aes(y=..density.., col=region))

wgip(data=subset(wgi, country %in% wgi.change[[2]]))

a <- ddply(wgi[complete.cases(subset(wgi, select=c('region','year','cc_est'))),]
      , c("region", "year"), summarize, mean=mean(cc_est))
p <- ggplot(a, aes(x=year, y=mean, group=region))
p + geom_line() + geom_text(data=subset(a, year==2011), aes(label=region)) +
  scale_y_continuous(breaks=seq(-2.5, 2.5, by=0.5))
p + geom_line(aes(color=region))

saveHTML({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})
temp <- subset(na.omit(wgi[, c('region', 'year', 'country', 'cc_est')]), year==2011)
country <- map_data("world")
temp.new <- temp[, !(names(temp) %in% c('region'))]
temp.new$country[temp.new$country=="United States"] <- "USA"
temp.new$country[temp.new$country=="Russian Federation"] <- "USSR"
temp.new$country[temp.new$country=="United Kingdom"] <- "UK"
temp.new$country[temp.new$country=="St. Vincent and the Grenadines"] <- "Saint Vincent"
temp.new$country[temp.new$country=="St. Lucia"] <- "Saint Lucia"
temp.new$country[temp.new$country=="Congo, Dem. Rep."] <- "Zaire"
choro <- merge(country, temp.new, by.x="region", by.y="country")
choro <- choro[order(choro$order), ]
qplot(long, lat, data=choro, fill=cc_est, group=group, geom="polygon") +
  scale_fill_gradient("Control of\ncorruption\n effectiveness")
ggsave("./graph/corruption_map.pdf")

# Checking ADB's effect in Nepal

d <- subset(wgi, country=="Nepal", select=c("year", "cc_est"))
d <- d[order(d$year), ]
d