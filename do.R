rm(list=ls())
try(detach("package:Hmisc", unload=TRUE), silent=TRUE)
library(ggplot2)
library(reshape2)
library(plyr)
library(rgdal)
library(maptools)
library(animation)
source("visual_func.R")
load("gov_clean.RData")

#### Compare wgi of regions ####
a <- ddply(wgi[complete.cases(subset(wgi, select=c('region','year','cc_est'))),]
           , c("region", "year"), summarize, mean=mean(cc_est))

p <- ggplot(a, aes(x=year, y=mean, group=region))
p + geom_line() + geom_text(data=subset(a, year==2011), aes(label=region)) +
  scale_y_continuous(breaks=seq(-2.5, 2.5, by=0.5)) + geom_line(aes)
p + geom_line(aes(color=region))

#### Plot the highest variance countries ####

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

#### Graph of implementation gap ####

# d <- subset(gir11, country %in% c("Indonesia", "India", "Mongolia", "China", "Germany", "United.States"),
#                     select=c("legal", "imp", "country"))

d <- subset(gir11, country %in% c("Armenia", "Vietnam", "Azerbaijan", "Georgia", "Indonesia", "India", "Mongolia", "China", "Germany", "United.States"),
            select=c("legal", "imp", "country"))
d <- within(d, country[country=="United.States"] <- "USA")
d <- within(d, country <- factor(country, levels=country[order(imp, decreasing=TRUE)]))
gir11.long <- melt(d, id="country")

girp <- ggplot(gir11.long, aes(country, value, fill=variable))
girp + geom_bar(stat="identity", width=1, position=position_dodge(width=0.5)) +
  scale_fill_brewer(palette="Set1", "Variable", labels=c("Legal Framework", "Implementation")) +
  geom_vline(xintercept=2.5, linetype="longdash") + coord_flip()

ggsave("./graph/implementation.pdf", width=7, height=7)

#### Animated world corruption ####

world.map <- readOGR(dsn="./mapdata", layer="TM_WORLD_BORDERS_SIMPL-0.3")
world.ggmap <- fortify(world.map, region = "NAME")

wgi <- add.rank(wgi, "cc_est")
wgi$country[wgi$country=="Brunei"] <- "Brunei Darussalam"
wgi$country[wgi$country=="Laos"] <- "Lao People's Democratic Republic"
wgi$country[wgi$country=="North Korea"] <- "Korea, Democratic People's Republic of"
wgi$country[wgi$country=="South Korea"] <- "Korea, Republic of"
wgi$country[wgi$country=="Hong Kong Sar, China"] <- "Hong Kong"
wgi$country[wgi$country=="Iran"] <- "Iran (Islamic Republic of)"
wgi$country[wgi$country=="Libya"] <- "Libyan Arab Jamahiriya"
wgi$country[wgi$country=="Macedonia, Fyr"] <- "The former Yugoslav Republic of Macedonia"
wgi$country[wgi$country=="Russian Federation"] <- "Russia"
wgi$country[wgi$country=="St. Vincent and The Grenadines"] <- "Saint Vincent and the Grenadines"
wgi$country[wgi$country=="St. Lucia"] <- "Saint Lucia"
wgi$country[wgi$country=="Syria"] <- "Syrian Arab Republic"
wgi$country[wgi$country=="Netherlands Antilles (former)"] <- "Netherlands Antilles"
wgi$country[wgi$country=="Timor-leste"] <- "Timor-Leste"
wgi$country[wgi$country=="Taiwan, China"] <- "Taiwan"
wgi$country[wgi$country=="Micronesia, Fed. Sts."] <- "Micronesia, Federated States of"
wgi$country[wgi$country=="Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
wgi$country[wgi$country=="Venezuela, Rb"] <- "Venezuela"
wgi$country[wgi$country=="Virgin Islands (u.s.)"] <- "United States Virgin Islands"
wgi$country[wgi$country=="Vietnam"] <- "Viet Nam"
wgi$country[wgi$country=="Guinea-bissau"] <- "Guinea-Bissau"
wgi$country[wgi$country=="Congo Republic"] <- "Congo"
wgi$country[wgi$country=="Côte D'ivoire"] <- "Cote d'Ivoire"  
wgi$country[wgi$country=="Myanmar"] <- "Burma"  
wgi$country[wgi$country=="São Tomé and Principe"] <- "Sao Tome and Principe"  
wgi$country[wgi$country=="Tanzania"] <- "United Republic of Tanzania" 
wgi$country[wgi$country=="West Bank and Gaza"] <- "Palestine"
wgi$country[wgi$country=="Moldova"] <- "Republic of Moldova"

# temp <- subset(na.omit(wgi[, c('year', 'country', 'cc_est')]), year==2011)
# choro <- merge(world.ggmap, temp, by.x="id", by.y="country", all=T)
# unique(choro$id[is.na(choro$cc_est)])

saveLatex({
  for (i in c(1996, 1998, 2000, 2002:2011)) {
    temp <- subset(na.omit(wgi[, c('year', 'country', 'cc_est')]), year==i)
    choro <- merge(world.ggmap, temp, by.x="id", by.y="country")
    choro <- choro[order(choro$order), ]
    p <- ggplot(data=choro, aes(long, lat, fill=cc_est, group=group))
    print(
      p + geom_polygon() + ylim(c(-60,85)) +
        theme(text=element_text(size=20))+
        scale_fill_gradient2("Control of\n corruption\n score", limits=c(-2.5,2.5)) + 
        labs(title=substitute(paste("Severity of corruption in the world, year ", i), list(i=i)))
    )
    #     qplot(long, lat, data=choro, fill=cc_est, group=group, geom="polygon") +
    #       scale_fill_gradient()
  }
}, ani.basename = "world_corruption", interval=1,
          ani.width=900, ani.height=450,
          ani.opts="autoplay,loop,controls,width=\\linewidth",
          latex.filename = "world.corruption.tex", 
          outdir="D:/Projects/corruption/graph")

