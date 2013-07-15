rm(list=ls())
try(detach("package:Hmisc", unload=TRUE), silent=TRUE)
toInstall <- c("ggplot2", "reshape2", "plyr", "rgdal", "maptools", "animation")
lapply(toInstall, library, character.only=TRUE)
source("visual_func.R")
load("gov_clean.RData")

### Define new theme ###

theme_mine <- theme_minimal() + 
  theme(axis.title = element_blank(), 
        axis.text.x = element_text(size=15), 
        axis.text.y = element_text(size=25),
        legend.title=element_blank(),
        legend.text = element_text(size=15),
        legend.position="bottom")

#### Implementation gap ####

d1 <- subset(gir11, country %in% c("India", "Germany", "United.States"),
             select=c("legal", "imp", "country"))
d1 <- within(d1, country[country=="United.States"] <- "USA")
d1 <- within(d1, country <- factor(country, levels=country[order(imp)]))
d1.long <- melt(d1, id="country")
d1.p1 <- ggplot(subset(d1.long, variable=="legal"), aes(country, value, fill=variable))


png("./presentation/mystery_country.png", width=650, height=240)
d1.p1 + geom_bar(stat="identity") +
  scale_x_discrete(labels=c("???", "Germany", "USA")) +
  scale_fill_brewer(palette="Set1", "Variable", labels=c("Legal Framework")) +
  theme_mine + coord_flip()
dev.off()

png("./presentation/india_revealed1.png", width=650, height=240)
d1.p1 + geom_bar(stat="identity") +
  scale_x_discrete(labels=c("India", "Germany", "USA")) +
  scale_fill_brewer(palette="Set1", "Variable", labels=c("Legal Framework")) +
  theme_mine + coord_flip()
dev.off()

png("./presentation/india_revealed2.png", width=650, height=300)
d1.p2 <- ggplot(d1.long, aes(country, value, fill=variable))
d1.p2 + geom_bar(stat="identity", position=position_dodge(width=0.6)) +
  scale_x_discrete(labels=c("India", "Germany", "USA")) +
  scale_fill_brewer(palette="Set1", "Variable", labels=c("Legal Framework", "Implementation")) +
  theme_mine + coord_flip()
dev.off()
#### All country ####

png("./presentation/all_country.png", width=650, height=500)
d <- subset(gir11, country %in% c("Armenia", "Vietnam", "Azerbaijan", "Georgia", "Indonesia", "India", "Mongolia", "China", "Germany", "United.States"),
            select=c("legal", "imp", "country"))
d <- within(d, country[country=="United.States"] <- "USA")
d <- within(d, country <- factor(country, levels=country[order(imp)]))
gir11.long <- melt(d, id="country")
girp <- ggplot(gir11.long, aes(country, value, fill=variable))
girp + geom_bar(stat="identity", position=position_dodge(width=0.5)) +
  scale_fill_brewer(palette="Set1", "Variable", labels=c("Legal Framework", "Implementation")) +
  geom_vline(xintercept=8.5, linetype="longdash") +
  theme_mine + coord_flip()
dev.off()

#### Animated world corruption ####

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

saveGIF({
  for (i in c(1996, 1998, 2000, 2002:2011)) {
    temp <- subset(na.omit(wgi[, c('year', 'country', 'cc_est')]), year==i)
    choro <- merge(world.ggmap, temp, by.x="id", by.y="country")
    choro <- choro[order(choro$order), ]
    p <- ggplot(data=choro, aes(long, lat, fill=cc_est, group=group))
    print(
      p + geom_polygon() + ylim(c(-60,85)) +
        theme(text=element_text(size=20)) +
        scale_fill_gradient2("Control of\n corruption\n score", limits=c(-2.5,2.5)) + 
        labs(title=substitute(paste("Severity of corruption in the world, year ", i), list(i=i)))
    )
    #     qplot(long, lat, data=choro, fill=cc_est, group=group, geom="polygon") +
    #       scale_fill_gradient()
  }
}, ani.basename = "world_corruption", interval=1,
          ani.width=900, ani.height=450,
          ani.opts="autoplay,loop,controls,width=\\linewidth",
          movie.name = "world.corruption.gif", 
          outdir="D:/Projects/corruption/presentation")

saveVideo({
  for (i in c(1996, 1998, 2000, 2002:2011)) {
    temp <- subset(na.omit(wgi[, c('year', 'country', 'cc_est')]), year==i)
    choro <- merge(world.ggmap, temp, by.x="id", by.y="country")
    choro <- choro[order(choro$order), ]
    p <- ggplot(data=choro, aes(long, lat, fill=cc_est, group=group))
    print(
      p + geom_polygon() + ylim(c(-60,85)) +
        theme(text=element_text(size=20)) +
        scale_fill_gradient2("Control of\n corruption\n score", limits=c(-2.5,2.5)) + 
        labs(title=substitute(paste("Severity of corruption in the world, year ", i), list(i=i)))
    )
    #     qplot(long, lat, data=choro, fill=cc_est, group=group, geom="polygon") +
    #       scale_fill_gradient()
  }
}, video.name = "world_corruption.mp4", interval=1,
        ani.width=900, ani.height=450,
        outdir="D:/Projects/corruption/presentation")
