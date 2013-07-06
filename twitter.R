rm(list=ls())
toInstall <- c("twitteR", "ggplot2", "dismo", "maps", "grid")
lapply(toInstall, library, character.only=TRUE)

options(RCurlOptions = list( capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

consumer_key <- "1NtqH9qnMgzcK82RemH28g"
consumer_secret <-"mwpGgnqSVAoRKrOGnFYZy0CUnrZe2hZcmEfk8NkgT5g"
getTwitterOAuth(consumer_key, consumer_secret)

# Search hashtag
searchRes <- searchTwitter('#SaveKPK', since='2013-06-01', until='2013-06-29', n=500)
save(searchRes, file="tweet0629.RData")
tweetFrame <- twListToDF(searchRes)
# Note that the Twitter search API only goes back 1500 tweets / 1 week

userInfo <- lookupUsers(tweetFrame$screenName)
userFrame <- twListToDF(userInfo)
locatedUser <- !is.na(userFrame$location)

temp <- userFrame$location[locatedUser]
temp[temp=="Makassar"] <- "Mangkasara"
temp[temp=="Berau - Makassar"] <- "Mangkasara"
temp[temp=="matahari"] <- "Indonesia"
temp[temp=="Briliant ayesha nadine"] <- "Mangkasara"
temp[temp=="Batavia"] <- "Jakarta"
temp[temp=="Buitenzorg"] <- "Kota Bogor"

locations <- geocode(temp, oneRecord=TRUE)
locations$latitude[locations$originalPlace=="ÜT: -6.1905027,106.7682388"] <- -6.1905027
locations$longitude[locations$originalPlace=="ÜT: -6.1905027,106.7682388"] <- 106.7682388
locations$latitude[locations$originalPlace=="ÜT: -0.9388407,100.395019"] <- -0.9388407
locations$longitude[locations$originalPlace=="ÜT: -0.9388407,100.395019"] <- 100.395019
locations$latitude[locations$originalPlace=="ÜT: -5.69944,119.69805"] <- -5.69944
locations$longitude[locations$originalPlace=="ÜT: -5.69944,119.69805"] <- 119.69805
locations$latitude[locations$originalPlace=="�T: -6.1905027,106.7682388"] <- -6.1905027
locations$longitude[locations$originalPlace=="�T: -6.1905027,106.7682388"] <- 106.7682388

world.map <- map_data("world")

PlotTweet <- function(data, xlim=NULL, ylim=NULL) {
  ggplot(data=data, aes(long, lat)) + geom_path(aes(group=group)) +
    geom_point(data=locations, aes(longitude, latitude), color='red', size=3) +
    theme_minimal() + coord_cartesian(xlim, ylim) +
    theme(line = element_blank(),
          text = element_blank(),
          line = element_blank(),
          title = element_blank())
}

indo <- PlotTweet(subset(world.map, region=="Indonesia"), 
             xlim=c(80, 150), ylim=c(-15, 10))
us <- PlotTweet(subset(world.map, region=="USA"),
          xlim=c(-200, -50), ylim=c(20, 80))

grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 1)))
vplayout <- function(x, y) {
  viewport(layout.pos.row=x, layout.pos.col=y)
}
print(indo, vp=vplayout(1, 1))
print(us, vp=vplayout(2, 1))