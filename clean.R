rm(list=ls())

library(reshape2)
library(plyr)
library(gtools)
library(Hmisc)
source("clean_func.R")
source("load.R")

#### Article cleaning ####
article <- subset(article, complete.cases(country))
article <- rename(article, replace=c("wbcode"="countrycode"))
article$country <- tolower(article$country)
article$country <- sapply(article$country, FUN=simpleCap)
article <- clean.name(article)

#### Extract region from article ####

region <- subset(article, select=c('country', 'countrycode', 'africa', 'asia.',
                                   'ceurope', 'weuro', 'meast', 
                                   'nam', 'sam', 'scan'))
region <- rename(region, replace=c('asia.'='asia', 'ceurope'='ceuro'))

temp <- with(region, paste(africa, asia, ceuro, weuro, meast, nam, sam, scan))
find.pos <- function(s) {
  grep('1', unlist(strsplit(s, split=' ')))
}
a <- as.factor( as.numeric(lapply(temp, FUN=find.pos)) )
levels(a) <- c('africa', 'asia', 'ceuro', 'weuro', 'meast', 'nam', 'sam', 'scan')

region <- cbind(region, region=a)

with(region, interaction(africa, asia))
#### CPI cleaning ####
# Invoke add.year to add a column of year
for (obj in c('ti00', 'ti01', 'ti02', 'ti03', 'ti04', 'ti05',
              'ti06', 'ti07', 'ti08', 'ti09', 'ti10', 'ti11', 'ti12')) {
  eval(parse(text=( paste(obj, ' <- add.year(', obj, ', "20")', sep='') )))
}

for (obj in c('ti98', 'ti99')) {
  eval(parse(text=( paste(obj, ' <- add.year(', obj, ', "19")', sep='') )))
}

# Bind all ti together
ti <- smartbind(ti98, ti99, ti00, ti01, ti02, ti03, ti04, ti05, 
                ti06, ti07, ti08, ti09, ti10, ti11, ti12)
# Keep some variables
keeps <- c('country', 'year', 'ti', 'ti.std', 'survey_used')
ti <- ti[, keeps]
ti <- ti[order(ti$country, ti$year), ]

# Add dummy for ADB membership
ti<- cbind(ti, ADB=in.ADB(ti$country))

# remove individual ti files
rm(list=ls(pattern="^ti.."))

#### WB cleaning ####
wb05 <- clean.wb(wb05)
wb05 <- clean.name(wb05)
wb11 <- clean.wb(wb11)
wb11 <- clean.name(wb11)

#### WGI cleaning ####
wgi <- rename(wgi, replace=c("CTRY"="countrycode"))
names(wgi) <- tolower(names(wgi))
wgi$country <- tolower(wgi$country)

wgi$country <- sapply(wgi$country, FUN=simpleCap)

wgi <- clean.name(wgi)
wgi <- merge(wgi, region, by='countrycode')
wgi <- rename(wgi, replace=c('country.x'='country'))
drops <- c('country.y') ; wgi <- wgi[, !(names(wgi) %in% drops)]
wgi <- cbind(wgi, ADB=in.ADB(wgi$country))

#### GIR cleaning ###
gir11 <- t(gir11)
colnames(gir11) <- gir11[2,]
gir11 <- gir11[3:nrow(gir11), ]
gir11 <- data.frame(country=row.names(gir11), gir11)
colnames(gir11) <- c("country", "overall", "legal", "imp", "gap")
gir11$country <- as.character(gir11$country)
gir11[2:5] <- lapply(gir11[2:5], as.character)
gir11[2:5] <- lapply(gir11[2:5], as.numeric)

#### Save cleaned data ####
save(article, ti, wb05, wb11, wgi, region, gir11, file="gov_clean.RData")

