# Run after load.R

library(reshape2)
library(plyr)
library(gtools)
library(Hmisc)
source("clean_func.R")

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

simpleCap <- function(x) { # function to capitalize first letter each word
  s <- strsplit(x, " ")[[1]]
  first <- substring(s, 1, 1)
  rest <- substring(s, 2)
  paste( ifelse(s!="and", toupper(first), first), rest, sep='', collapse=' ')
}
wgi$country <- sapply(wgi$country, FUN=simpleCap)

wgi <- clean.name(wgi)

#### Article cleaning ####
article <- subset(article, complete.cases(country))
article <- rename(article, replace=c("wbcode"="countrycode"))
article$country <- tolower(article$country)
article$country <- sapply(article$country, FUN=simpleCap)
article <- clean.name(article)

save(article, ti, wb05, wb11, wgi, file="gov_clean.RData")