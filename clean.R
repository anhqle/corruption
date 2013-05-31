load("clean_fun.RData")

article <- loadWorkbook("what_have_we_learned_data.xls")
d.article <- readWorksheet(article, sheet="Data", endRow=217)
d.article <- rename(d.article, replace=c("wbcode"="countrycode", "asia."="asia"))
d.article <- cbind(d.article, ADB=in.ADB(d.article$country))

wb11 <- clean.wb("WB/wb11.dta", "11")
wb11 <- cbind(wb11, ADB=in.ADB(wb11$country))
wb05 <- clean.wb("WB/wb05.dta", "05")
wb05 <- cbind(wb05, ADB=in.ADB(wb05$country))

temp <- read.dta("WB/wgidataset.dta")

wgi <- clean.wgi("WB/wgidataset.dta")

ti98 <- clean.ti.ts("CPI/CPI1998.xls", "Sheet1", startRow=2, endCol=5,
                    names=c('rank', 'country','ti', 'ti.std', 'survey_used'))
ti99 <- clean.ti.ts("CPI/CPI1999.xls", "Sheet1", startRow=3, endCol=5,
                    names=c('rank', 'country','ti', 'ti.std', 'survey_used'))
ti00 <- clean.ti.ts("CPI/CPI2000.xls", "Sheet1", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti01 <- clean.ti.ts("CPI/CPI2001.xls", "Sheet1", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti02 <- clean.ti.ts("CPI/CPI2002.xls", "Sheet1", startRow=3, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti03 <- clean.ti.ts("CPI/CPI2003.xls", "Sheet1", startRow=2, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti04 <- clean.ti.ts("CPI/CPI2004.xls", "2004 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti05 <- clean.ti.ts("CPI/CPI2005.xls", "2005 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti06 <- clean.ti.ts("CPI/CPI2006.xls", "2006 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti07 <- clean.ti.ts("CPI/CPI2007.xls", "2007 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti08 <- clean.ti.ts("CPI/CPI2008.xls", "2008 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti09 <- clean.ti.ts("CPI/CPI2009.xls", "Sheet1", startRow=2, endCol=4,
                    names=c('rank', 'country','ti', 'survey_used'))
ti10 <- clean.ti.ts("CPI/CPI2010.xls", "CPI table", startRow=5, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti11 <- clean.ti.ts("CPI/CPI2011.xls", "Global", startRow=3, endCol=6,
                    names=c('rank', 'country','ti', 'rank', 'survey_used', 'ti.std'))
ti12 <- clean.ti.ts("CPI/CPI2012.xls", "CPI 2012", startRow=3, endCol=7,
                    names=c('rank', 'country', 'region', 'ti', ' ', 'survey_used', 'ti.std'))

add.year <- function(tixx, prefix) {
  year <- substr(deparse(substitute(tixx)), 3, 4)
  full_year <- paste(prefix, year, sep='')
  tixx <- cbind(tixx, year=rep(full_year, nrow(tixx)))
  tixx
}

for (obj in c('ti00', 'ti01', 'ti02', 'ti03', 'ti04', 'ti05',
              'ti06', 'ti07', 'ti08', 'ti09', 'ti10', 'ti11', 'ti12')) {
  eval(parse(text=( paste(obj, ' <- add.year(', obj, ', "20")', sep='') )))
  
}
for (obj in c('ti98', 'ti99')) {
  eval(parse(text=( paste(obj, ' <- add.year(', obj, ', "19")', sep='') )))
  
}

dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]

ti <- smartbind(ti98, ti99, ti00, ti01, ti02, ti03, ti04, ti05, 
                ti06, ti07, ti08, ti09, ti10, ti11, ti12)
keeps <- c('country', 'year', 'ti', 'ti.std', 'survey_used')
ti <- ti[, keeps]
ti <- ti[order(ti$country, ti$year), ]

# Add dummy for ADB

ti<- cbind(ti, ADB=in.ADB(ti$country))

save(ti, file="ti99_12.RData")