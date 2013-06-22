rm(list=ls())
library(XLConnect)
library(foreign)
source("clean_func.R")

article <- loadWorkbook("rawdata/what_have_we_learned_data.xls")
article <- readWorksheet(article, sheet="Data")
wb11 <- read.dta("rawdata/wb11.dta")
wb05 <- read.dta("rawdata/wb05.dta")
wgi <- read.dta("rawdata/wgidataset.dta")
fdi <- read.dta("rawdata/fdi.dta")

gir11.book <- loadWorkbook("rawdata/GIR11_data.xls")
gir11 <- readWorksheet(gir11.book, sheet="Scorecard 2011", region="A1:AL5")
gir11 <- gir11[, !(names(gir11) %in% c("Category", "Subcat", "QID", 
                                       "Question.Set", "Title", "Median"))]
gir11 <- cbind(country=row.names(gir11), gir11)

# for (file in Sys.glob("rawdata/CPI*.xls")) {
#   df.name <- strsplit(file, split="/")[[1]][2]
#   df.name <- strsplit(df.name, split="\\.")[[1]][1]
#   temp <- loadWorkbook(file)
#   assign(df.name, temp)
# }

ti98 <- clean.ti("rawdata/CPI1998.xls", "Sheet1", startRow=2, endCol=5,
                    names=c('rank', 'country','ti', 'ti.std', 'survey_used'))
ti99 <- clean.ti("rawdata/CPI1999.xls", "Sheet1", startRow=3, endCol=5,
                    names=c('rank', 'country','ti', 'ti.std', 'survey_used'))
ti00 <- clean.ti("rawdata/CPI2000.xls", "Sheet1", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti01 <- clean.ti("rawdata/CPI2001.xls", "Sheet1", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti02 <- clean.ti("rawdata/CPI2002.xls", "Sheet1", startRow=3, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti03 <- clean.ti("rawdata/CPI2003.xls", "Sheet1", startRow=2, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti04 <- clean.ti("rawdata/CPI2004.xls", "2004 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti05 <- clean.ti("rawdata/CPI2005.xls", "2005 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti06 <- clean.ti("rawdata/CPI2006.xls", "2006 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti07 <- clean.ti("rawdata/CPI2007.xls", "2007 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti08 <- clean.ti("rawdata/CPI2008.xls", "2008 CPI", startRow=4, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti09 <- clean.ti("rawdata/CPI2009.xls", "Sheet1", startRow=2, endCol=4,
                    names=c('rank', 'country','ti', 'survey_used'))
ti10 <- clean.ti("rawdata/CPI2010.xls", "CPI table", startRow=5, endCol=5,
                    names=c('rank', 'country','ti', 'survey_used', 'ti.std'))
ti11 <- clean.ti("rawdata/CPI2011.xls", "Global", startRow=3, endCol=6,
                    names=c('rank', 'country','ti', 'rank', 'survey_used', 'ti.std'))
ti12 <- clean.ti("rawdata/CPI2012.xls", "CPI 2012", startRow=3, endCol=7,
                    names=c('rank', 'country', 'region', 'ti', ' ', 'survey_used', 'ti.std'))

