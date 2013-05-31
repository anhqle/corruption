rm(list=ls())
library(foreign)
library(plyr)
library(XLConnect)
library(stringr)
library(reshape2)

clean.wb <- function(file.name, year) { # file.name = "wb11.dta"
                                        # year = "11" to be attached to var name
  d <- read.dta(file.name)
  d <- rename(d, replace=c("countryname"="country"))
  d <- d[(d$region!='Aggregates' & d$region!=''),]
  
  # Rename and drop variables
  d <- rename(d, replace=c(  "ny_gdp_mktp_cd"="gdp",
                             "ny_gdp_mktp_kd_zg"="gdpgrowth",
                             "ny_gdp_pcap_cd"="gdppp",
                             "ny_gdp_pcap_kd_zg"="gdpppgrowth",
                             "ic_bus_ease_xq"="bus_ease",
                             "ic_bus_invs_xq"="bus_invs",
                             "ic_cns_corr_zs"="corr_asproblem_percent",
                             "ny_gdp_petr_rt_zs"="oil_percentgdp"))
  drops <- c("country.y", "iso2code", "region", "year")
  d <- d[, !(names(d) %in% drops)]
  
  names(d)[4:length(names(d))] <- paste(names(d)[4:length(names(d))], year, sep="_")
  
  # Rename country
  d$country[d$country=="Hong Kong SAR, China"] <- "Hong Kong"
  d$country[d$country=="Egypt, Arab Rep."] <- "Egypt"
  d$country[d$country=="Korea, Dem. Rep."] <- "North Korea"
  d$country[d$country=="Korea, Rep."] <- "South Korea"
  d$country[d$country=="Lao PDR"] <- "Laos"
  d$country[d$country=="Macao SAR, China"] <- "Macau"
  d$country[d$country=="Russian Federation"] <- "Russia"
  d$country[d$country=="Venezuela, RB"] <- "Venezuela"
  d$country[d$country=="Yemen, Rep."] <- "Yemen"
  d$country[d$country=="Bahamas, The"] <- "Bahamas"
  d$country[d$country=="Brunei Darussalam"] <- "Brunei"
  d$country[d$country=="Congo, Rep."] <- "Congo Republic"
  d$country[d$country=="Gambia, The"] <- "Gambia"
  d$country[d$country=="Iran, Islamic Rep."] <- "Iran"
  d$country[d$country=="Kyrgyz Republic"] <- "Kyrgyzstan"
  d$country[d$country=="Slovak Republic"] <- "Slovakia"
  d$country[d$country=="Syrian Arab Republic"] <- "Syria"
  
  # Return the cleaned data frame
  d
}

clean.wgi <- function(file.name) { # file.name = "wb11.dta"
  # year = "11" to be attached to var name
  d <- read.dta(file.name)
  rename(d, replace=c("COUNTRY"="country"))
  
  # Rename country
  d$country[d$country=="Hong Kong SAR, China"] <- "Hong Kong"
  d$country[d$country=="Egypt, Arab Rep."] <- "Egypt"
  d$country[d$country=="Korea, Dem. Rep."] <- "North Korea"
  d$country[d$country=="Korea, Rep."] <- "South Korea"
  d$country[d$country=="Lao PDR"] <- "Laos"
  d$country[d$country=="Macao SAR, China"] <- "Macau"
  d$country[d$country=="Russian Federation"] <- "Russia"
  d$country[d$country=="Venezuela, RB"] <- "Venezuela"
  d$country[d$country=="Yemen, Rep."] <- "Yemen"
  d$country[d$country=="Bahamas, The"] <- "Bahamas"
  d$country[d$country=="Brunei Darussalam"] <- "Brunei"
  d$country[d$country=="Congo, Rep."] <- "Congo Republic"
  d$country[d$country=="Gambia, The"] <- "Gambia"
  d$country[d$country=="Iran, Islamic Rep."] <- "Iran"
  d$country[d$country=="Kyrgyz Republic"] <- "Kyrgyzstan"
  d$country[d$country=="Slovak Republic"] <- "Slovakia"
  d$country[d$country=="Syrian Arab Republic"] <- "Syria"
  
  # Return the cleaned data frame
  d
}

clean.ti <- function(file.name, sheet, region, names, year) { # file.name = CPI2011_Results.xls (a string)
  workbook <- loadWorkbook(file.name)
  d <- readWorksheet(workbook, sheet=sheet, region=region, header=FALSE)
  names(d) <- names
  
#   # Rename and keep variables
#   d<- rename(d, replace=c("Country...Territory"="country", 
#                                     "CPI.2011.Score"='ti',
#                                     "Standard.Deviation"="ti.std",
#                                     "X90...Confidence.Interval"="ti.90ci.min",
#                                     "Col10"="ti.90ci.max"))
#   keeps <- c("country", "ti", "ti.std", "ti.90ci.min", "ti.90ci.max",
#              "ADB", "AFDB", "WB_CPIA", "TI_BPI", "FH_NIT")
#   d <- d[, keeps]
  
  names(d)[names(d)!='country'] <- paste(names(d)[names(d)!='country'], year, sep="_")
  
  # Rename country
  d$country[d$country=="Côte d´Ivoire"] <- "Cote d'Ivoire"
  d$country[d$country=="Cote d´Ivoire"] <- "Cote d'Ivoire"
  d$country[d$country=="FYR Macedonia"] <- "Macedonia, FYR"
  d$country[d$country=="Sao Tome & Principe"] <- "Sao Tome and Principe"
  d$country[d$country=="Saint Lucia"] <- "St. Lucia"
  d$country[d$country=="Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
  d$country[d$country=="Korea (North)"] <- "North Korea"
  d$country[d$country=="Korea (South)"] <- "South Korea"
  d$country[d$country=="Samoa"] <- "American Samoa"
  d$country[d$country=="Congo  Republic"] <- "Congo Republic"
  d$country[d$country=="Democratic Republic of the Congo "] <- "Congo, Dem. Rep."
  d$country[d$country=="Congo, Democratic Republic"] <- "Congo, Dem. Rep."
  d$country[d$country=="USA"] <- "United States"
  
  # Return cleaned dataset
  d
}

clean.ti.ts <- function(file.name, sheet, startRow, endCol, names) { # file.name = CPI2011_Results.xls (a string)
  workbook <- loadWorkbook(file.name)
  d <- readWorksheet(workbook, sheet=sheet, startRow=startRow, endCol=endCol, header=FALSE)
  names(d) <- names
  
  #   # Rename and keep variables
  #   d<- rename(d, replace=c("Country...Territory"="country", 
  #                                     "CPI.2011.Score"='ti',
  #                                     "Standard.Deviation"="ti.std",
  #                                     "X90...Confidence.Interval"="ti.90ci.min",
  #                                     "Col10"="ti.90ci.max"))
  #   keeps <- c("country", "ti", "ti.std", "ti.90ci.min", "ti.90ci.max",
  #              "ADB", "AFDB", "WB_CPIA", "TI_BPI", "FH_NIT")
  #   d <- d[, keeps]
  
  # Rename country
  d$country[d$country=="Côte d´Ivoire"] <- "Cote d'Ivoire"
  d$country[d$country=="Cote d´Ivoire"] <- "Cote d'Ivoire"
  d$country[d$country=="Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
  d$country[d$country=="Brunei Darussalam"] <- "Brunei"
  d$country[d$country=="FYR Macedonia"] <- "Macedonia, FYR"
  d$country[d$country=="Sao Tome & Principe"] <- "Sao Tome and Principe"
  d$country[d$country=="Saint Lucia"] <- "St. Lucia"
  d$country[d$country=="Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
  d$country[d$country=="Korea (North)"] <- "North Korea"
  d$country[d$country=="Korea (South)"] <- "South Korea"
  d$country[d$country=="Samoa"] <- "American Samoa"
  d$country[d$country=="Congo  Republic"] <- "Congo Republic"
  d$country[d$country=="Democratic Republic of the Congo "] <- "Congo, Dem. Rep."
  d$country[d$country=="Congo, Democratic Republic"] <- "Congo, Dem. Rep."
  d$country[d$country=="USA"] <- "United States"
  
  # Return cleaned dataset
  d
}

#### Dummy for ADB member ####
f <- readLines("test.txt")

keep.name <- function(s) {
  unlist(strsplit(s, split=" \\(" ))[1]
}

ADB <- unname(sapply(f, keep.name))
rm(f)

ADB[ADB=="Hong Kong, China"] <- "Hong Kong"
ADB[ADB=="Korea, Dem. Rep."] <- "North Korea"
ADB[ADB=="Korea, Republic of"] <- "South Korea"
ADB[ADB=="Lao People's Democratic Republic"] <- "Laos"
ADB[ADB=="Macao SAR, China"] <- "Macau"
ADB[ADB=="Russian Federation"] <- "Russia"
ADB[ADB=="Brunei Darussalam"] <- "Brunei"
ADB[ADB=="Kyrgyz Republic"] <- "Kyrgyzstan"
ADB[ADB=="Viet Nam, Socialist Republic of "] <- "Vietnam"
ADB[ADB=="Marshall Islands, Republic of the"] <- "Marshall Islands"
ADB[ADB=="Taipei,China"] <- "Taiwan"
ADB[ADB=="China, People's Republic of"] <- "China"

in.ADB <- function(v) { # Input is a vector of string (country names)
  as.numeric(v %in% ADB)
}

save.image("clean_fun.RData")