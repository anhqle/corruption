library(foreign)
library(plyr)
library(XLConnect)
library(stringr)
library(reshape2)

clean.wb <- function(d) {
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
  
#   names(d)[4:length(names(d))] <- paste(names(d)[4:length(names(d))], year, sep="_")
  
  # Return the cleaned data frame
  d
}

clean.ti <- function(file.name, sheet, startRow, endCol, names) { # file.name = CPI2011_Results.xls (a string)
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

clean.name <- function(df) { # the data frame has a column name country
  df$country[df$country=="Argenti"] <- "Argentina"
  df$country[df$country=="Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
  df$country[df$country=="Bosnia-herzegovi"] <- "Bosnia and Herzegovina"
  df$country[df$country=="Burki Faso"] <- "Burkina Faso"
  df$country[df$country=="Bahamas, The"] <- "Bahamas"
  df$country[df$country=="Brunei darussalam"] <- "Brunei"
  df$country[df$country=="Brunei Darussalam"] <- "Brunei"
  df$country[df$country=="Congo, Rep."] <- "Congo Republic"
  df$country[df$country=="Congo  Republic"] <- "Congo Republic"
  df$country[df$country=="Congo, dfemocratic Republic"] <- "Congo, Dem. Rep."
  df$country[df$country=="Congo, Dem. Rep. (zaire)"] <- "Congo, Dem. Rep."
  df$country[df$country=="Democratic Republic of the Congo "] <- "Congo, Dem. Rep."
  df$country[df$country=="Côte d´Ivoire"] <- "Cote d'Ivoire"
  df$country[df$country=="Cote d´Ivoire"] <- "Cote d'Ivoire"
  df$country[df$country=="Gha"] <- "Ghana"
  df$country[df$country=="Hong Kong SAR, China"] <- "Hong Kong"
  df$country[df$country=="Egypt, Arab Rep."] <- "Egypt"
  df$country[df$country=="Timor, East"] <- "East Timor"
  df$country[df$country=="Lao PDR"] <- "Laos"
  df$country[df$country=="Macao"] <- "Macau"
  df$country[df$country=="Macao SAR, China"] <- "Macau"
  df$country[df$country=="Russian Fedferation"] <- "Russia"
  df$country[df$country=="Gambia, The"] <- "Gambia"
  df$country[df$country=="Iran, Islamic Rep."] <- "Iran"
  df$country[df$country=="Kyrgyz Republic"] <- "Kyrgyzstan"
  df$country[df$country=="Slovak Republic"] <- "Slovakia"
  df$country[df$country=="Syrian Arab Republic"] <- "Syria"
  df$country[df$country=="FYR Macedfonia"] <- "Macedfonia, FYR"
  df$country[df$country=="Sao Tome & Principe"] <- "Sao Tome and Principe"
  df$country[df$country=="Saint Lucia"] <- "St. Lucia"
  df$country[df$country=="Saint Vincent and the Grenadfines"] <- "St. Vincent and the Grenadfines"
  df$country[df$country=="Korea (North)"] <- "North Korea"
  df$country[df$country=="Korea (South)"] <- "South Korea"
  df$country[df$country=="Korea, Dem. Rep."] <- "North Korea"
  df$country[df$country=="Korea, Rep."] <- "South Korea"
  df$country[df$country=="Samoa"] <- "American Samoa"
  df$country[df$country=="USA"] <- "Unitedf States"
  df$country[df$country=="Venezuela, RB"] <- "Venezuela"
  df$country[df$country=="Yemen, Rep."] <- "Yemen"
  df$country[df$country=="Vietm"] <- "Vietnam"
  return(df)
}

f <- readLines("ADBcountry.txt")
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

add.year <- function(tixx, prefix) {
  year <- substr(deparse(substitute(tixx)), 3, 4)
  full_year <- paste(prefix, year, sep='')
  tixx <- cbind(tixx, year=rep(full_year, nrow(tixx)))
  tixx
}