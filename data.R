library(rdbnomics)
source("utilities.R")
countries <- read.csv("countriesDOT.csv", na.strings ="#N/A", stringsAsFactors = FALSE)

# countries with WEO data (EMU as aggregate)
countries[(countries$is_country & !is.na(countries$weocode) & !countries$euro_area) | 
          (!is.na(countries$weocode) & countries$weocode == "163"),
          c("dotcode", "weocode")]
the.regions <- countries[(countries$is_country & !is.na(countries$weocode) & !countries$euro_area) | 
                               (!is.na(countries$weocode) & countries$weocode == "163"),
                         c("dotcode", "weocode")]

# 2018 GDP data from WEO
gdp <- weo2panel(the.regions[,"weocode"], "NGDPD", "2018-01-01", "2018-01-01")

# 2018 total import and export data from DOT
imports <- dot2panel(the.regions.dot, "W00", freq = "A", indicator = "imp", startDate = "2018-01-01", endDate = "2018-01-01")
exports <- dot2panel(the.regions.dot, "W00", freq = "A", indicator = "exp", startDate = "2018-01-01", endDate = "2018-01-01")



