library(rdbnomics)
library(xts)
library(countrycode)
source("utilities.R")
countries <- read.csv("countriesDOT.csv", na.strings ="#N/A", stringsAsFactors = FALSE)

# test project: GDP scaling of Japan's exports to EMU
countries[countries$is_country & countries$euro_area, "dotcode"]
countries[countries$is_country & countries$euro_area, "weocode"]

jp.exports.emu <- dot2panel("JP", countries[countries$is_country & countries$euro_area, "dotcode"],
                            indicator = "exp", freq ="A", 
                            startDate = "2018-01-01", endDate = "2018-01-01")

emu.gdp <- weo2panel(countries[countries$is_country & countries$euro_area, "weocode"], 
                     "NGDP", startDate = "2018-01-01", endDate = "2018-01-01")

jp.exp <- merge(emu.gdp, merge(jp.exports.emu, countries[,c("dotcode", "weocode")], 
                              by.x = "destination", by.y = "dotcode", all.x = TRUE)[,c("weocode", "value")], 
                by.x = "country", by.y = "weocode", all = TRUE)[, c("country", "NGDP", "value")]
names(jp.exp) <- c("country", "GDP", "exports")

plot(exports ~ GDP, data = jp.exp)          
with(jp.exp, text(exports ~ GDP, labels = country, pos = 4))         
          