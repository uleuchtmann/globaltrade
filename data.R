library(rdbnomics)
source("utilities.R")
countries <- read.csv("countriesDOT.csv", na.strings ="#N/A", stringsAsFactors = FALSE)

# test project: GDP scaling of Japan's exports to EMU
countries[countries$is_country & countries$euro_area, "dotcode"]
countries[countries$is_country & countries$euro_area, "weocode"]

jp.exports.emu <- dot2panel("JP", countries[countries$is_country & countries$euro_area, "dotcode"],
                            indicator = "exp", freq ="A", 
                            startDate = "2018-01-01", endDate = "2018-01-01")

emu.imports.jp <- dot2panel(countries[countries$is_country & countries$euro_area, "dotcode"], "JP",
                            indicator = "imp", freq = "A",
                            startDate = "2018-01-01", endDate = "2018-01-01")


emu.gdp <- weo2panel(countries[countries$is_country & countries$euro_area, "weocode"], 
                     "NGDP", startDate = "2018-01-01", endDate = "2018-01-01")

jp.exp <- merge(emu.gdp, merge(jp.exports.emu, countries[,c("dotcode", "weocode")], 
                              by.x = "destination", by.y = "dotcode", all.x = TRUE)[,c("weocode", "value")], 
                by.x = "country", by.y = "weocode", all = TRUE)[, c("country", "NGDP", "value")]
names(jp.exp) <- c("country", "GDP", "exports")


emu.imp <- merge(emu.gdp, merge(emu.imports.jp, countries[,c("dotcode", "weocode")], 
                                by.x = "origin", by.y = "dotcode", all.x = TRUE)[,c("weocode", "value")],
                 by.x = "country", by.y = "weocode", all = TRUE)[,c("country", "NGDP", "value")]
names(emu.imp) <- c("country", "GDP", "imports")


plot(exports ~ GDP, data = jp.exp)          
with(jp.exp, text(exports ~ GDP, labels = country, pos = 4))         

plot(imports ~ GDP, data = emu.imp)          
with(emu.imp, text(imports ~ GDP, labels = country, pos = 4))         


