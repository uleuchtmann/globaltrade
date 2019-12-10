library(rdbnomics)
source("utilities.R")
regions <- read.csv("countriesDOT.csv", na.strings ="#N/A", stringsAsFactors = FALSE)

# countries with WEO data 
countries <- regions[regions$is_country & !is.na(regions$weocode), ] 

# load total imports and total exports 2018
imports.wld <- dot2panel(countries[,"dotcode"], "W00", freq = "A", indicator = "imp", startDate = "2018-01-01", endDate = "2018-01-01")
exports.wld <- dot2panel(countries[,"dotcode"], "W00", freq = "A", indicator = "exp", startDate = "2018-01-01", endDate = "2018-01-01")
total.imp <- dot2panel("W00", "W00", freq = "A", indicator = "imp", startDate = "2018-01-01", endDate = "2018-01-01")$value
total.exp <- dot2panel("W00", "W00", freq = "A", indicator = "exp", startDate = "2018-01-01", endDate = "2018-01-01")$value

# the largest trading countries
trade.wld <- cbind(imports.wld, exports.wld$value)
names(trade.wld) <- c("origin", "destination", "period", "imports", "exports")
trade.wld$trade <- trade.wld$imports + trade.wld$exports
trade.wld$share <- trade.wld$trade / (total.exp + total.imp) 
trade.wld <- trade.wld[order(-trade.wld$trade),]
sum(trade.wld[1:42,]$share)

# the largest trading countries + EMU
the.countries <- union(trade.wld[1:42,]$origin, countries[countries$euro_area,]$dotcode)

# load import and export data
imports.cty <- dot2panel(the.countries, freq = "A", indicator = "imp", startDate = "2018-01-01", endDate = "2018-01-01")
exports.cty <- dot2panel(the.countries, freq = "A", indicator = "exp", startDate = "2018-01-01", endDate = "2018-01-01")

# aggregates
# EMU
imports.emu <- dot.agg(imports.cty, countries[countries$euro_area,]$dotcode, "EMU")
exports.emu <- dot.agg(exports.cty, countries[countries$euro_area,]$dotcode, "EMU")
# China + Hong Kong + Macao
imports.cnx <- dotn.agg(imports.emu, c("CN", "HK", "MO"), "CNX")
exports.cnx <- dot.agg(exports.emu, c("CN", "HK", "MO"), "CNX")
# fact about the data set
unique(imports.cnx$origin)
(sum(imports.cnx$value) + sum(exports.cnx$value)) / (total.imp + total.exp)

# combine import data + export data
trade <- exports.cnx
import.col <- rep(0, nrow(trade))
for (i in 1:nrow(trade)) {
  import.col[i] <- imports.cnx[(imports.cnx$origin == trade[i,]$destination) & 
                               (imports.cnx$destination == trade[i,]$origin) & 
                               (imports.cnx$period == trade[i,]$period),]$value
}
trade$from.imports <- import.col
names(trade) <- c("origin", "destination", "period", "exports", "imports")
trade$flow <- sqrt(trade$from.exports * trade$from.imports)

# load GDP data
countries[countries$dotcode %in% the.countries,]$weocode
gdp.cty <- weo2panel(countries[countries$dotcode %in% the.countries,]$weocode, "NGDPD", "2018-01-01", "2018-01-01")
gdp.cty <- merge(gdp.cty, countries, by.x = "region", by.y = "weocode", all.x = TRUE)[,c("dotcode", "period", "NGDPD")]
names(gdp.cty) <- c("region", "period", "value")

# aggregates
# EMU
baz <- gdp.cty
agg.region <- countries[countries$euro_area,]$dotcode
agg.region.name <- "EMU"
for (theperiod in unique(format(baz$period))) {
  therow <- data.frame(agg.region.name, as.Date(theperiod),
                       sum(baz[(baz$period == theperiod) & (baz$region %in% agg.region), ]$value)
                      )
  names(therow) <- names(baz)
  baz <- rbind(baz, therow)
}
gdp.emu <- baz[!(baz$region %in% agg.region), ]
# China + Hong Kong + Macao
baz <- gdp.emu
agg.region <- c("CN", "HK", "MO")
agg.region.name <- "CNX"
for (theperiod in unique(format(baz$period))) {
  therow <- data.frame(agg.region.name, as.Date(theperiod),
                       sum(baz[(baz$period == theperiod) & (baz$region %in% agg.region), ]$value)
  )
  names(therow) <- names(baz)
  baz <- rbind(baz, therow)
}
gdp.cnx <- baz[!(baz$region %in% agg.region), ]

# add GDP data (converted to $ mln) to trade
gdp.origin <- rep(0, nrow(trade))
gdp.destination <- rep(0, nrow(trade))
for (i in 1:nrow(trade)) {
  gdp.origin[i]      <- 10^3 * gdp.cnx[(gdp.cnx$period == trade[i,]$period) & (gdp.cnx$region == trade[i,]$origin), ]$value
  gdp.destination[i] <- 10^3 * gdp.cnx[(gdp.cnx$period == trade[i,]$period) & (gdp.cnx$region == trade[i,]$destination), ]$value
}
trade$gdp.origin <- gdp.origin
trade$gdp.destination <- gdp.destination

# add a dummy distance variable
trade$distance <- rep(1, nrow(trade))

