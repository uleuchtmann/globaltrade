library(rdbnomics)
library(xts)
library(countrycode)
library(readr)
countries <- read_csv("countriesDOT.csv", na="#N/A")


dot2panel <- function(countries, counterparts = NULL, freq = c("A", "Q", "M"),
                      indicator = c("exp", "imp", "bal"), 
                      startDate = "1960-01-01", endDate=NULL) {
  freq <- match.arg(freq)
  indicator <- match.arg(indicator)
  freqs <- c("A" = "year", "Q" = "quarter", "M" = "month")
  indicators <- c("imp" = "TMG_FOB_USD", "exp" = "TXG_FOB_USD", "bal" = "TBG_USD")
  if (is.null(counterparts)) counterparts <- countries
  if (is.null(endDate)) endDate <- Sys.Date()
  the.dim <- list("FREQ" = freq, "INDICATOR" = as.character(indicators[indicator]),
                  "REF_AREA" = countries, "COUNTERPART_AREA" = counterparts)
  foo <- rdb("IMF", "DOT", dimensions = the.dim)
  bar <- foo[foo$period >= startDate & foo$period <= endDate, 
             c("REF_AREA", "COUNTERPART_AREA", "period", "value")]
  names(bar) <- c("origin", "destination", "period", "value")
  return(bar)
}

the.countries <- c("W00", "US", "CN")
baz <- dot2panel(the.countries, startDate = "1999-01-01", endDate = "2010-01-01")

seq(as.Date("2018-01-01"), Sys.Date(), "month")


data.frame(NULL)


baz$period[288] >= as.Date("1960-01-01")

