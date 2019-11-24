library(rdbnomics)
source("utilities.R")
countries <- read.csv("countriesDOT.csv", na.strings ="#N/A", stringsAsFactors = FALSE)

# for which countries do we have WEO data?
countries[countries$is_country & !is.na(countries$weocode), c("dotcode", "weocode")]

regions <- c("USA", "CHN", "001", "163")
regions <- c("USA", "CHN")
regions <- c("001", "163")
subjects <- c("NGDPD", "NGDP_RPCH")



if (any(substr(regions, 1, 1) %in% LETTERS)) {
  cou <- regions[substr(regions, 1, 1) %in% LETTERS]
  cou.dat <- as.data.frame(rdb("IMF", "WEO", 
                               dimensions = list("weo-country" = cou, 
                                                 "weo-subject" = subjects
                                                )
                              )
                          )[,c("period", "weo-country", "weo-subject", "value")]
  names(cou.dat) <- c("period", "country", "subject", "value")
}
if (any(!(substr(regions, 1, 1) %in% LETTERS))) {
  agg <- regions[!(substr(regions, 1, 1) %in% LETTERS)]
  agg.dat <- as.data.frame(rdb("IMF", "WEOAGG", 
                               dimensions = list("weo-countries-group" = agg, 
                                                 "weo-subject" = subjects
                                                )
                              )
                          )[,c("period", "weo-country", "weo-subject", "value")]
  names(agg.dat) <- c("period", "country", "subject", "value")
}
dat <- rbind

!any(substr(regions, 1, 1) %in% LETTERS)

