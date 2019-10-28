weo2panel <- function(countries, subjects, startDate="1980-01-01", endDate="2019-12-31") {
  data1 <- as.data.frame(rdb("IMF", "WEO", 
                             dimensions = list("weo-country" = countries, 
                                               "weo-subject" = subjects)))
  data2 <- data1[data1[ , "weo-subject"] == subjects[1], c("period", "weo-country", "value")]
  if (length(subjects)>1) {
    for (s in 2:length(subjects)) {
      data2 <- cbind(data2, data1[data1[ , "weo-subject"] == subjects[s], "value"])
    }
  }
  names(data2) <- c("period", "country", subjects)
  data3 <- data2[data2[ , "period"] >= startDate, ]
  data4 <- data3[data3[ , "period"] <= endDate, ]
  return(data4)
}

ifs2panel <- function(areas, subjects, freq="M", startDate="1980-01-01", endDate="2019-12-31") {
  data1 <- as.data.frame(rdb("IMF", "IFS", 
                             dimensions = list("REF_AREA" = areas, 
                                               "INDICATOR" = subjects,
                                               "FREQ" = freq)))   
  data2 <- data1[data1[ , "INDICATOR"] == subjects[1], c("period", "REF_AREA", "value")]
  if (length(subjects)>1) {
    for (s in 2:length(subjects)) {
      data2 <- cbind(data2, data1[data1[ , "INDICATOR"] == subjects[s], "value"])
    }
  }
  names(data2) <- c("period", "area", subjects)
  data3 <- data2[data2[ , "period"] >= startDate, ]
  data4 <- data3[data3[ , "period"] <= endDate, ]
  return(data4)
}

foo <- ifs2panel(c("BR", "RU", "IN", "CN", "ZA", "TR", "MX"), "FIDR_PA", freq="M", 
                 startDate = "2019-01-01")


foo <- as.data.frame(rdb("IMF", "IFS", dimensions = list("REF_AREA" = "RU",
                                           "FREQ" = "M")))
sort(unique(foo[,"INDICATOR"]))

weo2frame <- function(countries, subject, startDate="1980-01-01", endDate="2019-12-31") {
  data1 <- as.data.frame(rdb("IMF", "WEO", 
                             dimensions = list("weo-country" = countries, 
                                               "weo-subject" = subject)))
  data2 <- as.data.frame(data1[data1[, "weo-country"]==countries[1], "value"])
  row.names(data2) <- data1[data1[, "weo-country"]==countries[1], "period"]
  if (length(countries)>1) {
    for (i in 2:length(countries)) {
      data2 <- cbind(data2, data1[data1[ , "weo-country"]==countries[i], "value"])
    }
  }
  names(data2) <- countries
  data3 <- data2[row.names(data2) >= startDate, ]
  data4 <- data3[row.names(data3) <= endDate, ]
  return(data4)
}

countries.weo <- c("AFG","ALB","DZA","AGO","ATG","ARG","ARM","ABW","AUS","AUT",
                   "AZE","BHR","BGD","BRB","BLR","BEL","BLZ","BEN","BTN","BOL",
                   "BIH","BWA","BRA","BRN","BGR","BFA","BDI","CPV","KHM","CMR",
                   "CAN","CAF","TCD","CHL","CHN","COL","COM","CRI","CIV","HRV",
                   "CYP","CZE","COD","DNK","DJI","DMA","DOM","ECU","EGY","SLV",
                   "GNQ","ERI","EST","SWZ","ETH","FJI","FIN","FRA","GAB","GEO",
                   "DEU","GHA","GRC","GRD","GTM","GIN","GNB","GUY","HTI","HND",
                   "HKG","HUN","ISL","IND","IDN","IRQ","IRL","IRN","ISR","ITA",
                   "JAM","JPN","JOR","KAZ","KEN","KIR","KOR","UVK","KWT","KGZ",
                   "LAO","LVA","LBN","LSO","LBR","LBY","LTU","LUX","MAC","MDG",
                   "MWI","MYS","MDV","MLI","MLT","MHL","MRT","MUS","MEX","FSM",
                   "MDA","MNG","MNE","MAR","MOZ","MMR","NAM","NRU","NPL","NLD",
                   "NZL","NIC","NER","NGA","MKD","NOR","OMN","PAK","PLW","PAN",
                   "PNG","PRY","PER","PHL","POL","PRT","PRI","QAT","COG","ROU",
                   "RUS","RWA","WSM","SMR","STP","SAU","SEN","SRB","SYC","SLE",
                   "SGP","SVK","SVN","SLB","SOM","ZAF","SSD","ESP","LKA","KNA",
                   "LCA","VCT","SDN","SUR","SWE","CHE","SYR","TWN","TJK","TZA",
                   "THA","BHS","GMB","TLS","TGO","TON","TTO","TUN","TUR","TKM",
                   "TUV","UGA","UKR","ARE","GBR","USA","URY","UZB","VUT","VEN",
                   "VNM","YEM","ZMB","ZWE")


