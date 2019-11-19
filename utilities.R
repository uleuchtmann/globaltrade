################################################################################

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

################################################################################

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

################################################################################

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

################################################################################

dot2panel <- function(countries, counterparts = NULL, freq = c("A", "Q", "M"),
                      indicator = c("exp", "imp", "bal"), 
                      startDate = "1960-01-01", endDate=NULL) {
  freq <- match.arg(freq)
  indicator <- match.arg(indicator)
  freqs <- c("A" = "year", "Q" = "quarter", "M" = "month")
  indicators <- c("imp" = "TMG_CIF_USD", "exp" = "TXG_FOB_USD", "bal" = "TBG_USD")
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

###############################################################################
