library(rdbnomics)
source("utilities.R")

data <- weo2panel(countries, c("NGDPD","NGDPRPPPPC"))

size <- data[data[,"period"]=="2018-01-01", "NGDPD"]
names(size) <- data[data[,"period"]=="2018-01-01", "country"]     
names(sort(size, decreasing=TRUE)[1:100])

NGDPD <- weo2series(countries, "NGDPD")


