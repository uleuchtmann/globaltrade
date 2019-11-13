library(rdbnomics)
library(xts)
library(countrycode)
library(tidyverse)

W00	

countriesDOT <- c("AF","AL","DZ","AS","AO","AI","AG","AR","AM","AW","AU","AT",
                  "AZ","BS","BH","BD","BB","BY","BE","BZ","BJ","BM","BT","BO",
                  "BA","BW","BR","BN","BG","BF","BI","KH","CM","CA","CV","CF",
                  "TD","CL","CN","CO","KM","CG","CD","CR","CI","HR","CU","CW",
                  "CY","CZ","DK","DJ","DM","DO","EC","EG","SV","GQ","ER","EE",
                  "SZ","ET","FK","FO","FJ","FI","FR","PF","GA","GM","GE","DE",
                  "GH","GI","GR","GL","GD","GU","GT","GN","GW","GY","HT","VA",
                  "HN","HK","HU","IS","IN","ID","IR","IQ","IE","IL","IT","JM",
                  "JP","JO","KZ","KE","KI","KP","KR","XK","KW","KG","LA","LV",
                  "LB","LS","LR","LY","LT","LU","MO","MG","MW","MY","MV","ML",
                  "MT","MH","MR","MU","MX","FM","MD","MN","ME","MS","MA","MZ",
                  "MM","NA","NR","NP","NL","AN","NC","NZ","NI","NE","NG","MK",
                  "NO","OM","PK","PW","PS","PA","PG","PY","PE","PH","PL","PT",
                  "QA","RO","RU","RW","KN","LC","VC","WS","SM","ST","SA","SN",
                  "RS","CS","SC","SL","SG","SX","SK","SI","SB","SO","ZA","SS",
                  "ES","LK","SD","SR","SE","CH","SY","TJ","TZ","TH","TL","TG",
                  "TO","TT","TN","TR","TM","TV","UG","UA","AE","GB","US","UY",
                  "UZ","VU","VE","VN","YE","ZM","ZW")

countriesWEO <- c("AFG","ALB","DZA","AGO","ATG","ARG","ARM","ABW","AUS","AUT",
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

countrycode(countriesDOT, "iso2c", "imf")

countrycode(countriesWEO, "iso3c", "country.name")

rdb("IMF", "WEO", dimensions = list("weo-country" = c("USA", "SRB"), 
                                    "weo-subject" = "NGDPD"))

rdb("IMF", "DOT", dimensions = list("REF_AREA" = c("US", "RS"), 
                                    "COUNTERPART_AREA" = c("US", "RS"), 
                                    "FREQ" = "A", 
                                    "INDICATOR" = "TXG_FOB_USD"))

countrycode(countriesWEO, "iso3c", "iso2c") %in% countriesDOT
countrycode(countriesWEO[88], "iso3c", "country.name")
countrycode("ALB", "iso3c", "country.name", )

as_tibble(iris)

