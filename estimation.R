library(gravity)
load("trade.RData")

names(trade) 

gravity.ols <- ols(dependent_variable = "flow", income_origin = "gdp.origin", income_destination = "gdp.destination", 
                   dist = "distance", code_origin = "origin", code_destination = "destination", data = trade)
residuals(gravity.ols)

gravity.bvu <- bvu("flow", "distance", income_origin = "gdp.origin", income_destination = "gdp.destination", 
                  code_origin = "origin", code_destination = "destination", data = trade)
residuals(gravity.bvu)


sils("flow", "distance", income_origin = "gdp.origin", income_destination = "gdp.destination", additional_regressors = "from.exports",
     code_origin = "origin", code_destination = "destination", data = trade, verbose= TRUE)

