library(googleVis)
library(RJSONIO)
url = "http://api.worldbank.org/countries/all/indicators/IT.NET.USER.P3?date=1990:2010&format=json&per_page=12000"
iu = fromJSON(url)
internetUsers = data.frame(year = as.numeric(sapply(iu[[2]], "[[", "date")),
                           InternetUsersPerThousands = as.numeric(sapply(iu[[2]],
                                                                         function(x) ifelse(is.null(x[["value"]]),  NA, x[["value"]]))),
                           country = sapply(iu[[2]], function(x)
                             x[["country"]]['value']))

## Create a line plot with gvisMotionChart
## Set initial state with a few regions selected and a log y-axes
myState <- '
{"yZoomedIn":false,"yZoomedDataMin":0,"xAxisOption":"_TIME",
 "xZoomedDataMax":1230768000000,"time":"2009","playDuration":15000,
 "yAxisOption":"2","yLambda":0,"duration":{"timeUnit":"Y","multiplier":1},
 "xZoomedDataMin":631152000000,"uniColorForNonSelected":false,
 "showTrails":false,"orderedByY":false,"nonSelectedAlpha":0,
 "orderedByX":false,"sizeOption":"_UNISIZE","xLambda":1,
 "colorOption":"_UNIQUE_COLOR",
 "iconKeySettings":[{"key":{"dim0":"Sub-Saharan Africa (all income levels)"}},
                    {"key":{"dim0":"North Africa"}},
                    {"key":{"dim0":"Seychelles"}},
                    {"key":{"dim0":"Sierra Leone"}},
                    {"key":{"dim0":"Africa"}}],
 "dimensions":{"iconDimensions":["dim0"]},
 "yZoomedDataMax":220,"xZoomedIn":false,"iconType":"LINE"}
'
M <- gvisMotionChart(internetUsers, "country", "year", 
                     options=list(width=650, height=350, state=myState))
plot(M)
