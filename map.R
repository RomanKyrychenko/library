library(googleVis)

Geo=gvisGeoChart(gci_rating, locationvar="Global Competitiveness Index", 
                 colorvar="Середні коливання індексу 2006-2016", 
                 options=list(projection="kavrayskiy-vii",
                              colorAxis="{colors:['#a50026', '#ffffbf','#006837']}",
                              backgroundColor="lightblue",
                              width=1000, 
                              height=400,
                              titleTextStyle="{color:'black', fontName:'Helvetica', fontSize:16}",
                              dataMode="regions"))
plot(Geo)

rating <- gci_rating[,1:22]

n_rating <- as_data_frame(t(rating))

n_rating$Year <- c("Country",2006:2016,2007:2016)




library(reshape2)
a2006 <- melt(rating, "Global Competitiveness Index","2006-2007")
a2007 <- melt(rating, "Global Competitiveness Index","2007-2008")
a2008 <- melt(rating, "Global Competitiveness Index","2008-2009")
a2009 <- melt(rating, "Global Competitiveness Index","2009-2010")
a2010 <- melt(rating, "Global Competitiveness Index","2010-2011")
a2011 <- melt(rating, "Global Competitiveness Index","2011-2012")
a2012 <- melt(rating, "Global Competitiveness Index","2012-2013")
a2013 <- melt(rating, "Global Competitiveness Index","2013-2014")
a2014 <- melt(rating, "Global Competitiveness Index","2014-2015")
a2015 <- melt(rating, "Global Competitiveness Index","2015-2016")
a2016 <- melt(rating, "Global Competitiveness Index","2016-2017")

b2006 <- melt(rating, "Global Competitiveness Index","2006-2007_1")
b2007 <- melt(rating, "Global Competitiveness Index","2007-2008_1")
b2008 <- melt(rating, "Global Competitiveness Index","2008-2009_1")
b2009 <- melt(rating, "Global Competitiveness Index","2009-2010_1")
b2010 <- melt(rating, "Global Competitiveness Index","2010-2011_1")
b2011 <- melt(rating, "Global Competitiveness Index","2011-2012_1")
b2012 <- melt(rating, "Global Competitiveness Index","2012-2013_1")
b2013 <- melt(rating, "Global Competitiveness Index","2013-2014_1")
b2014 <- melt(rating, "Global Competitiveness Index","2014-2015_1")
b2015 <- melt(rating, "Global Competitiveness Index","2015-2016_1")

a <- rbind(a2006,a2007,a2008,a2009,a2010,a2011,a2012,a2013,a2014,a2015,a2016)
b <- rbind(b2006,b2007,b2008,b2009,b2010,b2011,b2012,b2013,b2014,b2015)

b$year <-  ifelse(b$variable=="2006-2007_1",  2007, ifelse(b$variable=="2007-2008_1",2008,ifelse(b$variable=="2008-2009_1",
       2009,ifelse(b$variable=="2009-2010_1",2010,ifelse(b$variable=="2010-2011_1",2011,ifelse(b$variable=="2011-2012_1",
       2012,ifelse(b$variable=="2012-2013_1",2013,ifelse(b$variable=="2013-2014_1",2014,ifelse(b$variable=="2014-2015_1",
       2015,2016))))))))) 

a$year <-  ifelse(a$variable=="2006-2007",  2006, ifelse(a$variable=="2007-2008",2007,
                                                         ifelse(a$variable=="2008-2009",2008,
                                                                ifelse(a$variable=="2009-2010",2009,
                                                                       ifelse(a$variable=="2010-2011",2010,
                                                                              ifelse(a$variable=="2011-2012",2011,
                                                                                     ifelse(a$variable=="2012-2013",2012,
                                                                                            ifelse(a$variable=="2013-2014",2013,
                                                                                                   ifelse(a$variable=="2014-2015",2014,
                                                                                                          ifelse(a$variable=="2015-2016",2015,2016))))))))))
                                                                                                          
                                                                                                                                                                                                                                                                                



b <- b[c(1,3,4)]
a <- a[c(1,3,4)]
rename(b, "change"="value")

b$change <- b$value

c <- inner_join(a,b,by=c("Global Competitiveness Index","year"))

c$color <- ifelse(c$`Global Competitiveness Index`=="Ukraine", "Ukraine", "World")
c$rating <- c$change*(-1)

index <- as_data_frame(c)
index <- index %>% dplyr::rename("Країна"=`Global Competitiveness Index`)
index <- index %>% dplyr::rename("Динаміка"=`color`)
index <- index %>% dplyr::rename("Зміна позиції в рейтингу"=rating)
index <- index %>% dplyr::rename("Місце"=value)
index <- index %>% dplyr::rename("Рік"=year)
index <- index %>% dplyr::rename("Індекс"=index)

c$index <- abs(c$value-max(c$value, na.rm = T)-1)
max(index$`Позиція в рейтингу`, na.rm = T)

ind <- data.frame(index)

motion <- gvisMotionChart(c, idvar='Global Competitiveness Index', 
                timevar='year', 
                colorvar ='color', 
                xvar = 'value', 
                yvar = 'rating', 
                sizevar='index', 
                options=list(width=600, height=300), 
                chartid="Global Competitiveness Index")
plot(motion)

Bubble <- gvisBubbleChart(gci_rating, idvar="Global Competitiveness Index", xvar="Score1", 
                          yvar="Середні коливання індексу 2006-2016", colorvar="Країна", sizevar="GDP per Capita 2015 current $",
                          options=list(hAxis='{minValue:2.5, maxValue:5}', width=1100, height=800))
plot(Bubble)

d <- gci_rating[c(1,29)]

c <- inner_join(c,d, by='Global Competitiveness Index')

c$color <- ifelse(c$`Global Competitiveness Index`=="Ukraine", "Україна", 
                  ifelse(c$`Середні коливання індексу 2006-2016` < -1, "Негативна тенденція", 
                         ifelse(c$`Середні коливання індексу 2006-2016`> 1, "Позитивна тенденція","Стабільні показники")))

mytate <- '
{"yZoomedIn":false,"yZoomedDataMin":0,"xAxisOption":"_TIME",
 "xZoomedDataMax":1230768000000,"time":"2009","playDuration":15000,
 "yAxisOption":"2","yLambda":0,"duration":{"timeUnit":"Y","multiplier":1},
 "xZoomedDataMin":631152000000,"uniColorForNonSelected":true,
 "showTrails":false,"orderedByY":false,"nonSelectedAlpha":0,
 "orderedByX":false,"sizeOption":"_UNISIZE","xLambda":1,
 "colorOption":"_UNIQUE_COLOR",
 "iconKeySettings":[{"key":{"dim0":"Ukraine"}}],
 "dimensions":{"iconDimensions":["dim0"]},
 "yZoomedDataMax":220,"xZoomedIn":false,"iconType":"LINE"}
'

motion <- gvisMotionChart(c, idvar='Global Competitiveness Index',
                          timevar='year',
                          xvar = 'value',
                          yvar = 'rating',
                          sizevar='index',
                          options=list(
                            width=650, height=350, state=  
                            '{"iconKeySettings":[{"key":{"dim0":"Ukraine"}}],
                              "stateVersion":1,
                              "time":"notime",
                              "xAxisOption":"_NOTHING",
                              "playDuration":15,
                              "iconType":"BUBBLE",
                              "sizeOption":"_NOTHING",
                              "xZoomedDataMin":null,
                              "xZoomedIn":false,
                              "duration":{"multiplier":1,"timeUnit":"none"},
                              "yZoomedDataMin":null,
                              "xLambda":1,
                              "colorOption":"_NOTHING",
                              "nonSelectedAlpha":0.4,
                              "dimensions":{"iconDimensions":["dim0"]},
                              "yZoomedIn":false,
                              "yAxisOption":"_NOTHING",
                              "yLambda":1,
                              "yZoomedDataMax":null,
                              "showTrails":true,
                              "xZoomedDataMax":null};',
                            showXMetricPicker=T,
                            showYMetricPicker=T,
                            showXScalePicker=F,
                            showYScalePicker=F,
                            showAdvancedPanel=F,
                            showSidePanel=T,
                            showSelectListComponent=F,
                            showHeader=F)) 
h <- plot(motion)

options =

