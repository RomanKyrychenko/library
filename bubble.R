gci <- read_csv("GCI.csv")
rat <- read_csv("rating.csv")
gci_rating <- inner_join(rat, gci, by = c("Global Competitiveness Index"="Economy"))

gci3 <- read_csv("GCI3.csv")


rat <- mutate(rat, "Середні коливання індексу 2006-2016"=Среднее*(-1))

gci_rating <- gci_rating %>% mutate("Країна"=ifelse("2016-2017"=="85", "Україна", "Інші країни"))
gci_rating$Країна[27] <- "Україна"

ggplot(rat, aes(`Global Competitiveness Index`, `Середні коливання індексу 2006-2016`)) + geom_bar(stat="identity")

h1 <- hPlot(x = "2016-2017", y = "Середні коливання індексу 2006-2016", size = "Score1", data = rat, type = c( 
                                                                      "bubble", "scatter"))
h1$print("chart5")

h1 <- hPlot("2016-2017", "Середні коливання індексу 2006-2016", size = "Score1", data = rat, type = "bubble")

gci_rating[,20] <- as.numeric(as.character(gci_rating[,20]))

gci_rating$`Середні коливання індексу 2014-2016` <- mean(as.numeric(as.character(gci_rating[,20:22])),na.rm=T)*(-1)

GCIs <- inner_join(gci_rating,gci3, by=c("Global Competitiveness Index"="Edition"))

GCIs$changes <- GCIs$`Cередня зміна позиції за 3 роки`*(-1)

Bubble <- gvisBubbleChart(GCIs, idvar="Global Competitiveness Index", 
                          xvar="2016-2017", 
                          yvar="changes", 
                          colorvar="Країна", 
                          sizevar="Score1",
                          options=list(hAxis='{title : "Позиція в рейтингу за 2016-2017 роки"}', 
                                       width=1100, height=700,
                                       title = "Позиції країн в рейтингу конкурентоспроможності", 
                                       vAxis = "{title : 'Cередня зміна позиції за 3 роки'}",
                                       bubble ="{textStyle:{color: 'none'}}"))
plot(Bubble)

Bubble <- gvisBubbleChart(gci_rating, idvar="Global Competitiveness Index", xvar="Score1", 
                          yvar="Середні коливання індексу 2006-2016", colorvar="Країна", sizevar="GDP per Capita 2015 current $",
                          options=list(hAxis='{minValue:2.5, maxValue:5}', width=1100, height=800))
plot(Bubble)

Bubble <- gvisBubbleChart(gci_rating, idvar="Global Competitiveness Index", xvar="GDP per Capita 2015 current $", 
                          yvar="Середні коливання індексу 2006-2016", colorvar="Країна", sizevar="Score1",
                          options=list(hAxis='{minValue:0, maxValue:160}', width=1100, height=800))
plot(Bubble)


colorvar="",


bub1 = gvisBubbleChart(gci_rating, idvar="Global Competitiveness Index", xvar="2016-2017", 
                       yvar="Середні коливання індексу 2006-2016", sizevar="Score1",
                       options = list(legend = "none",width = 900, height = 600,title 
                                      =" Crime per State in 2012", sizeAxis ="{maxSize : 40, minSize 
                                      :0.5}",vAxis = "{title : 'Burglary'}",hAxis= "{title : 
                                      'Robbery'}"))





pk <- c("Інститути", "Інфраструктура", "Макроекономіка", "Здоров'я", 
        "Вища освіта", "Ринки", "Праця",
        "Фін. ринок", "Технології", "Розмір ринку", "Бізнес",
        "Інновації")
rs <- c(3,3.9,3.2,6,5.1,4,4.2,3,3.6,4.4,3.6,3.4)

pok <- data.frame(pk,rs)

Gauge <-  gvisGauge(pok, 
                    options=list(min=0, max=7, greenFrom=5,
                                 greenTo=7, yellowFrom=3, yellowTo=5,
                                 redFrom=0, redTo=3, width=900, height=600))
plot(Gauge)


ggplot(long_dat, aes(x = variable, y = value, colour = id, group = id)) +
  geom_line() +
  coord_polar(theta = "x", direction = -1) +
  scale_y_continuous(labels = percent)

df=data_frame(country=c("2006-2007","2007-2008",	"2008-2009",	"2009-2010",	"2010-2011",	"2011-2012",	"2012-2013",	"2013-2014",	"2014-2015",	"2015-2016",	"2016-2017"), 
              val1=c(69,73,72,82,89,82,73,84,76,79,85)
df$`Позиція в рейтингу` <- df$val1*(-1)
Line <- gvisLineChart(df)
plot(Line)

head(CityPopularity)
