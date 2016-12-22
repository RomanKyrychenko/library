library(plotrix)

staircase.plot

value <- c(-10,-7,7,9,-11,8,-3,-6)
labels <- c("2009-2010",	"2010-2011",	"2011-2012",	"2012-2013",	"2013-2014",	"2014-2015",	"2015-2016",	"2016-2017")
sales <- data.frame(value, labels)

staircase.plot(sales$value, totals= c(F,F,F,F,F,F,F,F), labels = 
                 sales$labels, total.col = c("lightgreen"),
               inc.col = c("red","red","lightgreen","lightgreen","red","lightgreen","red","red"),main ="Waterfall Plot showing financial data")


sample_size<-c(72,10,82,7,89,-7,82,-9,73,11,84,-8,76,3,79,6,85)
totals<-c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,F,T,F,T,F,T,F,T,F,T)
labels<-c("2008-2009","Зміна позиції", "2009-2010",	"Зміна позиції","2010-2011","Зміна позиції",	"2011-2012","Зміна позиції",	"2012-2013","Зміна позиції",	"2013-2014","Зміна позиції",	"2014-2015","Зміна позиції",	"2015-2016","Зміна позиції",	"2016-2017")
staircase.plot(sample_size,totals,labels,main="Зміна позиції України у рейтингу конкурентоспроможності ВЕФ",
               total.col="gray",bg.col="white",inc.col=2:4,direction="s")
font.lab="Helvetica"

#eeeebb

font=list(face="italic")

.define.fonts <- function () {
  quartzFonts(helvetica = c("Helvetica Neue Light", "Helvetica Neue Bold", "Helvetica Neue Light Italic", 
                                                                  "Helvetica Neue Bold Italic"))
}