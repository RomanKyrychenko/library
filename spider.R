library(fmsb)

# Create data: note in High school for Jonathan:

name=c("Інститути", "Інфраструктура", "Макроекономічне середовище","Розмір ринку", 
                 "Вища освіта та професійна підготовка", "Ефективність товарних ринків", "Ефективність ринку праці",
       "Охорона здоров’я та початкова освіта", "Технологічна готовність","Розвиток фінансового ринку" , "Рівень розвитку бізнесу",
                 "Інновації")
rs2016 <- c(3,3.9,3.2,4.4,5.1,4,4.2,6,3.6,3,3.6,3.4)
rs2015 <- c(3.1,4.1,3.1,4.5,5.0,4,4.3,6.1,3.4,3.2,3.7,3.4)
data=data.frame(rbind(rs2015,rs2016))
colnames(data) <- name
rownames(data) <- c("2015","2016")



colors_border=c(rgb(0.2,0.5,0.5,0.9),rgb(0.2,0.5,0.5,0.9),rgb(0.8,0.2,0.5,0.9))
colors_in=c(rgb(0.2,0.5,0.5,0.4),rgb(0.8,0.2,0.5,0.4),rgb(0.2,0.4,0.5,0.3))
radarchart( data  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,7.01,2), cglwd=0.8,
            #custom labels
            vlcex=0.8, bg.col="white"
)
legend(1.5,1, rownames(data[-c(1,2),]), seg.len=0.5, title="Рік", pch=1, 
       bty="n" ,lwd=3, y.intersp=0.5, horiz=FALSE, col=colors_in)

save

min <- c(137,79,134,47,54,129,84,88,94,130,103,93)
 
max <- c(97,65,80,26,33,85,49,43,65,63,76,52)

m2016 <- c(129,75,128,47,33,108,73,54,85,130,98,52)

rdata <- data.frame(rbind(max,min,m2016))
colnames(rdata) <- name
rownames(rdata) <- c("Мінімальні значення", "Максимальні значення", "2016 рік")
rdata=rbind(rep(200,12) , rep(0,12) , rdata)
radarchart(rdata, axistype=1, 
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,200,50), cglwd=0.8,
            vlcex=0.8, bg.col="white"
)
legend(1.5,1, rownames(rdata[-c(1,2),]), seg.len=0.5, title="", pch=1, 
       bty="n" ,lwd=3, y.intersp=0.5, horiz=FALSE, col=colors_in)
