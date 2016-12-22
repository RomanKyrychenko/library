pk <- c("Інститути", "Інфраструктура", "Макроекономіка", "Здоров'я", 
        "Вища освіта", "Ринки", "Праця",
        "Фін. ринок", "Технології", "Розмір ринку", "Бізнес",
        "Інновації")
rs <- c(3,3.9,3.2,6,5.1,4,4.2,3,3.6,4.4,3.6,3.4)
pok <- data.frame(pk,rs)

Gauge <-  gvisGauge(pok, 
                    options=list(min=0, max=7, greenFrom=4.66,
                                 greenTo=7, yellowFrom=2.33, yellowTo=4.66,
                                 redFrom=0, redTo=2.33, width=900, height=600))
plot(Gauge)