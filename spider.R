library(fmsb)

name <- c(
  "Інститути", "Інфраструктура", "Макроекономічне середовище", "Розмір ринку",
  "Вища освіта та професійна підготовка", "Ефективність товарних ринків", "Ефективність ринку праці",
  "Охорона здоров’я та початкова освіта", "Технологічна готовність", "Розвиток фінансового ринку", "Рівень розвитку бізнесу",
  "Інновації"
)

colors_border <- c(rgb(0.2, 0.5, 0.5, 0.9), rgb(0.2, 0.5, 0.5, 0.9), rgb(0.8, 0.2, 0.5, 0.9))
colors_in <- c(rgb(0.2, 0.5, 0.5, 0.4), rgb(0.8, 0.2, 0.5, 0.4), rgb(0.2, 0.4, 0.5, 0.3))

min <- c(137, 79, 134, 47, 54, 129, 84, 88, 94, 130, 103, 93)
max <- c(97, 65, 80, 26, 33, 85, 49, 43, 65, 63, 76, 52)
m2016 <- c(129, 75, 128, 47, 33, 108, 73, 54, 85, 130, 98, 52)

rdata <- data.frame(rbind(max, min, m2016))
colnames(rdata) <- name
rownames(rdata) <- c("Мінімальні значення", "Максимальні значення", "2016 рік")
rdata <- rbind(rep(200, 12), rep(0, 12), rdata)

radarchart(rdata,
  axistype = 1,
  pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
  cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 200, 50), cglwd = 0.8,
  vlcex = 0.8
)

legend(1.5, 1, rownames(rdata[-c(1, 2), ]),
  seg.len = 0.5, title = "", pch = 1,
  bty = "n", lwd = 3, y.intersp = 0.5, horiz = FALSE, col = colors_in
)
