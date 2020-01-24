library(readxl)
Budget <- read_excel("~/backup/Downloads/Budget.xlsx")
library(ggplot2)

ggplot(Budget, aes(x = 2016, xend = 2017, y = `2016`, yend = `2017`, color = `...1`)) +
  geom_segment() +
  geom_point() +
  geom_point(aes(x = 2017,y = `2017`, color = `...1`)) +
  xlab(NULL) + 
  ylab(NULL) +
  hrbrthemes::theme_ipsum()
