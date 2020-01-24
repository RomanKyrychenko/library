library(ggplot2)

poriv <- tibble(
  variable = c("чоловіки", "жінки", "18-34", "35-49", "50-64", "65+"),
  Ukraine = c(26.38, 31.91, 30.35, 33.33, 29.11, 21.15),
  US = c(14.83, 14.34, 30, 18, 10, 7.7)
)

ggplot() +
  geom_bar(data = poriv[1:2, ], aes(x = variable, y = Ukraine, colour = "lightblue"), stat = "identity", fill = "#278DBC") +
  geom_bar(data = poriv[1:2, ], aes(x = variable, y = US, colour = "navyblue"), stat = "identity", fill = "navyblue", width = .6) +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40)) +
  xlab("") +
  ylab("") +
  scale_colour_manual(name = "", values = c("lightblue" = "#278DBC", "navyblue" = "navyblue"), labels = c("Ukraine", "US")) +
  scale_fill_manual(values = c("lightblue" = "#278DBC", "navyblue" = "navyblue")) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(size = .05, color = "gray"),
    panel.background = element_rect(fill = "white", colour = "white"),
    axis.ticks = element_blank(),
    legend.position = c(.9, 1),
    legend.direction = "horizontal"
  )

usu <- tibble(
  variable = c("ukr", "ukr", "us", "us"),
  value = c(29.4, 70.6, 14.5, 85.5),
  nev = c("nev", "vus", "nev", "vus")
)

ggplot(usu, aes(variable, value, fill = nev)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_colour_manual(values = c("#278DBC", "navyblue")) +
  hrbrthemes::theme_ipsum()
