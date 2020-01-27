#Хмарна інтерактивна графіка

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(ggiraph)
})

credits3 <- readr::read_csv("https://raw.githubusercontent.com/RomanKyrychenko/plots/master/credits2.csv")

gg_poly_1 <- ggplot(credits3, aes(x = Дата,y = Кредити)) +
  geom_polygon_interactive(aes(fill = Статус, group = Банк,
                               tooltip = Банк, data_id = Банк, onclick = Банк),alpha=0.5) +
  scale_y_continuous(limits=c(0, 190000000), labels = comma) +
  scale_fill_manual(values = c("#2b8cbe","#ece7f2")) +
  ylab("") +
  xlab("") +
  hrbrthemes::theme_ipsum(base_family = "Lato")+
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dotted"), 
        panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 6,color = "black"),
        axis.title.y = element_text(margin = margin(r = 20), color = "grey70"),
        axis.title.x = element_text(margin = margin(t = 20), color = "darkslategrey"),
        plot.title = element_text(size = 12, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 8, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 6, margin = margin(t = 10), color = "grey70", hjust = 0),
        strip.text = element_text(size = 4, angle = 0),
        strip.background=element_rect(colour = "white", fill="white"),
        text = element_text(family = "Lato"))

ggiraph(code = {print(gg_poly_1)})
