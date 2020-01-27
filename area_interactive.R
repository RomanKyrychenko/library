#Інтерактивний ареа чарт

library(ggplot2)
library(scales)
library(ggiraph)

positions <- data.frame(
  id = c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2),
  y = c(1,2,3,3,2,4,1,0,0,2,3,1,2,5,2,1,0,0),
  x = c(1,2,3,4,5,6,7,7,1,1,2,3,4,5,6,7,7,1))

gg_poly_1 <- ggplot(positions, aes(x = x,y = y )) +
  geom_polygon_interactive(aes(fill = as.character(id), group = as.character(id),
                               tooltip = id, data_id = id, onclick = id),alpha=0.5) +
  scale_fill_manual(values = c("#2b8cbe","#ece7f2")) +
  ylab("") +
  xlab("") +
  theme_bw()+
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
        text = element_text(family = "Arial"))

ggiraph(code = {print(gg_poly_1)})
