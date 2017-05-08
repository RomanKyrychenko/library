library(ggplot2)
library(scales)
bt <- data.frame(x=as.numeric(unlist(blago[21:32,2])),y=as.numeric(unlist(blago[21:32,3])),z=unlist(blago[21:32,1]))

ggplot(bt) + 
  geom_rect(aes(xmin=0,ymin=0,xmax=x,ymax=x),fill="#ef3b2c",color="#ef3b2c",alpha=0.4) + 
  geom_rect(aes(xmin=0,ymin=0,xmax=y,ymax=y),fill="#238b45",color="#238b45",alpha=0.4) + 
  geom_text(aes(x=y,y=y, label=ifelse(format(ifelse(y<120000,NA,y),big.mark = " ")=="        NA","",format(ifelse(y<120000,NA,y),big.mark = " "))),hjust = 1,vjust=1,color="white") +
  geom_text(aes(x=x,y=x, label=ifelse(format(ifelse(x<120000,NA,x),big.mark = " ")=="       NA","",format(ifelse(x<120000,NA,x),big.mark = " "))),hjust = 1,vjust=1,color="white") +
  facet_wrap(~reorder(z,-y), strip.position="bottom") +
  scale_y_log10(labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) +
  scale_x_log10(labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) +
  xlab("") + ylab("") +
  theme_minimal() + theme(
    strip.text = element_text(hjust = 0, size = 8, family = "Tahoma"),
    legend.position = "none" , axis.ticks.y = element_line(), 
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) 