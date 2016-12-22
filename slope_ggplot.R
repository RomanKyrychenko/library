library(readxl)
Budget <- read_excel("~/Downloads/Budget.xlsx")
library(ggplot2)
budget2 <- reshape2::melt(Budget, id.vars="")
ggplot() +
  geom_line(data=budget2[c(1,5),],aes(x=as.numeric(as.character(variable)), y=value),color="lightblue") +
  geom_line(data=budget2[c(2,6),],aes(x=as.numeric(as.character(variable)), y=value),color="red") +
  geom_line(data=budget2[c(3,7),],aes(x=as.numeric(as.character(variable)), y=value),color="gold") +
  geom_line(data=budget2[c(4,8),],aes(x=as.numeric(as.character(variable)), y=value),color="pink") +
  geom_point(data=budget2[c(1,5),],aes(x=as.numeric(as.character(variable)), y=value),color="lightblue")+
  geom_point(data=budget2[c(2,6),],aes(x=as.numeric(as.character(variable)), y=value),color="red")+
  geom_point(data=budget2[c(3,7),],aes(x=as.numeric(as.character(variable)), y=value),color="gold")+
  geom_point(data=budget2[c(4,8),],aes(x=as.numeric(as.character(variable)), y=value),color="pink")+
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none")+
  theme(panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank()) +
  xlab(NULL) + 
  ylab(NULL) 
