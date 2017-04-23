Sys.setlocale(,"RU_ru")
library("FactoMineR")
library("factoextra")
library(foreign)
gfk <- read.spss("Dataset.sav",to.data.frame = T,reencode = "utf-8")

gfk$v81 <- droplevels(gfk$v81)

crsp <- CA(xtabs(v632~v633+v81,data=gfk),graph = F)
fviz_ca_biplot(crsp, col.row="orange", col.col ="steelblue") +
  theme_minimal()

ggplot()+
  geom_point(aes(scale(unname(crsp$row$coord[,1])),scale(unname(crsp$row$coord[,2])),size=unname(crsp$call$marge.row),labels=names(crsp$call$marge.row)),color="red")+
  geom_text(aes(scale(unname(crsp$row$coord[,1])),scale(unname(crsp$row$coord[,2]))+0.15,label=names(crsp$call$marge.row)),size=3)+
  geom_point(aes(scale(unname(crsp$col$coord[,1])),scale(unname(crsp$col$coord[,2])),size=unname(crsp$call$marge.col),labels=names(crsp$call$marge.col)),color="blue") +
  geom_text(aes(scale(unname(crsp$col$coord[,1])),scale(unname(crsp$col$coord[,2]))+0.15,label=names(crsp$call$marge.col)),size=3)+
  theme_minimal() + theme(legend.position = "none") + ylab("")+xlab("")

data <- data.frame(gfk$v633,gfk$v81,gfk$v7,gfk$v13,gfk$v5)
mca1 <- MCA(data,graph = F)
cats = apply(data, 2, function(x) nlevels(as.factor(x)))
# data frame with variable coordinates
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") + 
  theme_minimal()
  #theme(legend.position = "none") + ylab("")+xlab("")


ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable") +
  theme_minimal()
