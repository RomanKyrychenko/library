suppressPackageStartupMessages({
  library(plyr)
  library(readr)
  library(dplyr)
  library(foreign)
  library(reshape)
  library(reshape2)
  library(scales)
  library(ggplot2)
  library(lubridate)
})

# Парламентські вибори

el2016m <- read.spss(file.choose())
exp2014par <- as_data_frame(el2016m)
re <- exp2014par %>%
  filter(age == "18-29") %>%
  do_(golos = table(golos))
x <- ddply(exp2014par, .(age), summarize, fr = table(golos), par = names(table(golos)))
y <- dcast(x, par ~ age, value.var = freq)

y <- cast(x, par ~ age, mean, value = "fr")
y <- y %>% arrange(desc(percent18_29))
y <- as_data_frame(y)
y$all <- y$`NA` + y$`18-29` + y$`30-39` + y$`40-49` + y$`50-59` + y$`60+`
y$all_per <- y$all / sum(y$all) * 100

# Голос

golos_par <- ggplot() +
  geom_bar(aes(reorder(par, order(percent18_29, decreasing = TRUE)), all_per), data = y, fill = "cyan", stat = "identity", position = "stack") +
  geom_bar(data = y, aes(reorder(par, order(percent18_29, decreasing = TRUE)), percent18_29), fill = "dodgerblue", stat = "identity", width = .6) +
  scale_fill_manual() +
  scale_y_continuous(limits = c(0, 25), labels = comma) +
  scale_x_discrete(labels = y$par) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  geom_hline(aes(yintercept = mean(5)), colour = "#990000", linetype = "dashed")

w <- ddply(exp2014par, .(age), summarize, fr = table(koly_vyrishyly), par = names(table(koly_vyrishyly)))

z <- cast(w, par ~ age, mean, value = "fr")
z$percent18_29 <- z$`18-29` / sum(z$`18-29`) * 100

z <- z %>% arrange(desc(percent18_29))
z <- as_data_frame(z)
z$all <- z$`NA` + z$`18-29` + z$`30-39` + z$`40-49` + z$`50-59` + z$`60+`
z$all_per <- z$all / sum(z$all) * 100

ggplot() +
  geom_bar(aes(reorder(par, order(percent18_29, decreasing = TRUE)), all_per), data = z, fill = "cyan", stat = "identity", position = "stack") +
  geom_bar(data = z, aes(reorder(par, order(percent18_29, decreasing = TRUE)), percent18_29), fill = "dodgerblue", stat = "identity", width = .6) +
  scale_fill_manual() +
  scale_y_continuous(limits = c(0, 40), labels = comma) +
  scale_x_discrete(labels = z$par) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

reg <- ddply(exp2014par, .(age), summarize, fr = table(region5), par = names(table(region5)))

rg <- cast(reg, par ~ age, mean, value = "fr")
rg$percent18_29 <- rg$`18-29` / sum(z$`18-29`) * 100

rg <- rg %>% arrange(desc(percent18_29))
rg <- as_data_frame(rg)
rg$all <- rg$`NA` + rg$`18-29` + rg$`30-39` + rg$`40-49` + rg$`50-59` + rg$`60+`
rg$all_per <- rg$all / sum(rg$all) * 100

ggplot() +
  geom_bar(aes(reorder(par, order(percent18_29, decreasing = TRUE)), all_per), data = rg, fill = "cyan", stat = "identity", position = "stack") +
  geom_bar(data = rg, aes(reorder(par, order(percent18_29, decreasing = TRUE)), percent18_29), fill = "dodgerblue", stat = "identity", width = .6) +
  scale_fill_manual() +
  scale_y_continuous(limits = c(0, 50), labels = comma) +
  scale_x_discrete(labels = rg$par) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

sex <- ddply(exp2014par, .(age), summarize, fr = table(sex), par = names(table(sex)))

sx <- cast(sex, par ~ age, mean, value = "fr")
sx$percent18_29 <- sx$`18-29` / sum(sx$`18-29`) * 100

sx <- sx %>% arrange(desc(percent18_29))
sx <- as_data_frame(sx)
sx$all <- sx$`NA` + sx$`18-29` + sx$`30-39` + sx$`40-49` + sx$`50-59` + sx$`60+`
sx$all_per <- sx$all / sum(sx$all) * 100

ggplot() +
  geom_bar(aes(reorder(par, order(percent18_29, decreasing = TRUE)), all_per), data = sx, fill = "cyan", stat = "identity", position = "stack") +
  geom_bar(data = sx, aes(reorder(par, order(percent18_29, decreasing = TRUE)), percent18_29), fill = "dodgerblue", stat = "identity", width = .6) +
  scale_fill_manual() +
  scale_y_continuous(limits = c(0, 55), labels = comma) +
  scale_x_discrete(labels = sx$par) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

ed <- ddply(exp2014par, .(age), summarize, fr = table(education), par = names(table(education)))

e <- cast(ed, par ~ age, mean, value = "fr")
e$percent18_29 <- e$`18-29` / sum(e$`18-29`) * 100

e <- e %>% arrange(desc(percent18_29))
e <- as_data_frame(e)
e$all <- e$`NA` + e$`18-29` + e$`30-39` + e$`40-49` + e$`50-59` + e$`60+`
e$all_per <- e$all / sum(e$all) * 100

ggplot() +
  geom_bar(aes(reorder(par, order(percent18_29, decreasing = TRUE)), all_per), data = e, fill = "cyan", stat = "identity", position = "stack") +
  geom_bar(data = e, aes(reorder(par, order(percent18_29, decreasing = TRUE)), percent18_29), fill = "dodgerblue", stat = "identity", width = .6) +
  scale_fill_manual() +
  scale_y_continuous(limits = c(0, 55), labels = comma) +
  scale_x_discrete(labels = e$par) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

nas <- data_frame(
  gr = c("18-29", "30-39", "40-49", "50-59", "60+"),
  fr = c(15.6, 15.5, 13.6, 14.5, 21.8)
)
nas$dor <- nas$fr / sum(nas$fr) * 100

gols <- data_frame(gr = names(colSums(e[3:7]) / sum(colSums(e[3:7])) * 100), fr = colSums(e[3:7]) / sum(colSums(e[3:7])) * 100)

ggplot() +
  geom_bar(aes(gr, dor), data = nas, fill = "cyan", stat = "identity", position = "stack") +
  geom_bar(data = gols, aes(gr, fr), fill = "dodgerblue", stat = "identity", width = .6) +
  scale_fill_manual() +
  scale_y_continuous(limits = c(0, 30), labels = comma) +
  scale_x_discrete() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# Президентські вибори

exp2014pre <- as_data_frame(read.spss(file.choose()))

exp2014pre_sum <- ddply(exp2014pre, .(age), summarize, fr = table(golos), par = names(table(golos)))

exp2014pre_sum <- cast(exp2014pre_sum, par ~ age, mean, value = "fr")

exp2014pre_sum$percent18_29 <- exp2014pre_sum$`18-29` / sum(exp2014pre_sum$`18-29`) * 100

exp2014pre_sum <- exp2014pre_sum %>% arrange(desc(percent18_29))
exp2014pre_sum <- as_data_frame(exp2014pre_sum)
exp2014pre_sum$all <- exp2014pre_sum$`NA` + exp2014pre_sum$`18-29` + exp2014pre_sum$`30-39` + exp2014pre_sum$`40-49` + exp2014pre_sum$`50-59` + exp2014pre_sum$`60+`
exp2014pre_sum$all_per <- exp2014pre_sum$all / sum(exp2014pre_sum$all) * 100

golos_pre <- ggplot() +
  geom_bar(aes(reorder(par, order(percent18_29, decreasing = TRUE)), all_per), data = exp2014pre_sum, fill = "cyan", stat = "identity", position = "stack") +
  geom_bar(data = exp2014pre_sum, aes(reorder(par, order(percent18_29, decreasing = TRUE)), percent18_29), fill = "dodgerblue", stat = "identity", width = .6) +
  scale_fill_manual() +
  scale_y_continuous(limits = c(0, 60), labels = comma) +
  scale_x_discrete(labels = exp2014pre_sum$par) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

o <- ddply(exp2014pre, .(age), summarize, fr = table(koly_vyrishyly), par = names(table(koly_vyrishyly)))

p <- cast(o, par ~ age, mean, value = "fr")
p$percent18_29 <- p$`18-29` / sum(p$`18-29`) * 100

p <- p %>% arrange(desc(percent18_29))
p <- as_data_frame(p)
p$all <- p$`NA` + p$`18-29` + p$`30-39` + p$`40-49` + p$`50-59` + p$`60+`
p$all_per <- p$all / sum(p$all) * 100

ggplot() +
  geom_bar(aes(reorder(par, order(percent18_29, decreasing = TRUE)), all_per), data = p, fill = "cyan", stat = "identity", position = "stack") +
  geom_bar(data = p, aes(reorder(par, order(percent18_29, decreasing = TRUE)), percent18_29), fill = "dodgerblue", stat = "identity", width = .6) +
  scale_fill_manual() +
  scale_y_continuous(limits = c(0, 40), labels = comma) +
  scale_x_discrete(labels = p$par) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

# euro_rus

omn <- read_csv("omn.csv")
omn <- as.data.frame(omn)
molodgol <- melt(omn[c(1, 35:37)], id.vars = "X1")
molodgol$X1 <- dmy(molodgol$X1)
molodgol$X2 <- as.numeric(as.Date(molodgol$X1, origin = "1970-01-01"))

ggplot(molodgol, aes(X1, value, fill = variable)) +
  geom_area() +
  scale_y_continuous(limits = c(0, 100), labels = comma) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )


yavka <- data_frame(
  "вибори" = c("Парламентські вибори 2014", "Президентські вибори 2014", "Парламентські вибори 2012", 
               "Президентські вибори 2010 І тур", "Президентські вибори 2010 ІІ тур", "Парламентські вибори 2007", "Парламентські вибори 2006"),
  "явка" = c(52.42, 60.29, 57.98, 66.76, 69.15, 57.94, 67.55),
  "nev" = c(21.59713, 17.1742, 16.27146, 14.27951, 14.27951, 17.31796, 14.63561),
  "proty" = c(21.67624, 19.18191, 22.34095, 19.56666, 19.56666, 22.34095, 8.701651)
)

ggplot() +
  geom_point(data = yavka, aes(явка, nev), color = "blue") +
  stat_smooth(data = yavka, aes(явка, nev), method = "lm", se = F) +
  geom_point(data = yavka, aes(явка, proty), color = "red") +
  stat_smooth(data = yavka, aes(явка, proty), method = "lm", color = "red", se = F) +
  scale_y_continuous(limits = c(5, 25), labels = comma) +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), panel.background = element_blank()
  ) +
  theme(
    axis.text.x = element_text(colour = "black", size = 10, angle = 90, hjust = 1),
    axis.text.y = element_text(colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )