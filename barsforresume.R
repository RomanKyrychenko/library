library(ggplot2)
library(haven)
library(readr)
library(xlsx)
library(reshape2)
library(scales)

#Зчитуєм файл, я для зручності перевів таблицю ексель в csv і вже пряцував із легшим форматом, file.choose() 
#дозволяє нам обрати будь-який файл у будь-якій директорії

current <- read.csv(file.choose())
pol <- as_data_frame(current[2:11,])
colnames(pol) <- c("День", "Дата", "День дата", "Нейтральне президент",	"Негатив президент",	
                   "% негативу президент",	"Нейтральне АПУ",	"Негатив АПУ",	"% негативу АПУ",
                   "Нейтральне Кабмін",	"Негатив Кабмін",	"% негативу Кабмін",	"Нейтральне політичні персони",
                   "Негатив політичні персони",	"% негативу політичні персони",	"День-дата", "Президент",	"АПУ",
                   "Кабмін",	"Персони")

#Змінні зчитуються як фактори, тому кількісні потрібно переформатувати
#Тут краще через dplyr було змінити, але в мене машина чомусь деякі імена змінних не хотіла читати, тому формат 
#змінив по стандарту

pol$`Нейтральне президент` <- as.numeric(as.character(pol$`Нейтральне президент`))
pol$`Негатив президент` <- as.numeric(as.character(pol$`Негатив президент`))
pol$`Нейтральне АПУ` <- as.numeric(as.character(pol$`Нейтральне АПУ`))
pol$`Негатив АПУ` <- as.numeric(as.character(pol$`Негатив АПУ`))
pol$`Нейтральне Кабмін` <- as.numeric(as.character(pol$`Нейтральне Кабмін`))
pol$`Негатив Кабмін` <- as.numeric(as.character(pol$`Негатив Кабмін`))
pol$`Нейтральне політичні персони` <- as.numeric(as.character(pol$`Нейтральне політичні персони`))
pol$`Негатив політичні персони` <- as.numeric(as.character(pol$`Негатив політичні персони`))
pol$Президент <- as.numeric(as.character(pol$Президент))
pol$АПУ <- as.numeric(as.character(pol$АПУ))
pol$Кабмін <- as.numeric(as.character(pol$Кабмін))
pol$Персони <- as.numeric(as.character(pol$Персони))

pol <- pol[c(1:7,9),]

pres <- melt(pol[3:5], c("День дата"))
apu <- melt(pol[c(3,7,8)], c("День дата"))
kabmin <- melt(pol[c(3,10,11)], c("День дата"))
persons <- melt(pol[c(3,13,14)], c("День дата"))

fill <- c("#56B4E9", "#F0E442")

#Президент

p1 <- ggplot() + 
  geom_bar(aes(`День дата`, value, fill = variable), data = pres, stat="identity", position ="stack") + 
  scale_fill_manual(values=fill) + 
  scale_y_continuous(limits=c(0, 8000000), labels = comma) +
  scale_x_discrete(limits=c("19 Пн","20 Вт","21 Ср","22 Чт","23 Пт","24 Сб", "25 Нд", "1 Сьогодні")) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
        axis.text.x=element_text(colour="black", size = 10), 
        axis.text.y=element_text(colour="black", size = 10),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +
  geom_hline(aes(yintercept=mean(pres$value)), colour="#990000", linetype="dashed") 
p1

#АПУ

p2 <- ggplot() + 
  geom_bar(aes(`День дата`, value, fill = variable), data = apu, stat="identity", position ="stack") + 
  scale_fill_manual(values=fill) + 
  scale_y_continuous(limits=c(0, 8000000), labels = comma) +
  scale_x_discrete(limits=c("19 Пн","20 Вт","21 Ср","22 Чт","23 Пт","24 Сб", "25 Нд", "1 Сьогодні")) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") +
  geom_hline(aes(yintercept=mean(apu$value)), colour="#990000", linetype="dashed") 
p2

#Кабмін

p3 <- ggplot() + 
  geom_bar(aes(`День дата`, value, fill = variable), data = kabmin, stat="identity", position ="stack") + 
  scale_fill_manual() + 
  scale_y_continuous(limits=c(0, 80000000), labels = comma) +
theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") +
  geom_hline(aes(yintercept=mean(kabmin$value,na.rm=T)), colour="#990000", linetype="dashed") 
p3

#Політичні персони

p4 <- ggplot() + 
  geom_bar(aes(`День дата`, value, fill = variable), data = persons, stat="identity", position ="stack") + 
  scale_fill_manual(values=fill) + 
  scale_y_continuous(limits=c(0, 8000000), labels = comma) +
  scale_x_discrete(limits=c("19 Пн","20 Вт","21 Ср","22 Чт","23 Пт","24 Сб", "25 Нд", "1 Сьогодні")) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") +
  geom_hline(aes(yintercept=mean(persons$value, na.rm=T)), colour="#990000", linetype="dashed") 
p4

#Якщо зберігати так повторно, то R без зайвих питань перезапише файл і навіть якщо цей файл відкритий, то він 
#просто в режимі перегляду змінить вигляд

pdf("plots1.pdf")
p1
grid.text(label="5%", x = 0.18, y = 0.06)
grid.text(label="4%", x = 0.18+0.64/6, y = 0.06)
grid.text(label="3%", x = 0.18+0.64/6+0.64/6, y = 0.06)
grid.text(label="2%", x = 0.18+0.64/6+0.64/6+0.64/6, y = 0.06)
grid.text(label="3%", x = 0.18+0.64/6+0.64/6+0.64/6+0.64/6, y = 0.06)
grid.text(label="8%", x = 0.18+0.64/6+0.64/6+0.64/6+0.64/6+0.64/6, y = 0.06)
grid.text(label="0%", x = 0.82, y = 0.06)

grid.text(label="Пн", x = 0.18, y = 0.03)
grid.text(label="Вт", x = 0.18+0.64/6, y = 0.03)
grid.text(label="Ср", x = 0.18+0.64/6+0.64/6, y = 0.03)
grid.text(label="Чт", x = 0.18+0.64/6+0.64/6+0.64/6, y = 0.03)
grid.text(label="Пт", x = 0.18+0.64/6+0.64/6+0.64/6+0.64/6, y = 0.03)
grid.text(label="Сб", x = 0.18+0.64/6+0.64/6+0.64/6+0.64/6+0.64/6, y = 0.03)
grid.text(label="Нд", x = 0.82, y = 0.03)
dev.off()
pdf("plots2.pdf")
p2
dev.off()
pdf("plots3.pdf")
p3
dev.off()
pdf("plots4.pdf")
p4
dev.off()


p1
grid.text(label="5%", x = 0.18, y = 0.06)
grid.text(label="4%", x = 0.18+0.64/6, y = 0.06)
grid.text(label="3%", x = 0.18+0.64/6+0.64/6, y = 0.06)
grid.text(label="2%", x = 0.18+0.64/6+0.64/6+0.64/6, y = 0.06)
grid.text(label="3%", x = 0.18+0.64/6+0.64/6+0.64/6+0.64/6, y = 0.06)
grid.text(label="8%", x = 0.18+0.64/6+0.64/6+0.64/6+0.64/6+0.64/6, y = 0.06)
grid.text(label="0%", x = 0.82, y = 0.06)

grid.text(label="Пн", x = 0.18, y = 0.03)
grid.text(label="Вт", x = 0.18+0.64/6, y = 0.03)
grid.text(label="Ср", x = 0.18+0.64/6+0.64/6, y = 0.03)
grid.text(label="Чт", x = 0.18+0.64/6+0.64/6+0.64/6, y = 0.03)
grid.text(label="Пт", x = 0.18+0.64/6+0.64/6+0.64/6+0.64/6, y = 0.03)
grid.text(label="Сб", x = 0.18+0.64/6+0.64/6+0.64/6+0.64/6+0.64/6, y = 0.03)
grid.text(label="Нд", x = 0.82, y = 0.03)

ggsave("plots1.pdf")