sbd <- bigset %>% group_by(trans_date) %>% dplyr::summarise(am=sum(amount))
hh<- seq(as.Date("2015-09-01"), as.Date("2017-05-16"), by="days")
'%!in%' <- function(x,y)!('%in%'(x,y))
hh <- data_frame(
  trans_date=hh[hh %!in% as.Date(sbd$trans_date+3600*3)],
  am=NA
)
sbd <- rbind(sbd,hh)

sbd <- sbd %>% 
mutate(year = as.numeric(format(trans_date+3600*3, "%Y")), 
       month.order = ordered(format(trans_date+3600*3, "%B"), levels = c("січень", "лютий", "березень", "квітень", "травень", "червень", "липень", "серпень", "вересень", "жовтень", "листопад", "грудень")),
       weekday.order = ordered(weekdays(trans_date+3600*3), levels = rev(c("понеділок", "вівторок", "середа", "четвер", "п'ятниця", "субота", "неділя"))), 
       day.month = as.integer(format(trans_date+3600*3, "%d")),
        year.month = format(trans_date+3600*3, "%h %Y"), 
       week = as.numeric(format(trans_date+3600*3, "%W")), 
       tmp_date = as.Date(format(trans_date+3600*3, "%Y%m%d"), "%Y%m%d")) %>%
  select(-trans_date) 

sbd %>% group_by(year.month) %>% dplyr::mutate(monthweek = 1+week-min(week)) %>%
ggplot(aes(monthweek, weekday.order, fill = am))+ 
  geom_tile(colour = "white")+ 
  facet_grid(year~month.order)+ 
  scale_fill_gradient2(name = "",  low = "royalblue3", mid = "gold2", 
                       high = "orangered3", na.value = "#f0f0f0",breaks=seq(0,400000000,50000000),
                       labels=format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE),
                       limits=c(0,400000000))+ 
  scale_x_continuous(breaks=seq(1, 6, 1),expand=c(0,0))+ 
  labs(y="",x = "тиждень місяця") +
  geom_text(aes(label = day.month), 
            vjust = 0.5, family = "PT Sans", face = "bold", size = 2.6)+
  theme_minimal(base_family="PT Sans")+
  theme(
    panel.grid=element_line(),
    panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    axis.line.x=element_line(color="#2b2b2b", size=0.15),
    axis.ticks=element_line(),
    axis.ticks.x=element_line(color="#2b2b2b", size=0.15),
    axis.ticks.y=element_blank(),
    axis.ticks.length=unit(5, "pt"),
    plot.margin=unit(rep(0.5, 4), "cm"),
    legend.position = "bottom", legend.text = element_text(family = "PT Sans", 
                                                            size = 9, lineheight = 0.8), 
    axis.text.y=element_text(margin=margin(r=-5)),
    plot.title=element_text(family="PT Sans", margin=margin(b=15)),
    plot.subtitle=element_text(family="PT Sans"),
    plot.caption=element_text(size=8, hjust=0, margin=margin(t=15))) 
