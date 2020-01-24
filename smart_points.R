lims <- as.POSIXct(strptime(c("2015-10-01 03:00","2017-05-01 03:00"), format = "%Y-%m-%d %H:%M")) 

ggplot(bigset) +
  geom_point(aes(trans_date,amount),color="#fdae6b") +
  xlab("")+
  ylab("")+
  scale_x_datetime(expand=c(0,0),date_breaks = "1 month", date_minor_breaks = "1 week", 
                   date_labels = "%B\n%Y",limits = lims)+
  scale_y_continuous(expand=c(0,10),
                              breaks=seq(1000000, 11000000, by=1000000),
                              limits=c(0, 11000000),
                              labels=format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) +
  theme_minimal(base_family="PT Sans")+
  theme(
      panel.grid=element_line(),
      panel.grid.major.y=element_line(color="#2b2b2b", linetype="dotted", size=0.15),
      panel.grid.major.x=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.minor.y=element_blank(),
      axis.line.x=element_line(color="#2b2b2b", size=0.15),
      axis.ticks=element_line(),
      axis.ticks.x=element_line(color="#2b2b2b", size=0.15),
      axis.ticks.y=element_blank(),
      axis.ticks.length=unit(5, "pt"),
      plot.margin=unit(rep(0.5, 4), "cm"),
      axis.text.y=element_text(margin=margin(r=-5)),
      plot.title=element_text(family="PT Sans", margin=margin(b=15)),
      plot.subtitle=element_text(family="PT Sans"),
      plot.caption=element_text(size=8, hjust=0, margin=margin(t=15))) 
