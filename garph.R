Sys.setlocale(,"RU_ru")
library(lubridate)
library(ggplot2)
library(reshape2)
omn2 <- omn[is.na(omn$X1)!=T,]
omn3 <- melt(omn2[c(1,38:39)], id.vars = "X1")
ggplot(omn3, aes(x = dmy(X1), y = value, color=variable)) +
  geom_line()+
  scale_y_continuous(limits=c(0, 30), labels = comma) +
  geom_vline(xintercept = as.numeric(dmy("30-10-2004")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("26-03-2006")),lty = "dashed",color="lightgreen")+
  geom_vline(xintercept = as.numeric(dmy("30-09-2007")),lty = "dashed",color="lightgreen")+
  geom_vline(xintercept = as.numeric(dmy("19-01-2010")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("07-02-2010")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("26-10-2012")),lty = "dashed",color="lightgreen")+
  geom_vline(xintercept = as.numeric(dmy("25-05-2014")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("27-10-2014")),lty = "dashed",color="lightgreen")+
  theme_classic()

omnus$`Hillary Clinton` <- as.numeric(sub("%", "",omnus$`Hillary Clinton`,fixed=TRUE))/100
omnus$`Donald Trump` <- as.numeric(sub("%", "",omnus$`Donald Trump`,fixed=TRUE))/100
omnus$date <- mdy(omnus$date)

ep$zavzhdy <- as.numeric(sub("%", "",ep$zavzhdy,fixed=TRUE))/100
ep2 <- melt(ep[2:5], id.vars = "date")
ggplot(ep2, aes(x = date, y = value, color=variable)) +
  geom_line()+
  scale_y_continuous(limits=c(0, 0.8), labels = comma) +
  geom_vline(xintercept = as.numeric(dmy("31-10-1999")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("31-03-2002")),lty = "dashed",color="lightgreen")+
  geom_vline(xintercept = as.numeric(dmy("30-10-2004")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("26-03-2006")),lty = "dashed",color="lightgreen")+
  geom_vline(xintercept = as.numeric(dmy("30-09-2007")),lty = "dashed",color="lightgreen")+
  geom_vline(xintercept = as.numeric(dmy("19-01-2010")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("07-02-2010")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("26-10-2012")),lty = "dashed",color="lightgreen")+
  geom_vline(xintercept = as.numeric(dmy("25-05-2014")),lty = "dashed",color="pink")+
  geom_vline(xintercept = as.numeric(dmy("27-10-2014")),lty = "dashed",color="lightgreen")+
  theme_classic()

, group = Site

ma <- function(x,n=3){filter(x,rep(1/n,n), sides=2)}

movingAverage <- function(x, n=1, centered=FALSE) {
  
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x))
  count <- rep(0, length(x))
  
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), x[1:(length(x)-i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # return sum divided by count
  s/count
}

vik2 <- melt(vik,id.vars = "date")
ggplot(vik2, aes(x = dmy(date), y = value, color=variable)) + scale_y_continuous(limits=c(18, 55), labels = comma) +
  geom_line() + theme_classic()

omn4 <- melt(omn[c(1,24:27)], id.vars = "X") 
ggplot(omn4, aes(x = dmy(X), y = value2, color=variable)) +
  geom_line() + theme_classic()


ggplot(med_obs, aes(parse_number(year), parse_number(percent), fill=r1_2)) + geom_area(position = "stack") + theme_classic()


hchart(data2[data2$city=="Черкаси",], "treemap", x = universities.name, value = percent, color = percent) %>% hc_title(text = paste0("Університети, в яких вчаться/вчилися випускники школи №")) %>% 
  hc_subtitle(text = "За даними профілів vk.com")





if (length(nbase[[which(schools_base$V4==(osvita %>% 
                                          filter(grepl(substring("Дніпро",1,4), full_name)) %>%
                                          filter(grepl(paste0(4," "), full_name)))$full_name[1])]])==1)
{print ("Sorry, there is no data for you requested combination. Please change your input selections")} else {
  data<-ddply(nbase[[which(schools_base$V4==(osvita %>% filter(grepl(substring("Дніпро",1,4), full_name)) %>%
                                               filter(grepl(paste0(4," "), full_name)))$full_name[1])]], .(trans_date), summarize, money=sum(as.numeric(as.character(amount)), na.rm = T))
  
  data$trans_date <- parse_date(substring(data$trans_date,1,10))
  
  q = data.frame(data$money)
  rownames(q) = data$trans_date
  q <- as.xts(q)
  
  hchart(q)}


round(sum(parse_number(osvita$number_places[osvita$full_name==(osvita %>% 
                                                                 filter(grepl(substring("Дніпро",1,4), full_name)) %>%
                                                                 filter(grepl(paste0(3," "), full_name)))$full_name[1]])>osvita$number_places[osvita$city=="Дніпро"], na.rm=T)/length(osvita$number_places[osvita$city=="Дніпро"])*100,2)
