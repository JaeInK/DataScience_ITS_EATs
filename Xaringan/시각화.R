setwd("C:\\Users\\EUNJI\\Desktop\\ds_yonsei\\프로젝트\\data\\real")
its_eats <- read.csv("final_0606.csv",header = TRUE)      
its_eats$day.x <- as.character(its_eats$day.x)
its_eats$day.x <- factor(its_eats$day.x,levels=c("월","화","수","목","금","토","일"))
library(tidyverse)
install.packages("zoo")


calls <- its_eats %>% 
  group_by(type,date) %>% 
  dplyr::summarise(total=sum(call))

calls$date <- paste(str_sub(calls$date,start=1,end=4),
                    "-",
                    str_sub(calls$date,start=5,end=6),
                    "-",
                    str_sub(calls$date,start=7,end=8),
                    sep="")
calls$date <- as.Date(calls$date)
calls$month <- as.numeric(as.POSIXlt(calls$date)$mon+1)
calls$monthf <- factor(calls$month,levels=as.character(1:12),
                       labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec"),
                       ordered = TRUE)
calls$weekday <- as.POSIXlt(calls$date)$wday
calls$weekdayf <- factor(calls$weekday, levels= c(0,6,5,4,3,2,1),
                         labels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
                         ordered=TRUE)
calls$yearmonth <- zoo::as.yearmon(calls$date)
calls$yearmonthf <- factor(calls$yearmonth)
calls$week <- as.numeric(format(as.Date(calls$date),"%W"))



library(plyr)
?ddply
calls <- ddply(calls,.(yearmonthf),transform,monthweek=1+week-min(week))
p1 <- ggplot(calls,aes(monthweek,weekdayf,fill=total)) +
  geom_tile(colour="white") + facet_grid(type~monthf) +
  scale_fill_gradient(low="yellow",high="red")
p1
data_original %>% filter(date==20180618) %>% filter(type=="치킨") %>% 
  group_by(region) %>% dplyr::summarise(total=sum(call)) %>% dplyr::summarise(total=sum(total))

myTheme <- theme(
  panel.background = element_rect(fill = "white"), 
  panel.grid.major = element_line(colour = "lightgrey"),
  panel.grid.minor = element_line(colour = "lightgrey"),
  strip.background = element_rect(fill = "black"),
  strip.text = element_text(color = "white"),
  axis.text = element_text(color = "black")
)
calls_cn <- calls %>% filter(type=="중국집")
calls_ck <- calls %>% filter(type=="치킨")
calls_pz <- calls %>% filter(type=="피자")
p1_cn <- ggplot(calls_cn,aes(monthweek,weekdayf,fill=total)) +
  geom_tile(colour="white") + facet_grid(.~monthf) +
  scale_fill_gradient(low="white",high="#EA4335") +
  labs(title = "2018년 중국집 주문 량") + ylab("") + xlab("") + myTheme
p1_ck <- ggplot(calls_ck,aes(monthweek,weekdayf,fill=total)) +
  geom_tile(colour="white") + facet_grid(~monthf) +
  scale_fill_gradient(low="white",high="#4285F4") +
  labs(title = "2018년 치킨 주문 량") + ylab("") + xlab("") + myTheme
p1_pz <- ggplot(calls_pz,aes(monthweek,weekdayf,fill=total)) +
  geom_tile(colour="white") + facet_grid(~monthf) +
  scale_fill_gradient(low="white",high="#FBBC05")+
  labs(title = "2018년 피자 주문 량") + ylab("") + xlab("") + myTheme

library(gridExtra)
grid.arrange(p1_cn,p1_ck,p1_pz,ncol=1)


###
days <- its_eats %>% group_by(type, day.x,region) %>% 
        dplyr::summarise(total=sum(call)) %>%
        arrange(desc(total))
days$day.x <- factor(days$day.x,levels = c("월","화","수","목","금","토","일"))


p2 <- ggplot(data = days ,aes(x = day.x,
                              y = total,color=type ,group = type)) +
  geom_line(size=1) +
  facet_wrap(~ region) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_rect(fill = "steelblue"),
        strip.text = element_text(color = "white")
  ) +
  scale_color_manual(values=c("#EA4335", "#4285F4", "#FBBC05")) 
p2

# 3) 성별

genders <- its_eats_1 %>% group_by(gender,type) %>% 
  dplyr::summarise(total=sum(call)) %>% 
  arrange(gender,desc(type)) 
 
genders  <- ddply(genders, "gender",transform,label_ypos=cumsum(total)) 
genders  <- ddply(genders, "gender",transform,pct=round(total/sum(total),2)*100) 
p3 <-  ggplot(genders, aes(x=gender,y=total,fill=type)) + geom_bar(stat="identity") +
  theme_bw() + 
  scale_fill_manual(values=c("#EA4335", "#4285F4", "#FBBC05")) +
  geom_text(aes(y=label_ypos, label=paste(pct,"%")), vjust=1.6,color="white", size=5) + 
  geom_text(aes(y=label_ypos-400000, label=paste("(",total,"건)")), vjust=1.6,color="white", size=3) 
p3

sum(genders[1:3,3])
sum(genders[4:6,3])


# 4) 연령별

ages <- its_eats_1 %>% group_by(age,type) %>% 
  dplyr::summarise(total=sum(call)) %>% 
  arrange(age,desc(type)) 
ages <- ddply(ages, "age",transform,label_ypos=cumsum(total)) 
ages <- ddply(ages, "age",transform,pct=round(total/sum(total),2)*100)
ages <- ddply(ages, "age",transform,label_ypos2 = sum(total))
p4 <- ggplot(aes(x=age,y=total,fill=type)) + geom_bar(stat="identity") +
  theme_bw() + 
  scale_fill_manual(values=c("#EA4335", "#4285F4", "#FBBC05")) +
  geom_text(aes(y=label_ypos+50000, label=paste(pct,"%")), vjust=1.6,color="white", size=5) + 
  geom_text(aes(y=label_ypos2+300000, label=paste(label_ypos2,"건")), vjust=1.6,color="black", size=4) 
p4

## 강수량
gangsus <- its_eats_1 %>% 
  group_by(date) %>% 
  dplyr::summarise(mean_gangsu = mean(gangsu))
data_p5 <- its_eats_1 %>%
  filter(gangsu>0) %>% 
  group_by(date,type,day.x) %>% 
  dplyr::summarise(total=sum(call))
data_p5 <- left_join(data_p5,gangsus,by="date")
p5 <- data_p5 %>%
  ggplot(aes(x=mean_gangsu,y=total,color=type)) +
  geom_point(alpha=0.5) +
  facet_wrap(~day.x,scales = "free") +
  scale_color_manual(values=c("#EA4335", "#4285F4", "#FBBC05")) +
  theme_bw() + 
  stat_smooth(method="gam",se=FALSE)+
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white"))
p5


# 6) temperature

temperatures <- its_eats_1 %>% 
  group_by(date,type,region) %>% 
  dplyr::summarise(total=sum(call),temperature = mean(temperature))

p6 <-  ggplot(temperatures, aes(x=temperature,y=total,color=type)) +
  geom_point(alpha=0.6,size=0.6) +
  facet_wrap(~region,scales = "free") +
  scale_color_manual(values=c("#EA4335", "#4285F4", "#FBBC05")) +
  theme_bw() +
  stat_smooth(method="loess",se=FALSE)+
  theme(strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white"))
p6

library(zoo)
