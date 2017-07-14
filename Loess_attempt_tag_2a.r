#You need to install two package dplyr,ggplot2

library(dplyr)
library(ggplot2)
library(VTrack)
library(lubridate)

max_count <- 701
df <- read.csv("~/r-code/35175_tag2.csv")
#second column is UTC+10 which is required time format
df$date <- as.Date(df$Date.and.Time..UTC.10.00.,format="%d/%m/%Y")

df <- df[,c(3,12)]

df <- data.frame(df, stringsAsFactors = TRUE)

#check for missing values (false or true)
missing(df)

head(df)
dim(df)
summary(df)

#Count by date
df <- df %>% count(date,Receiver)
df$prop <- df$n/max_count

#Reorder factors
df$Receiver <- factor(df$Receiver, levels=c("VR2W-121133","VR2W-112177","VR2W-110020","VR2W-111070","VR2W-110021"))
#Recode 
df$rec<- dplyr::recode(as.character(df$Receiver),"VR2W-121133"=0,"VR2W-112177"=100,"VR2W-110020"=220,"VR2W-111070"=230,"VR2W-110021"=420)

df$Receiver <- as.numeric(df$Receiver)

#exclude day one and last day


attach(df)
df <- df[ which(df$date!='2014-03-11'),]
df <- df[ which(df$date!='2014-06-19'),]
#df <- df[ which(df$rec!='420' && df$date >= '2014-04-30')]


# Graph
ggplot(df, aes(x=rec, y=prop)) +
  geom_smooth(span= 0.9) +
  geom_point() +
  labs(x="Receiver", y="prop", title="Range Tag 2") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20) )


#I can kind of see how the scale is continous below but not sure how to integrate the two so that not only is the x scale relative but so the curve is based on that relative data too
#ggplot(data = totalDay,aes(x=rec,y=prop,group=rec))+geom_point()+xlab("Distance (m)")+ylab("Detection proportion")+scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1.1),labels = seq(0,100,10)) + scale_x_continuous( breaks = c(0,100,220,230,420),limits = c(0,450)

#id also like to exclude day 1 which is 11 march 2014 and the last day (day 70) 19 may 2014 (both based on second column UTC +10) and
# and for one receiver at distance "420" metres was stopped working on the 1 may 2014 so Id like that reciver to have only the data to the 30 april included in plot.

# lastly some days potentially have no data but that needs to be 0 rather than no data...