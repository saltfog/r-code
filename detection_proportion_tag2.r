#You need to install two package dplyr,ggplot2

library(dplyr)
library(ggplot2)

#Replace max_count value with your desired value 
# of maximum count per day
max_count <- 701

df <- read.csv("35175.csv")
df$date <- as.Date(df$Date.and.Time..UTC.10.00.,format="%d/%m/%Y")

df <- df[,c(3,12)]


dat <- read_csv("~/r-code/35175_tag2.csv", 
                        col_types = cols(`Date and Time (UTC)` = col_datetime(format = "%d/%m/%Y %H:%M"), 
                                         `Date and Time (UTC+10:00)` = col_datetime(format = "%d/%m/%Y %H:%M"), 
                                         Latitude = col_skip(), Longitude = col_skip(), 
                                         `Sensor Unit` = col_skip(), `Sensor Value` = col_skip(), 
                                         `Station Name` = col_skip(), `Transmitter Name` = col_skip(), 
                                         `Transmitter Serial` = col_skip()))


totalDay <- dat %>% count(dat$`Date and Time (UTC+10:00)`,Receiver)
totalDay$prop <- paste(totalDay$n/max_count)


#Count by date

totalDay <- df %>% count(date,Receiver)
totalDay$prop <- totalDay$n/max_count



#Recode Receiver
totalDay$rec<- dplyr::recode(as.character(totalDay$Receiver),"VR2W-121133"=0,"VR2W-112177"=100,"VR2W-110020"=220,"VR2W-111070"=230,"VR2W-110021"=420)

#Order data
# Proportion by receiver 

# plot <- ggplot(data = totalDay,aes(x=rec,y=prop,group=rec))+geom_point()+xlab("Distance (m)")+ylab("Detection proportion")+scale_y_continuous(expand = c(0,0),breaks = seq(0,1,0.1),limits = c(0,1.1)) + scale_x_continuous(expand = c(0, 0), breaks = c(0,190,200,320,420),limits = c(0,450)) 


plot <- ggplot(data = totalDay,aes(x=rec,y=prop,group=rec))+geom_point()+xlab("Distance (m)")+ylab("Detection proportion")+scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1.1),labels = seq(0,100,10)) + scale_x_continuous( breaks = c(0,100,220,230,420),limits = c(0,450)) 

plot <- plot + theme(
  plot.title = element_text(size=14, face="bold"),
  axis.title.x = element_text( size=14, face="bold"),
  axis.title.y = element_text(size=14, face="bold")
)

plot
