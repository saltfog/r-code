#You need to install two package dplyr,ggplot2

library(dplyr)
library(ggplot2)
library(lubridate)

#=======================Start Range Tag 2===============================
max_count <- 701
df0 <- read.csv("~/r-code/ fishthinkers /Rangetag2.csv")
#second column is UTC+10 which is required time format
df0$date <- as.Date(df0$Date.and.Time..UTC.10.00.,format="%d/%m/%Y")

df0 <- df0[,c(3,12)]

df0 <- data.frame(df0, stringsAsFactors = TRUE)

#check for missing values (false or true)
missing(df0)
head(df0)
dim(df0)

#Count by date
df0 <- df0 %>% count(date,Receiver)
df0$prop <- df0$n/max_count

#Reorder factors
df0$Receiver <- factor(df0$Receiver, levels=c("VR2W-121133","VR2W-112177","VR2W-110020","VR2W-111070","VR2W-110021"))
#Recode 
df0$rec<- dplyr::recode(as.character(df0$Receiver),"VR2W-121133"=0,"VR2W-112177"=100,"VR2W-110020"=220,"VR2W-111070"=230,"VR2W-110021"=420)

df0$Receiver <- as.numeric(df0$Receiver)

#exclude day one and last day
attach(df0)
df0 <- df0[ which(df0$date!='2014-03-11'),]
df0 <- df0[ which(df0$date!='2014-06-19'),]
#=======================End of Range Tag 2===============================

#=======================Start Range Tag 1===============================
max_count <- 4768

library(readr)
df1 <- read_csv("~/r-code/ fishthinkers /Rangetag1.csv")

#Second column is UTC+10 which is required time format (FYI the formatting is not needed in this case) the count down below takes care
#of stripping the timestamp
df1$date <- as.Date(df1$`Date.and.Time.(UTC+10:00)`,format="%d/%m/%Y")

#Subset the data
df1 <- df1[,c(3,12)]

#Convert the data (df) to a data.frame
df1 <- data.frame(df1, stringsAsFactors = TRUE)

#check for missing values (return false or true)
#if this returned true then we would replace the missing values.
missing(df1)
head(df1)
dim(df1)

#Count by dates with pipe %>%> = df <- (df,count) return count as "n"
df1 <- df1 %>% count(date,Receiver)

#Determine the daily proportions
df1$prop <- df1$n/max_count

#Reorder factors by distance
df1$Receiver <- factor(df1$Receiver, levels=c("VR2W-110021","VR2W-111070","VR2W-110020","VR2W-112177","VR2W-121133"))

#Recode change the catagorical data to numeric 
df1$rec<- dplyr::recode(as.character(df1$Receiver),"VR2W-110021"=0,"VR2W-111070"=190,"VR2W-110020"=200,"VR2W-112177"=320,"VR2W-121133"=420)

#Tell R that Receiver is numeric data
df1$Receiver <- as.numeric(df1$Receiver)

#exclude day one and last day
df1 <- df1[ which(df1$date!='2014-03-11'),]
df1 <- df1[ which(df1$date!='2014-05-01'),]

#df <- df[ which(df$date!='2014-03-11' - '2014-05-01'),]


#=======================End Range Tag 1===============================

# Graph


p1 <- ggplot(df0, aes(x=rec, y=prop)) +
  geom_smooth(span= 0.9) +
  geom_point() +
  labs(x="Receiver", y="prop", title="Range Tag 2") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20) )

p2 <- ggplot(df1, aes(x=rec, y=prop)) +
  geom_smooth(span= 0.9) +
  geom_point() +
  labs(x="Receiver", y="prop", title="Range Tag 1") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20) )

p <- ggplot() +
geom_point(data = df0, aes(x=rec, y=prop), shape = 16) +
geom_point(data = df1, aes(x=rec, y=prop), colour = 'red', size = 1, shape = 1) +
labs(x="Receiver", y="prop", title="Range Tag 1 and 2") +
geom_smooth(span= 0.9) +
geom_smooth(span=0.9) +
theme(plot.title = element_text(hjust = 0.5, size = 20) )
p

