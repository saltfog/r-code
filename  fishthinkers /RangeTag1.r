
library(dplyr)
library(ggplot2)
library(lubridate)

#setwd()

max_count <- 4768

library(readr)
df <- read_csv("~/r-code/#FO384AF5F243/Rangetag1.csv")

#Second column is UTC+10 which is required time format (FYI the formatting is not needed in this case) the count down below takes care
#of stripping the timestamp
df$date <- as.Date(df$`Date.and.Time.(UTC+10:00)`,format="%d/%m/%Y")

#Subset the data
df <- df[,c(3,12)]

#Convert the data (df) to a data.frame
df <- data.frame(df, stringsAsFactors = TRUE)

#check for missing values (return false or true)
#if this returned true then we would replace the missing values.
missing(df)

head(df)
dim(df)

#Count by dates with pipe %>%> = df <- (df,count) return count as "n"
df <- df %>% count(date,Receiver)

#Determine the daily proportions
df$prop <- df$n/max_count

#Reorder factors by distance
df$Receiver <- factor(df$Receiver, levels=c("VR2W-110021","VR2W-111070","VR2W-110020","VR2W-112177","VR2W-121133"))

#Recode change the catagorical data to numeric 
df$rec<- dplyr::recode(as.character(df$Receiver),"VR2W-110021"=0,"VR2W-111070"=190,"VR2W-110020"=200,"VR2W-112177"=320,"VR2W-121133"=420)

#Tell R that Receiver is numeric data
df$Receiver <- as.numeric(df$Receiver)

#exclude day one and last day
df <- df[ which(df$date!='2014-03-11'),]
df <- df[ which(df$date!='2014-01-05'),]

# Graph
ggplot(df, aes(x=rec, y=prop)) +
  geom_smooth(span= 0.9) +
  geom_point() +
  labs(x="Receiver", y="prop", title="Range Tag 1") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20) )
