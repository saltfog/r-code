#set the directory where the files are located
#setwd()

#import the data sets
d1 <- read.csv2("GT_EM_5yr_US.csv", sep = ",") #This file has row with line "Catagory" you need to manually remove it.
d2 <- read.csv2("Wiki_Em.csv", sep = ",")
d3 <- read.csv("daily_price.csv", sep =",")

#check the data
head(d1)
head(d2)
head(d3)

#need libraries
library(dplyr)
#change column name in d1 to match so we can merge then we can change it back after the merge if needed.
names(d1)[names(d1) == 'Week'] <- 'Date'

#check the d1 data
head(d1)

#format the column to be a date, R thinks they are just characters
d1$Date <- as.Date(d1$Date,format="%m/%d/%y")
d2$Date <- as.Date(d2$Date,format="%m/%d/%y")
d3$Date <- as.Date(d3$Date,format="%m/%d/%y")

#merge the data sets
mydata <- merge(d1, d2, by.x = "Date", by.y = "Date", all = TRUE, sort = TRUE)
mydata <- merge(mydata, d3, by.x = "Date", by.y = "Date", all = TRUE, sort = TRUE)

#View the final data set
head(mydata)
tail(mydata)

#Export new csv file
write.csv(mydata, file = "merged.csv", row.names = FALSE)
