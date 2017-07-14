#set the directory where the files are located
setwd()

#import the data sets
d1 <- read.csv2("GT_EM_5yr_US.csv", sep = ",", na = "NA") #This file has row with line "Catagory" you need to manually remove it.
d2 <- read.csv2("Wiki_Em.csv", sep = ",", na = "NA")
d3 <- read.csv2("daily_price.csv", sep =",", na = "NA")

#check the data
head(d1)
head(d2)
head(d3)

#need libraries
library(dplyr)
#change column name in d1 to match so we can merge then we can change it back after the merge if needed.
names(d1)[names(d1) == 'Week'] <- 'Date'

#check the data
head(d1)

#format the column to be a date, R thinks they are just characters
#d1$Date1 <- as.Date(d1$Date,format="%m/%d/%y")
#d2$Date2 <- as.Date(d2$Date,format="%m/%d/%y")
#d3$Date3 <- as.Date(d3$Date,format="%m/%d/%y")

head(d1)
head(d2)
head(d3)

#merge the data sets
mydata <- merge(d1, d2, d3, by.x = "Date", by.y = "Date", all = TRUE, sort = TRUE)

#View the final data set
View(mydata)
