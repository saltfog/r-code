#Read ins the data
library(readxl)
sample_assault <- read_excel("~/Downloads/sample_assault.xlsx")

#Concatonate the Year, Month and Day to a workable format
dateformat <- data.frame(sample_assault$year, sample_assault$month, sample_assault$day )
sample_assault$date<- paste( sample_assault$year, sample_assault$month, sample_assault$day, sep="/" )

library(timeDate)
library(bizdays)
#Create new columns weekday, weekend, holiday
sample_assault$weekend <- paste(sample_assault$X__1)
sample_assault$weekday <- paste(sample_assault$X__1)

#Find weekdays and weekends
sample_assault$weekend <- isWeekend(sample_assault$date, wday = 1:5)
sample_assault$weekday <- isWeekday(sample_assault$date, wday = 1:5)

#Replace TRUE and False with Y or N
sample_assault$weekend[sample_assault$weekend==TRUE] <- "Y"
sample_assault$weekday[sample_assault$weekday==TRUE] <- "Y"
sample_assault$weekend[sample_assault$weekend==FALSE] <- "N"
sample_assault$weekday[sample_assault$weekday==FALSE] <- "N"

#Holidays
#24, 25th, 26th Dec (christmas)
#31 Dec and 1st Jan (new years eve and day)
#26 Jan (Australia day)
#25 April (ANZAC day)

sample_assault$holiday<- paste( sample_assault$year, sample_assault$month, sample_assault$day, sep="/" )

sample_assault$holiday[sample_assault$date=="2006/12/24"] = "Y"
sample_assault$holiday[sample_assault$date=="2007/12/24"] = "Y"
sample_assault$holiday[sample_assault$date=="2008/12/24"] = "Y"

sample_assault$holiday[sample_assault$date=="2006/12/31"] = "Y"
sample_assault$holiday[sample_assault$date=="2007/12/31"] = "Y"
sample_assault$holiday[sample_assault$date=="2008/12/31"] = "Y"

sample_assault$holiday[sample_assault$date=="2006/1/1"] = "Y"
sample_assault$holiday[sample_assault$date=="2007/1/1"] = "Y"
sample_assault$holiday[sample_assault$date=="2008/1/1"] = "Y"

sample_assault$holiday[sample_assault$date=="2006/1/26"] = "Y"
sample_assault$holiday[sample_assault$date=="2007/1/26"] = "Y"
sample_assault$holiday[sample_assault$date=="2008/1/26"] = "Y"

sample_assault$holiday[sample_assault$date=="2006/4/25"] = "Y"
sample_assault$holiday[sample_assault$date=="2007/4/25"] = "Y"
sample_assault$holiday[sample_assault$date=="2008/4/25"] = "Y"

sample_assault$holiday[sample_assault$holiday!= "Y"] = "N"

library(WriteXLS)
WriteXLS(sample_assault, "c:/sample_assault_with_weekdays_weekends_holidays.xlsx")

