
library(readxl)
sample_assault <- read_excel("~/r-code/Data Mining/sample_assault.xlsx", 
                             na = "NA")

#read in temp.csv
library(readr)
temp <- read_csv("~/r-code/Data Mining/temp.csv", 
                 col_types = cols(`Bureau of Meteorology station number` = col_skip(), 
                                  `Days of accumulation of maximum temperature` = col_skip(), 
                                  `Product code` = col_skip(), Quality = col_skip()), 
                 na = "NA")

#check the first few row of the imported data
mydata <- merge(temp, sample_assault, by=c("Year","Month","Day"), all=TRUE)

library(WriteXLS)
WriteXLS(mydata, "c:/sample_assault_with_temps.xlsx")
