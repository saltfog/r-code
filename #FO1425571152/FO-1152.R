#Count data and create new columns for the counts As well as count of assaults, 
#i'd like to also have count of; 
#- count male 
#- count female 
#- count alcohol (Y) 
#- count alcohol (N) 
#- count domestic violence (Y) 
#- count domestic violence (N) 
#So it should look something like 
#date count DV_Y DV_N Male Female Alcohol_Y Alcohol_N 01/01/2016 34,000 4456 29600 25000 9000 30000 8000

library(readxl)
sample_assault <- read_excel("~/r-code/#FO1425571152/sample_assault.xlsx")

#Quick look at the data
summary(sample_assault)

d1 <- as.matrix.data.frame(table(sample_assault$date, sample_assault$gender))
d2 <- as.matrix.data.frame(table(sample_assault$date, sample_assault$Alcohol.Related))
d3 <- as.matrix.data.frame(table(sample_assault$date, sample_assault$DV.Related))
d4 <- as.matrix(table(sample_assault$date))

#New data.frames to hold the counts

sample_assault_counts <- data.frame(d1,d2,d3,d4)

#Rename columns
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "X1")] <- "1"
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "X2")] <- "2"
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "X3")] <- "3"
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "X4")] <- "NA"
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "X1.1")] <- "Alcohol_Related_N"
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "X2.1")] <- "Alcohol_Related_Y"
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "X1.2")] <- "DV_Related_N"
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "X2.2")] <- "DV_Related_Y"
colnames(sample_assault_counts)[which(names(sample_assault_counts) == "d4")] <- "Assaults"

write.csv(sample_assault_counts, file = "~/r-code/#FO1425571152/MyData.csv")
MyData <- read_csv("~/r-code/#FO1425571152/MyData.csv")
colnames(MyData)[which(names(MyData) == "X1")] <- "date"
colnames(MyData)[which(names(MyData) == "X5")] <- "NA"

sample_assault_final <- unique(merge(MyData, sample_assault, by=c("date"), all=TRUE))

WriteXLS(sample_assault_final, "~/r-code/#FO1425571152/sample_assault_final.xlsx")

#Takes a couple of minutes to read in.....
sample_assault_final <- read_excel("~/r-code/#FO1425571152/sample_assault_final.xlsx", 
                                   col_types = c("text", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "skip", "skip", "skip", 
                                                 "skip", "numeric", "skip", "skip", 
                                                 "skip", "skip", "skip", "skip", 
                                                 "skip", "skip", "skip", "skip", 
                                                 "skip", "skip", "skip", "skip", 
                                                 "text", "text", "text"))

sample_assault_final <- unique(sample_assault_final)
WriteXLS(sample_assault_final, "~/r-code/#FO1425571152/sample_assault_final.xlsx")
