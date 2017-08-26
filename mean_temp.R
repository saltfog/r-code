library(readxl)
mean_temp <- read_excel("~/Downloads/All_LGA_Mean_Temp_casey.xlsx")

#remove junk at end of file
mean_temp <- mean_temp[-c(154, 155, 156),]

#transpose
mydata <- t(mean_temp)

#melt id to data
mydata <- melt(mydata, ids=c("code"))

library(WriteXLS)
WriteXLS(mydata, "~/Downloads/final.xls")
