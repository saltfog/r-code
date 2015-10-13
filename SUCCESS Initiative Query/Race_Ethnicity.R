#2013=============================================================================

data_2013=read.csv("SUCCESS Initiative Query SFY 13.csv",header=TRUE)
data_2013[is.na(data_2013)] <- 0
head(data_2013)
str(data_2013)
names(data_2013)

race <- split(data_2013, data_2013$Race)
data_2013[is.na(data_2013)] <- 0

allraces <- rbind(race$White,race$`All Other Races`)
white <- race$White
other <- race$`All Other Races`


par(mfrow=c(1,2))
hist(race$White$Percent, breaks=10,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 1800),
     xlab="",
     ylab="Exited Children (White)",
     main="")
axis(side=1, at=seq(0,100, 10))
#     text(50, 1600, "Average 86.76% / Min 0% / Max 100%",
#          cex = .8),
#     text(50, 1500, "Total Exited Children 3740",
#         cex = .8))

hist(race$`All Other Races`$Percent, breaks=10,
     col="lightgreen", 
     xlab="",
     ylab="Exited Children (All Other Races)",labels=TRUE,
     main="")
axis(side=1, at=seq(0,100, 10))

mean(race$White$Percent)
mean(race$`All Other Races`$Percent)


