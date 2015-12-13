#STATE 2007 to 2015

data_2013=read.csv("2007 to now SI.csv",header=TRUE)
data_2013[is.na(data_2013)] <- 0
head(data_2013)
str(data_2013)
names(data_2013)

hist(data_2013$percent, breaks=10,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 15000),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation 
     in Early Intervention Services for 
     Children Exiting in SFY13")
axis(side=1, at=seq(0,100, 5),
     text(50, 2500, "Average 86.76% / Min 0% / Max 100%",
          cex = .8),
     text(50, 2400, "Total Exited Children 3740",
          cex = .8))
text(50, 285, "Total Exited: 501",
     cex = .8)


#Normal Distribution
mean(data_2013$percent)
sd(provider$DDIV$Percentage)
min(provider$KOTM$Percentage)
max(provider$DDIV$Percentage)

