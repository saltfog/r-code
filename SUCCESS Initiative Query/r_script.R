#2013=============================================================================

data_2013=read.csv("2013_all_providers.csv",header=TRUE)
head(data_2013)
str(data_2013)
names(data_2013)

#Historgram
#hist(data_2013$Percentage)

hist(data_2013$Percentage, breaks=10,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 2500),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation 
     in Early Intervention Services for 
     Children Exiting in SFY13",
     xaxt='n')
axis(side=1, at=seq(0,100, 10),
     text(50, 2500, "Average 86.76% / Min 0% / Max 100%",
          cex = .8),
     text(50, 2400, "Total Exited Children 3740",
          cex = .8))


#Normal Distribution
mean(data_2013$Percentage)
sd(data_2013$Percentage)
min(data_2013$Percentage)
max(data_2013$Percentage)

pnorm(90, mean = 87, sd = 18.6, lower.tail=TRUE)

#2014=============================================================================

data_2014=read.csv("2014_all_providers.csv",header=TRUE)
head(data_2014)
str(data_2014)
names(data_2014)

#Historgram
hist(data_2014$Percentage)

hist(data_2014$Percentage, breaks=10,
     col="lightgreen",
     labels=TRUE,
     ylim=c(0, 2500),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation in Early Intervention Services for Children Exiting in SFY14",
     xaxt='n')
axis(side=1, at=seq(0,100, 10),
     text(50, 2500, "Average 86.97% / Min 0% / Max 100%",
          cex = .8),
     text(50, 2400, "Total Exited Children 3829",
          cex = .8))


#Normal Distribution
mean(data_2014$Percentage)
sd(data_2014$Percentage)

pnorm(90, mean = 87, sd = 18.6, lower.tail=TRUE)


