#DDIV

data_2013=read.csv("2013_all_providers.csv",header=TRUE)
data_2013[is.na(data_2013)] <- 0
head(data_2013)
str(data_2013)
names(data_2013)

provider <- split(data_2013, data_2013$Provider)
data_2013[is.na(data_2013)] <- 0
hist(provider$DDIV$Percentage, breaks=10,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 900),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation in Early Intervention Services for Children Exiting in SFY13
     DDIV",
     xaxt='n')
axis(side=1, at=seq(0,100, 10),
     text(50, 900, "Average 92.68% / Min 0% / Max 100%",
          cex = .8))
text(50, 845, "Total Exited: 908",
     cex = .8)


#Normal Distribution
mean(provider$DDIV$Percentage)
sd(provider$DDIV$Percentage)
min(provider$DDIV$Percentage)
max(provider$DDIV$Percentage)

