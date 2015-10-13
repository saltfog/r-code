#Jordan

data_2013=read.csv("2013_all_providers.csv",header=TRUE)
data_2013[is.na(data_2013)] <- 0
head(data_2013)
str(data_2013)
names(data_2013)

provider <- split(data_2013, data_2013$Provider)
data_2013[is.na(data_2013)] <- 0
hist(provider$Jordan$Percentage, breaks=10,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 400),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation in Early Intervention Services for Children Exiting in SFY13
     Jordan",
     xaxt='n')
axis(side=1, at=seq(0,100, 10),
     text(50, 400, "Average 89.46% / Min 14.3% / Max 100%",
          cex = .8))
text(50, 385, "Total Exited: 502",
     cex = .8)


#Normal Distribution
mean(provider$Jordan$Percentage)
sd(provider$DDIV$Percentage)
min(provider$Jordan$Percentage)
max(provider$DDIV$Percentage)