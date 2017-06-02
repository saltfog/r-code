#KOTM

data_2013=read.csv("2013_all_providers.csv",header=TRUE)
data_2013[is.na(data_2013)] <- 0
head(data_2013)
str(data_2013)
names(data_2013)

provider <- split(data_2013, data_2013$Provider)
data_2013[is.na(data_2013)] <- 0
hist(provider$KOTM$Percentage, breaks=10,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 300),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation in Early Intervention Services for Children Exiting in SFY13
     KOTM",
     xaxt='n')
axis(side=1, at=seq(0,100, 10),
     text(50, 300, "Average 83.05% / Min 0% / Max 100%",
          cex = .8))
text(50, 285, "Total Exited: 501",
     cex = .8)


#Normal Distribution
mean(provider$KOTM$Percentage)
sd(provider$DDIV$Percentage)
min(provider$KOTM$Percentage)
max(provider$DDIV$Percentage)