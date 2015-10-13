#Davis

data_2013=read.csv("2013_all_providers.csv",header=TRUE)
data_2013[is.na(data_2013)] <- 0
head(data_2013)
str(data_2013)
names(data_2013)

provider <- split(data_2013, data_2013$Provider)
data_2013[is.na(data_2013)] <- 0
hist(provider$Davis$Percentage, breaks=10,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 250),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation in Early Intervention Services for Children Exiting in SFY13
     Davis",
     xaxt='n')
axis(side=1, at=seq(0,100, 10),
     text(50, 250, "Average 88.68% / Min 0% / Max 100%",
          cex = .8))
text(50, 240, "Total Exited: 311",
     cex = .8)


#Normal Distribution
mean(provider$Davis$Percentage)
sd(provider$Davis$Percentage)
min(provider$Davis$Percentage)
max(provider$Davis$Percentage)