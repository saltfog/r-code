#Central

data_2013=read.csv("2013_all_providers.csv",header=TRUE)
data_2013[is.na(data_2013)] <- 0
head(data_2013)
str(data_2013)
names(data_2013)

provider <- split(data_2013, data_2013$Provider)
data_2013[is.na(data_2013)] <- 0
hist(provider$Central$Percentage, breaks=10,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 50),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation in Early Intervention Services for Children Exiting in SFY13
     Central",
     xaxt='n')
axis(side=1, at=seq(0,100, 10),
     text(50, 50, "Average 86.15% / Min 16.7% / Max 100%",
          cex = .8))
     text(50, 47, "Total Exited: 61",
          cex = .8)


#Normal Distribution
mean(provider$Central$Percentage)
sd(provider$Central$Percentage)
min(provider$Central$Percentage)
max(provider$Central$Percentage)

