#Engaged Particpation 8 years of data

#DDIV

data_2013=read.csv("07-15 tobit data.csv",header=TRUE)
head(data_2013)
str(data_2013)
names(data_2013)

provider <- split(data_2013, data_2013$short_name)
hist(provider$DDIV$percen, breaks=10,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 4000),
     xlab="Months Served by Months Enrolled (percent)",
     ylab="Exited Children",
     main="Engaged Participation in Early Intervention Services for Children Exiting in SFY13
     DDIV",
     xaxt='n')
axis(side=1, at=seq(0,100, 10),
     text(50, 3000, "Average 92.62% / Min 0% / Max 100%",
          cex = .8))
text(50, 2500, "Total Exited: 20744",
     cex = .8)


#Normal Distribution
mean(provider$DDIV$percent)
sd(provider$DDIV$percent)
