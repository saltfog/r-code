#STATE 2007 to 2015

dat <- read.csv("2007 to now SI.csv",header=TRUE)

head(dat)
str(dat)
names(dat)

hist(dat$months_served, breaks=5,
     #probability = TRUE,
     col="lightblue",
     labels=TRUE,
     ylim=c(0, 8000),
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


data_2007 = read.csv("2007 to now SI.csv")
summary(data_2007)

# Set up the x y variables
install.packages("ggplot2")
library(ggplot2)
enrolled <- (data_2007$enrollment_months)
served <- (data_2007$months_served)
ggplot(data=data_2007, aes(data_2007$enrollment_months)) + 
  geom_histogram(breaks=seq(0, 40, by =1), 
                 col="black", 
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  geom_density(col=1) + labs(title="Histogram for Months Enrolled SFY07 to SFY15") + scale_x_continuous(breaks=seq(0,36,1)) +
  scale_y_continuous(breaks=seq(0,25000,100)) +
  labs(x="Months Enrolled", y="Child Count") +
  geom_vline(aes(xintercept=mean(data_2007$enrollment_months, na.rm=T)),
             color="blue", linetype="dashed", size=1)

