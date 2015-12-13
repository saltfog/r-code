
# Install ggplot2 package
install.packages("ggplot2")
install.packages("sm")

# Load the package
library(ggplot2)
library(sm)

# Read in the data
data_2013=read.csv("SUCCESS Initiative Query SFY 13.csv",header=TRUE)


mean(exitage)
mean(enrolled)

# Scatter Plot
plot(exitage, enrolled)

# Fit lines
abline(lm(enrolled~exitage), col="red")
lines(lowess(exitage,enrolled), col="blue")

# Bivariate Analysis
plot(enrolled)

# Bivariate Analysis without outliners
qqplot(enrolled,exitage)

# Children in Enrolled
qplot(data_2013$Months.Enrolled,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for Months Enrolled", 
      xlab = "Months Served",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(0,40))

mean(data_2013$Months.Enrolled)
mean(data_2013$Months.Served)
mean(data_2013$Age.at.Exit)
sd(data_2013$Months.Enrolled)

points(seq(min(data_2013$Months.Enrolled), max(data_2013$Months.Enrolled), length.out=500),
       dnorm(seq(min(data_2013$Months.Enrolled), max(data_2013$Months.Enrolled), length.out=500),
             mean(data_2013$Months.Enrolled), sd(data_2013$Months.Enrolled)), type="l", col="red")

ggplot(data=data_2013, aes(data_2013$Months.Enrolled)) + 
  geom_histogram(breaks=seq(0, 40, by =1), 
                 col="black", 
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  geom_density(col=1) + labs(title="Histogram for Months Enrolled SFY13") + scale_x_continuous(breaks=seq(0,36,1)) +
  scale_y_continuous(breaks=seq(0,350,10)) +
  labs(x="Months Enrolled", y="Child Count")


ggplot(data=data_2013, aes(data_2013$Months.Served)) + 
  geom_histogram(breaks=seq(0, 40, by =1), 
                 col="gray",
                 labels=TRUE,
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "gray", high = "blue") +
  geom_density(col=1) + labs(title="Histogram for Months Served SFY13") + scale_x_continuous(breaks=seq(0,36,1)) +
  scale_y_continuous(breaks=seq(0,350,10)) +
  labs(x="Months Served", y="Child Count")

ggplot(data=data_2013, aes(data_2013$Age.at.Exit)) + 
  geom_histogram(breaks=seq(0, 40, by =1), 
                 col="gray",
                 labels=TRUE,
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "gray", high = "blue") +
  geom_density(col=1) + labs(title="Histogram for Exit Age SFY13") + scale_x_continuous(breaks=seq(0,36,1)) +
  scale_y_continuous(breaks=seq(0,1500,100)) +
  labs(x="Months Served", y="Child Count")

ggplot(data_2013, aes(x = data_2013$Months.Served, y = data_2013$Months.Enrolled)) + geom_point()


data_2013 <-read.csv("SUCCESS Initiative Query SFY 13 Federal Codes.csv",header=TRUE)
exitreasons <- factor(data_2013$Federal.Exit.Reason, levels=c(1,2,3,4,5,6,7,8,9), labels=c("1", "2", "3", "4","5","6","7","8","9"))
colfill<- c(2:(2+length(levels(data_2013$Federal.Exit.Reason))))
library(sm)
par(lwd=3)
sm.density.compare(data_2013$Months.Enrolled, data_2013$Federal.Exit.Reason, xlab="Months Enrolled")
#legend(locator(1), levels(data_2013$Federal.Exit.Reason), fill=colfill)
title(main="Distribution by Exit Reason")



sm.density.compare(enrolled, exitage, xlab="Months Enrolled")
title(main="Distribution by Exit Age")

data_2007 = read.csv("2007 to now SI.csv")
summary(data_2007)

# Set up the x y variables
enrolled <- (data_2007$enrollment_months)
served <- (data_2007$months_served)
ggplot(data=data_2007, aes(data_2007$enrollment_months)) + 
  geom_histogram(breaks=seq(0, 40, by =1), 
                 col="black", 
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  geom_density(col=1) + labs(title="Histogram for Months Enrolled SFY07 to SFY15") + scale_x_continuous(breaks=seq(0,36,1)) +
  scale_y_continuous(breaks=seq(0,25000,100)) +
  labs(x="Months Enrolled", y="Child Count")

