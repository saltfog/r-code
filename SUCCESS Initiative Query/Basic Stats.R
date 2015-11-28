
# Install ggplot2 package
install.packages("ggplot2")

# Load the package
library(ggplot2)

# Read in the data
data_2013=read.csv("SUCCESS Initiative Query SFY 13.csv",header=TRUE)

# Set up the x y variables
enrolled <- (data_2013$Months.Enrolled)
served <- (data_2013$Months.Served)

# Bivariate Analysis
plot(enrolled,served)

# Bivariate Analysis without outliners
qqplot(enrolled,served)

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
sd(data_2013$Months.Enrolled)

ggplot(data=data_2013, aes(data_2013$Months.Enrolled)) + 
  geom_histogram(breaks=seq(0, 40, by =1), 
                 col="gray", 
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  geom_density(col=1) + labs(title="Histogram for Months Enrolled SFY13") + scale_x_continuous(breaks=seq(0,36,1)) +
  scale_y_continuous(breaks=seq(0,350,10)) +
  labs(x="Months Served", y="Child Count")


ggplot(data=data_2013, aes(data_2013$Months.Served)) + 
  geom_histogram(breaks=seq(0, 40, by =1), 
                 col="gray",
                 labels=TRUE,
                 aes(fill=..count..)) +
  scale_fill_gradient("Count", low = "gray", high = "blue") +
  geom_density(col=1) + labs(title="Histogram for Months Served SFY13") + scale_x_continuous(breaks=seq(0,36,1)) +
  scale_y_continuous(breaks=seq(0,350,10)) +
  labs(x="Months Served", y="Child Count")


