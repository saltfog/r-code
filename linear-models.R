
#1
#import the data set

available <-c(5.0,
              11.6,
              16.6,
              16.2,
              18.3,
              13.8)

average_rent <-c(72.04,
                 53.57,
                 42.98,
                 38.40,
                 38.92,
                 41.31)

#Scatter plot
plot(available, average_rent, xlab= "Available", ylab = "Average Rent ($/(square foot))", main = "Available vs Average Rent / Sqft")

#2
#Compute the correlation coefficient
cor(available, average_rent)

#Correlation coefficient is -0.9602503 and the variables are negatively linearly and strength of relationship seems moderate.

#3
#Find the least squares regression line
abline(lm(average_rent ~ available), lwd=2, lty=2, col="red")

#Intercept and slope
#30.5195       -0.3538

#4Estimate office rent when the availability rate is 8% is around $63 per Sqft
