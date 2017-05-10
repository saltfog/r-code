#More Linear Modeling

#Use the following data set on the Tower of Pisa, study linear model with R.

#a.  input the year data into a variable “years”; use the R function;

xyears <- c(1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987)

#b.  input the lean data into a variable “leans”; use the R function scan() or c();
yleans <- c(2.9642,2.9644,2.9656,2.9667,2.9673,2.9688,2.9696,2.9698,2.9713,2.9717,2.9725,2.9742,2.9757)

#c.   make the scatter plot of years and leans; use the R function plot();
plot(xyears, yleans, xlab="Years", ylab="Leans", main="Tower of Pisa")

#d. guess a linear model that can fit this data, i.e., guess the slope and intercept, and then plot the results with the R function abline(). You can use your function as
# abline(guessed_intercept, guessed_slope) or use help(abline) to find more info about the use of the model.

#The correlation is pretty close to 1.0 so we can say that the relationship is strong.
cor(xyears,yleans)

# We can find the intercept and slope using the command lm
fit <- lm(yleans ~ xyears)

#This model is a positive linear model with a slope of 0.00093 and intercept of 1.1233
plot(xyears, yleans, xlab="Years", ylab="Leans", main="Tower of Pisa")
abline(fit, lwd=2, lty=2, col="red")

#e.figure out R commands to compute the mean squared error of the predictions
#(In this formula, b = the y intercept, and m = slope):
summary(fit)
attributes(fit)
library(Metrics)

#mean squared error
rmse(xyears, yleans)

#f.	compute the mean squared error of the prediction of your linear model with your neighbor’s, see who can come up with the closest fit.
rmse(xyears, yleans)