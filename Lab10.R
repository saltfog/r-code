# Lab 10

#Import the data into RStudio
SurveySp13 <- read.csv("SurveySp13.csv", header = TRUE)

#1(a) 
# Use a histogram to see the distrbution.
# The GPA is left skewed 
hist(SurveySp13$GPA)

#1(b)
#Creating a clean set due to some GPA's being higher than 4.0
cleanedSurvey <-subset(SurveySp13,SurveySp13$GPA<4.0) 
hist(cleanedSurvey$GPA)
#Data contains 200 observations.

#2(a)
#Plotting histogram for hrs extracurriculars 
hist(SurveySp13$hrs_extracurricular)
#Data is right skewed.

#2(b)
#Plotting a scatter plot to show the relationship between hrs extracurricular and GPA 
plot(cleanedSurvey$GPA,cleanedSurvey$hrs_extracurricular)
#The scatter plot showed that there may be a slight positive correlation. Higher GPAs have more extracurricular involvement.

#2(c)
#Finding correation between hrs extracurricular and GPA 
cor(cleanedSurvey$GPA,cleanedSurvey$hrs_extracurricular) 
cor.test(cleanedSurvey$GPA,cleanedSurvey$hrs_extracurricular) 
#The correlation is 0.1459643. This is a weak positive correlation. 
#The 95% confidence interval is (0.007372618, 0.279054151) and does not include 
#zero, which means the null hypothesis (p=0) is rejected.

#3(a)
#Estimating regression model 
model1<-lm(cleanedSurvey$hrs_extracurricular~cleanedSurvey$GPA) 
summary(model1) 
#viewing regressing results ŷ = -1.792 + 2.764GPA

#3(b)
#Get confidence interval for intercept and slope 
confint(model1) 
#The confidence interval for the intercept is (-10.8649885, 7.281313) 
#The intercept is not significantly different than zero since the p-value > 0.05 also since the confidence interval includes 0

#3(c)
#Get confidence interval for intercept and slope. 
confint(model1) 
#The confidence interval for the slope is (0.1386326, 5.390075). 
#Since it does not include 0 we can conclude the slope is significantly different (greater than) 0

#3(d)
#The R 2 of the model is 0.02131. 
#This is quite low and so the association between hrs extrscurricula and GPA is rickety.

#3(e)
#The residual standard error is 7.303 on 198 degrees of freedom. It is moderately high.

#4(a)
#Checking model assumptions using residual plots 
model1$residuals 
#regular residuals 
resid(model1) 
#regular residuals 
rstandard(model1) 
#regular residuals 
predict(model1) 
#regular residuals 
hist(rstandard(model1)) 
qqnorm(rstandard(model1)) 
#produce qq plot 
qqline(rstandard(model1)) 
library(mosaic) 
favstats(rstandard(model1)) 
#It does not appear that the residuals are approximately normally distributed. They are right skewed.

#4(b)
#scatter plot of hrs extracurricular and standardized residuals 
plot(cleanedSurvey$hrs_extracurricular,rstandard(model1))

#Is there any evidence of a non-linear trend in the residuals? 
# No
#Is there any evidence of non-constant variance in the residuals? 
# No
