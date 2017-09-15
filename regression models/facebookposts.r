

setwd(getwd())
library(readr)
library(ggplot2)
library(class)
library(caret)
options(scipen = 999) # removes scientfic notation
dd <- read_csv("~/Desktop/fb/P1TrainingData.csv", 
                           col_names = FALSE)
head(dd)
tail(dd)

#convert to a data frame
dd <- as.data.frame(dd)

#look at the variable names
str(dd)

#looks like we will need to rename the columns to keep things organized according to the appendix.

dd <- subset(dd, select = -c(X6:X30))

#change column names to match the appendix
names(dd)[names(dd)=="X1"] <- "TargetVariable"
names(dd)[names(dd)=="X2"] <- "PagePopularity"
names(dd)[names(dd)=="X3"] <- "Checkins"
names(dd)[names(dd)=="X4"] <- "PageTalkingAbout"
names(dd)[names(dd)=="X5"] <- "PageCatagory"
names(dd)[names(dd)=="X31"] <- "Comments"
names(dd)[names(dd)=="X32"] <- "Comments24"
names(dd)[names(dd)=="X33"] <- "Comments2448"
names(dd)[names(dd)=="X34"] <- "Comment24Post"
names(dd)[names(dd)=="X35"] <- "CommentDiff2448"
names(dd)[names(dd)=="X36"] <- "BaseTime"
names(dd)[names(dd)=="X37"] <- "PostLength"
names(dd)[names(dd)=="X38"] <- "PostShareCount"
names(dd)[names(dd)=="X39"] <- "PostPromotionStatus"
names(dd)[names(dd)=="X40"] <- "Hours"
names(dd)[names(dd)=="X41"] <- "PostPublishedWeekday"
names(dd)[names(dd)=="X42"] <- "BaseDateTimeWeekday"

#check that the column name where changed
str(dd)

#let get some descriptive stats
summary(dd$Comments) 
summary(dd$BaseTime)

#quick look at the data with a pairs plot and scatter plot to see any patterns in comment volume
plot(dd$BaseTime, dd$Comments)

#shows some linear regression
pairs(~ dd$Comments + dd$Comments24 + dd$Comments2448 + dd$Comment24Post)

#total comments vs 24hr, 24-48, 24-48 diff
qplot(dd$Comments, dd$Comments24)
qplot(dd$Comments, dd$Comments2448)
qplot(dd$Comments, dd$CommentDiff2448)
qplot(dd$PageCatagory, dd$Comments)

#histogram on Weekday values
qplot(dd$BaseDateTimeWeekday, dd$Comments) 
#looks like the number of comments is clustered under 1000 comments, you would think we would see a spike on the WeekEnds. 

#comments vs baseline
qplot(dd$BaseTime, dd$Comments)

#scatter plot of number of comments vs the basetime
ggplot(dd, aes(x=dd$BaseTime, y=dd$Comments)) +
  geom_point(color="blue") +
  labs(x="Hours", y="Comments", title="Comments from Base Time") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20) ) +
  geom_smooth(colour="red", method="lm")

#ggplot shows the comment volume of time it linear in fashion

#attach will enable the ability to only the variable not the whole dd$Variable just Variable
attach(dd)

#need to determine which variables are statistically significant to use in the model.
#lets examine the data before creating a model

#3.1 ==========================================================================================
#lets subset the data and look at there corvariance
#total comments in relation to basetime
ef <- subset(dd, select = c("BaseTime", "Comments"))
summary(ef)
cor(ef)

ef <- subset(dd, select = c("Comments", "PagePopularity")) # strong corelation
summary(ef)
cor(ef)

ef <- subset(dd, select = c("Comments", "PageCatagory")) # weak corelation
summary(ef)
cor(ef)

ef <- subset(dd, select = c("Comments", "PostLength")) # weak corelation
summary(ef)
cor(ef)

ef <- subset(dd, select = c("Comments", "PostShareCount")) # strong corelation
summary(ef)
cor(ef)

ef <- subset(dd, select = c("Comments", "Checkins")) # strong corelation
summary(ef)
cor(ef)

ef <- subset(dd, select = c("Comments", "PageTalkingAbout")) # strong corelation
summary(ef)
cor(ef)

#lets check out if all catagories are in the data set and if there is a trend
ggplot(dd, aes(x=dd$PageCatagory, dd$Comments)) +
  geom_point(color="blue") +
  labs(x="Page Catagory", y="Comments", title="Page Catagory vs Comments") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20) ) +
  geom_smooth(colour="red", method="lm")

#since this is a simple regression we will only use one variable 
#create a model by page talking about showed the strongest correlation
model.lm <- lm(Comments ~ PageTalkingAbout, data = dd)
summary(model.lm)

plot(model.lm)

#Regression equation
#CommentsVolume = 37.513321847 + 0.000406405 * Comments

#predictor pagetalkingabout predict will tell you how many comments in x from baseline 
#BaseTime = x
newdata = data.frame(PageTalkingAbout = 64641) #67 comments
predict(model.lm, newdata)

newdata = data.frame(PageTalkingAbout = 57833) #61 comments
predict(model.lm, newdata)

newdata = data.frame(PageTalkingAbout = 1242488) #542 comments
predict(model.lm, newdata)

newdata = data.frame(PageTalkingAbout = 1) #37 comments
predict(model.lm, newdata)

#3.2 ============================================================================================
# Multiple Regression

model2 <- lm(Comments ~ PagePopularity + Checkins + PageTalkingAbout + PageCatagory + PostLength, data = dd)
summary(model2)
#the summary show that Checkins and PostLength doesn't relate to comment volume.
plot(model2)

#ouput the coefficients and show fitted values
model2

# y = 51.2161358839 + -0.0000030748  + 0.0002137216 + 0.0005005108 + -0.6195993744 + 0.0009154428

#The output shows that F = 322.5(p < 0.00000000000000022), indicating that we should clearly reject the null hypothesis that the variables
#PostLength, Checkins collectively have no effect on Comment Volume. The results also show that the variable PagePopularity is significant controlling for
#the variable PageTalkingAbout(p = 67.044), as is Comment Volume controlling for the variable Comments(p = 46.565).
#In addition, the output also shows that R2 = 0.1336 and R2adjusted = 0.1335.
#We could perform a partial F-test by creating a full model and a reduced model then compare them with an ANOVA analysis

#3.3 ==============================================================================================================
#Free form
#lets create a new model and remove Checkins and Post Length and PageTalkingAbout
model3 <- lm(Comments ~ PagePopularity + PageTalkingAbout, data = dd)
summary(model3)

#Prediction Interval 95% CI
predict(model3, data.frame(PagePopularity = 456, PageTalkingAbout = 1242488), interval = "confidence")

#A 95 % confidence interval is given by(679.576, 714.3349)
#the numbers above indicate that with a page popularity of 456 and page talking about of 1224488 will get at least 679-714 comments from baseline

predict(model3, data.frame(PagePopularity = 162624443, PageTalkingAbout = 1242488), interval = "prediction")

#A 95 % prediction interval is given by(-90.7484, 415.2865) . Note that this is quite a bit wider than the confidence interval, 
#indicating that the variation about the mean is fairly large.

##Conclusion
#With the three models complete the best predictors for comments volume from basetime are 
#Page Popularity and Page Talking Abount
