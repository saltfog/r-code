#extra credit

SurveySp13 <- read.csv("SurveySp13.csv", header = TRUE)

hist(SurveySp13$GPA)

cleanedSurvey <-subset(SurveySp13,SurveySp13$GPA<4.0) 

#Examining relationship between Emory FirstChoice, hrs extracurricular and GPA 
pairs(GPA~Emory_FirstChoice+hrs_extracurricular,data=cleanedSurvey) 

#Calculating a linear model 
m4<-lm(GPA~Emory_FirstChoice+hrs_extracurricular,data=cleanedSurvey)

summary(m4) 
#viewing results 
#Finding confidence interval 
confint(m4) 

#1. What is the estimated linear regression equation? ŷ = 3.47 - 0.188908(Emory_FirstChoiceYes) + 0.006911(hrs_extracurricular) 
#2. The intercept is 3.47 - at 0 hours of extracurriculars and 0 Emory_FirstChoiceYes the GPA is 3.47. The confidence interval is (3.3776318612, 3.56271670) which means it is very different from 0. 
#3  The slope is -0.188908 an the confidence interval is (-0.0002181709, 0.01403948) 
#4  The slope is 0.006911 and the confidence interval is (-0.2937626424, -0.08405350) 
#5  Yes.