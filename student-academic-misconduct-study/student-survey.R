# Lets import the data set SurveySP16.csv

SurveySP16 <- read.csv("SurveySP16.csv", header = TRUE)

# Let take a look at data to see if everyting imported correctly

SurveySP16$gender2<-SurveySP16$gender
table(SurveySP16$gender)
GenderTable<-table(SurveySP16$gender)
prop.table(GenderTable)

summary(survey$school.year)
summary(survey$gender)
summary(survey$num_class)
plot(survey$num_class)

# dichotomous variable
#Gender
#exam_prep
#num_credithrs







