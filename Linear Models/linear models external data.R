
#Load data from external source via download
data("women")

#Take a peak of women data set
head(women)

#Correlation of the women: height vs weight
cor(women$height, women$weight)

#Correlation is 0.9954 this is indeed a strong linear model, due to the correlation value so close to 1.0

#Quick scatter plot
plot(women$weight,women$height)

#Load data from external source via download
data("cars")

#Take a peak of car data set
head(cars)

#Correlation of the cars: speed vs weight
cor(cars$speed, cars$dist)

#Correlation is 0.8068 this is mild linear model, due to the correlation value is < 1.0, but there is a relationship.
plot(cars$speed, cars$dist)
