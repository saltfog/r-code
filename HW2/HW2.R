dat <- read.csv("femaleMiceWeights.csv")
dat[1:12,2]
dat[13:24,2]
mean(dat[13:24,2]) - mean(dat[1:12,2])

s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)

abline(h=sapply(s, mean), col=1:2)

#new vector for data set

highfat = s[["hf"]]
chow = s[["chow"]]

sample(highfat, 6, replace=TRUE) # replaces values with TRUE
as.numeric(highfat > 30) #converts TRUE to 0 or 1

sum(highfat > 30)
mean(as.numeric(highfat > 30))

population <- read.csv("femaleControlsPopulation.csv") #import data

control <- sample(population[,1],12)
mean(control)

n <- 10000
null <- vector("numeric",n)
for(i in 1:n){
  control <- sample(population[,1],12)
  treatment <- sample(population[,1],12)
  null[i] <- mean(control) - mean(treatment)
}

hist(null)

sampleMean = replicate(10000, mean(sample(population, 12)))
mean(sample(population, 12))
head(sampleMean)



diff = mean(dat[13:24,2]) - mean(dat[1:12,2])
hist(diff)
abline(v=diff, col="red")
abline(v=-diff, col="red")

#HW2 Distro II

library(devtools)
evtools::install_github("hadley/devtools")
install_github("hadley/devtools")
install.packages("devtools")
library(devtools)
install_github("jennybc/gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

#vector of Country and lifeExp 1952
t = gapminder[gapminder$year <= 1952,c(1,4)] #Both country and lifeExp
x = gapminder[gapminder$year <= 1952,c(1)] #country
y = gapminder[gapminder$year <= 1952,c(4)] #lifeExp
hist(y)
stem(y)
plot(ecdf(y), do.points=FALSE, verticals=TRUE)
mean(y <=40) #What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
mean(y <=60) - mean(y <=40) 
mean(y >= 40)

#vector of Country and Population in 1952
t = gapminder[gapminder$year <= 1952,c(1,5)] #Both country and Population
head(t)
x = gapminder[gapminder$year <= 1952,c(5)] #population
hist(log10(x), breaks=20)
sd(log10(x)) #Standard Dev
qqnorm(x) #Q-Q Plot

#2.2 Standardize the population
#vector of x of Log10 of 1952 populations
x = gapminder[gapminder$year <= 1952, c(5)]
x <- log10(x) #new Log10 vector
qqnorm(x) #Q-Q Plot
z <- ((x - mean(x))/sd(x)) #standardize the population vector
qqnorm(z) #Q-Q Plot
abline(0,1)
max(z) #z - score

# 2.3 Normal Distrobution Approximation
x = gapminder[gapminder$year <= 1952, c(5)]
x <- log10(x) #new Log10 vector
F = function(q) pnorm(q, mean=mean(x), sd=sd(x))
n = length(x) #number of countries
(F(7) - F(6)) * n
mean(z)
head(pnorm(z))

#Problem 2.3
x = gapminder[gapminder$year <= 1952, c(5)]
x <- log10(x) #new Log10 vector
qqnorm(x) #Q-Q Plot
n = length(x)
ps = ((1:n) - 0.5)/n
plot(qnorm(ps), sort(x))
qnorm(ps)
