#Tobit Models

#Examples of Tobit Analysis
#Example 1.  In the 1980s there was a federal law restricting speedometer readings to no more than 85 mph. 
#            So if you wanted to try and predict a vehicle's top-speed from a combination of horse-power 
#            and engine size, you would get a reading no higher than 85, regardless of how fast the vehicle was 
#            really traveling. This is a classic case of right-censoring (censoring from above) of the data. 
#            The only thing we are certain of is that those vehicles were traveling at least 85 mph.

#Example 2.  A research project is studying the level of lead in home drinking water 
#            as a function of the age of a house and family income. 
#            The water testing kit cannot detect lead concentrations below 5 parts per billion (ppb). 
#            The EPA considers levels above 15 ppb to be dangerous. 
#            These data are an example of left-censoring (censoring from below).

#Example 3.  Consider the situation in which we have a measure of academic aptitude (scaled 200-800) 
#            which we want to model using reading and math test scores, as well as, the type of program the 
#            student is enrolled in (academic, general, or vocational). 
#            The problem here is that students who answer all questions on the academic aptitude test correctly 
#            receive a score of 800, even though it is likely that these students are not "truly" equal in aptitude. 
#            The same is true of students who answer all of the questions incorrectly. 
#            All such students would have a score of 200, although they may not all be of equal aptitude.
library(ggplot2)


dat <- read.csv("http://www.ats.ucla.edu/stat/data/tobit.csv")
dat <- read.csv("Tobit 2013.csv")
summary(dat)

# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 1) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(dat, aes(x = Months.Enrolled, fill=Race.Ethnicity))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=1) +
  stat_function(fun = f, size = 1,
                args = list(var = dat$Months.Enrolled))



#2007 Data

dat <- read.csv("2007 to now SI.csv",header=TRUE)
summary(dat)

# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 1) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(dat, aes(x = dat$race_ethnicity, fill=short_name))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=1) +
  stat_function(fun = f, size = 1,
                args = list(var = dat$enrollment_months))



#2007 to 2015 12-13-15 Data Months Enrolled

dat <- read.csv("2007 to now SI.csv")
summary(dat)

# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 1) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(dat, aes(x = months_served, fill=percent))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=1) +
  stat_function(fun = f, size = 1,
                args = list(var = dat$months_served))



#2007 to 2015 12-13-15 Data Gender

dat <- read.csv("2007 to now SI.csv")
summary(dat)


# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 1) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(dat, aes(x = enrollment_months, fill=gender))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=1) +
  stat_function(fun = f, size = 1,
                args = list(var = dat$enrollment_months))

