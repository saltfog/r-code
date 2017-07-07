#propensity score matching

library(readr)

dat <- read_csv("~/r-code/#FO4108AAB7A08/my_saved_schema.csv")

#Age convert to years function
library(lubridate)

age <- function(dob, age.day = today(), units = "year", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

#Return ages
my.dob <- as.Date(dat$age)
agm <- age(my.dob, units = "months") / 365
dat$age_in_years <- paste(floor(agm))

head(dat$age_in_years)

#Goal One Target Amount less Saving Payment Amount and the MOTZA Payment Amount)
#Payments

dat$payments <- paste((dat$goal_one_target_amount - dat$saving_payment_amount) - dat$MOTZA_payment_amount)
head(dat$payments)

#diff days
#diff_in_days = difftime(datetimes[2], datetimes[1], units = "days") # days

#Need binary data for matching

#dat$adviser <- paste(as.integer(as.logical(dat$adviser)))
#dat$crew <- paste(as.integer(as.logical(dat$crew)))
#dat$full_time_work <- paste(as.integer(as.logical(dat$full_time_work)))

library(MatchIt)  
m.out = matchit(dat$adviser ~ dat$age_in_years + dat$quizzes_passed + dat$goal_one_progress,
                data = dat, method = "nearest",
                ratio = 1)

summary(m.out, standardize = TRUE)
plot(m.out, type = "hist")
plot(m.out, type = "jitter")

library(Zelig)
m.data <- match.data(m.out)
z.out <- zelig(Y ~ m.data$adviser + x1 + x2, model = mymodel, data = m.data)




# weighted diff in means
weighted.mean(mdata$adviser[mdata$treat == 1], mdata$weights[mdata$treat==1])
weighted.mean(mdata$re78[mdata$treat==0], mdata$weights[mdata$treat==0])

# weighted least squares without covariates
zelig(re78 ~ treat, data = m.data, model = "ls", weights = "weights")

zelig(formula = re78 ~ treat, model = "ls", data = m.data, weights =
          "weights")


