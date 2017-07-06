

#propensity score matching

library(readr)
dat <- read_csv("~/Downloads/My_Saved_Schema_2.csv", 
                              col_types = cols(Goal_One_Target_Amount = col_number(), 
                                               MOTZA_Payment_Amount = col_number(), 
                                               `Monthly income` = col_number(), 
                                               `Saving Payment Amount` = col_number()))

#Age convert to years function
library(lubridate)

age <- function(dob, age.day = today(), units = "year", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

#Return ages
my.dob <- as.Date(dat$Age)
agm <- age(my.dob, units = "months") / 365
dat$age_in_years <- paste(floor(agm))

head(dat$age_in_years)

#Goal One Target Amount less Saving Payment Amount and the MOTZA Payment Amount)
#Payments

payments <- (dat$Goal_One_Target_Amount - dat$`Saving Payment Amount`) - dat$MOTZA_Payment_Amount
head(payments)

#diff days
diff_in_days = difftime(datetimes[2], datetimes[1], units = "days") # days

library(MatchIt)
m.out = matchit(stw ~ tot + min + dis,
                data = mydata, method = "nearest",
                ratio = 1)
summary(m.out)
plot(m.out, type = "jitter")
plot(m.out, type = "hist")