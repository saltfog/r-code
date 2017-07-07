

#propensity score matching

library(readr)
dat <- read_csv("~/r-code/#FO4108AAB7A08/My_Saved_Schema_2.csv", 
                              col_types = cols(Age = col_character(), 
                                               Goal_One_Target_Amount = col_number(), 
                                               MOTZA_Payment_Amount = col_number(), 
                                               `Monthly income` = col_number(), 
                                               `Saving Payment Amount` = col_number()),
                na = "0")

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

#Need binary data for matching

dat$`Adviser?` <- paste(as.integer(as.logical(dat$`Adviser?`)))
dat$`Crew?` <- paste(as.integer(as.logical(dat$`Crew?`)))
dat$`Full time work?` <- paste(as.integer(as.logical(dat$`Full time work?`)))

library(MatchIt)
m.out = matchit(dat$`Adviser?` ~ dat$age_in_years + dat$Gender + dat$Relationship + dat$Housing,
                data = dat, method = "nearest",
                ratio = 1)

summary(m.out)
plot(m.out, type = "jitter")
plot(m.out, type = "hist")