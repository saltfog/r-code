#propensity score matching
#propensity score–defined as the probability of receiving the treatment (must be a binary) given the covariates–is a key tool
#Data Prep
#Perform the Matching
#Post Balance Analysis

data(lalonde)

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

cat(dat$age_in_years)

#Change a numeric variables to categorical variables
# Monthly Income
min(dat$monthly_income)
max(dat$monthly_income)
mean(dat$monthly_income)
summary(dat$monthly_income)

dat$monthly_income_cat <- cut(dat$monthly_income, breaks = c(0.00,10000.00,20000.00,30000.00,40000.00,50000.00),
                              labels = c("10K","20K","30K","40K","50K"), right = FALSE)

min(dat$quizzes_passed)
max(dat$quizzes_passed)
mean(dat$quizzes_passed)
summary(dat$quizzes_passed)

dat$quizzes_passed_cat <- cut(dat$quizzes_passed, breaks = c(0,50,100),
                              labels = c("0","1"), left = TRUE)

min(dat$goal_one_progress)
max(dat$goal_one_progress)
mean(dat$goal_one_progress)
summary(dat$goal_one_progress)

dat$goal_one_progress_cat <- cut(dat$goal_one_progress, breaks = c(0,20,30,40,50,60,70,80,90,100),
                              labels = c("A","B","C","D","E","F","G","H","I"), right = FALSE)

#Goal One Target Amount less Saving Payment Amount and the MOTZA Payment Amount)
#Payments

#dat$payments <- paste((dat$saving_payment_amount - dat$MOTZA_payment_amount) - dat$goal_one_target_amount)
#head(dat$payments)

min(dat$payments)
max(dat$payments)
mean(dat$payments)
summary(dat$payments)

#dat$quizzes_passed_cat <- cut(dat$quizzes_passed, breaks = c(0,50,150),
#                              labels = c("<50",">50"), right = FALSE)

#diff days
#diff_in_days = difftime(datetimes[2], datetimes[1], units = "days") # days

#Need binary data for matching

#dat$adviser <- paste(as.integer(as.logical(dat$adviser)))
#dat$crew <- paste(as.integer(as.logical(dat$crew)))
#dat$full_time_work <- paste(as.integer(as.logical(dat$full_time_work)))

library(MatchIt) 
m.out = matchit(dat$gender ~ dat$age_in_years + dat$goal_one_progress_cat + dat$monthly_income_cat + dat$quizzes_passed_cat
                + dat$full_time_work + dat$degree + dat$adviser,
                data = dat, method = "nearest", distance = "logit")

m.out2 = matchit(dat$gender ~ dat$age_in_years + dat$goal_one_progress_cat + dat$monthly_income_cat + dat$quizzes_passed_cat
                + dat$full_time_work + dat$degree + dat$adviser,
                data = dat, method = "subclass")

m.data4 <- match.data(m.out2, subclass = "block", weights = "w", distance = "pscore")
names(m.data4)
plot(m.data4$id[8:8],m.data4$pscore[8:8])

m.data1 <- match.data(m.out)
summary(m.data1)

m.data2 <- match.data(m.out, group = "treat")
summary(m.data2)

m.data3 <- match.data(m.out, group = "control")
summary(m.data3)

summary(m.out, standardize = TRUE)
plot(m.out, type = "hist")
plot(m.out, discrete.cutoff = 5, type = "QQ",
     numdraws = 5000, interactive = TRUE, which.xs = NULL)
#plot(m.out, type = "jitter")
plot(m.out)
m.data <- match.data(m.out)
m.data <- match.data(m.out, group = "all", distance = "distance",
                     weights = "weights", subclass = "subclass")
summary(m.data, interactions = FALSE, addlvariables = NULL, standardize = FALSE)



library(Zelig)
m.data <- match.data(m.out)
z.out <- zelig(Y ~ m.data$adviser + x1 + x2, model = mymodel, data = m.data)

library(WriteXLS)
WriteXLS(dat, "~/r-code/#FO4108AAB7A08/my_saved_schema.csv.xlsx")




# weighted diff in means
weighted.mean(mdata$adviser[mdata$treat == 1], mdata$weights[mdata$treat==1])
weighted.mean(mdata$re78[mdata$treat==0], mdata$weights[mdata$treat==0])

# weighted least squares without covariates
zelig(re78 ~ treat, data = m.data, model = "ls", weights = "weights")

zelig(formula = re78 ~ treat, model = "ls", data = m.data, weights =
          "weights")


