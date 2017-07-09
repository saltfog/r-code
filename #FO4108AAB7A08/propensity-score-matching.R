#propensity score matching
#propensity score–defined as the probability of receiving the treatment/or (must be a binary) given the covariates–is a key tool
#Data Prep
#Perform the Matching
#Post Balance Analysis

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

# min(dat$payments)
# max(dat$payments)
# mean(dat$payments)
# summary(dat$payments)

# library(MatchIt) 
# m.out = matchit(dat$gender ~ dat$age_in_years + dat$goal_one_progress_cat + dat$monthly_income_cat + dat$quizzes_passed_cat
#                 + dat$full_time_work + dat$degree + dat$adviser,
#                 data = dat, method = "nearest", distance = "logit")
# 
m.out2 = matchit(dat$gender ~ dat$age_in_years + dat$goal_one_progress_cat + dat$monthly_income_cat + dat$quizzes_passed_cat
                 + dat$full_time_work + dat$degree + dat$adviser,
                 data = dat, method = "subclass")

m.data4 <- match.data(m.out2, subclass = "block", weights = "w", distance = "pscore")
names(m.data4)
m.data4$pscore <- paste(m.data4$pscore * 100)

final_pscore <- data.frame(m.data4$id, m.data4$last_name, m.data4$pscore)
library(df2json)
json <- df2json(final_pscore)
View(json)
plot(m.data4$id,m.data4$pscore)

# library(WriteXLS)
# WriteXLS(dat, "~/r-code/#FO4108AAB7A08/my_saved_schema.csv.xlsx")


fn_bal <- function(dat, variable) {
  dat$variable <- dat[, variable]
  if (variable == 'gender') dat$variable <- dat$variable / 10^3
  dat$gender <- as.factor(dat$gender)
  support <- c(min(dat$variable), max(dat$variable))
  ggplot(dat, aes(x = m.data4$pscore, y = variable, color = quizzes_passed)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}
library(MatchIt)
library(dplyr)
library(ggplot2)
library(gridExtra)
grid.arrange(
  fn_bal(dat, "quizzes_passed"),
  fn_bal(dat, "full_time_work") + theme(legend.position = "none"),
  fn_bal(dat, "goal_one_progress"),
  fn_bal(dat, "monthly_income") + theme(legend.position = "none"),
  fn_bal(dat, "gender"),
  nrow = 3, widths = c(1, 0.8)
)
