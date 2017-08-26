#propensity score matching
#propensity score–defined as the probability of receiving the treatment/or program etc... (must be a binary) given the covariates–is a key tool
#Perform the Matching
#Post Balance Analysis

#Read in the data.
library(readr)
dat <- read_csv("~/r-code/#FO4108AAB7A08/my_saved_schema.csv")

#==================================== Data conversion and clean-up ====================================
#Age convert to years custom function
library(lubridate)

age <- function(dob, age.day = today(), units = "year", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

#Return ages and add them to data frame
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
library(MatchIt)
#Use MatchIt to perform the score matching
m.out2 = matchit(dat$gender ~ dat$age_in_years + dat$goal_one_progress_cat + dat$monthly_income_cat + dat$quizzes_passed_cat
                 + dat$full_time_work + dat$degree + dat$adviser,
                 data = dat, method = "subclass")

m.data4 <- match.data(m.out2, subclass = "block", weights = "w", distance = "pscore")
names(m.data4)

#Convert score to percent
m.data4$pscore <- paste(m.data4$pscore * 100)

#Final Propensity Score by Client Id and Last Name
final_pscore <- data.frame(m.data4$id, m.data4$last_name, m.data4$pscore)
full_data_frame <- data.frame(m.data4)
#Convert to Json

json_id_score <- toJSON(final_pscore)
cat(json_id_score)

library(readr)
library(jsonlite)
full_data_frame_toJSON <- toJSON(full_data_frame, method = "C")

#Validation check 
validate(full_data_frame_toJSON) 
write_lines(full_data_frame_toJSON, path = "~/r-code/#FO4108AAB7A08/pscore.json")

#Plot all Scores by Client Id
plot(m.data4$id,m.data4$pscore)

#Custom function for plotting score by variable
fn_bal <- function(dat, variable) {
  dat$variable <- dat[, variable]
  if (variable == 'gender') dat$variable <- dat$variable / 10^3
  dat$gender <- as.factor(dat$gender)
  support <- c(min(dat$variable), max(dat$variable))
  ggplot(dat, aes(x = m.data4$pscore, y = variable)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

#Need libraries
library(MatchIt)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Score Ploting by variable
grid.arrange(
  fn_bal(dat, "quizzes_passed"),
  fn_bal(dat, "full_time_work"),
  fn_bal(dat, "goal_one_progress"),
  fn_bal(dat, "monthly_income"),
  fn_bal(dat, "gender"),
  nrow = 3, widths = c(1, 0.8)
)


