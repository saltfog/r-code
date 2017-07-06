library(MatchIt)

library(readr)
MOTZASavingsFitnessScorePartialDummyData <- read_csv("~/Downloads/MOTZASavingsFitnessScorePartialDummyData.csv", 
                                                     col_types = cols(`Monthly income` = col_number(), 
                                                                      `Saving Payment Date` = col_skip(), 
                                                                      email = col_skip(), first_name = col_skip(), 
                                                                      goal_one_target_amount = col_number(), 
                                                                      goal_one_target_date = col_skip(), 
                                                                      id = col_skip(), last_name = col_skip(), 
                                                                      `motza _payment_amount` = col_number(), 
                                                                      motza_payment_date = col_skip(), 
                                                                      saving_payment_amount = col_number()))


m.out <- matchit(treat ~ educ + black + hispan, data = lalonde,
                 + method = "exact")


data("lalonde")
m.out <- matchit(treat ~ educ + black + hispan, data = lalonde,
                 + method = "exact")

m.out <- matchit(treat ~ re74 + re75 + educ + black + hispan + age,
                 + data = lalonde, method = "subclass")

m.out <- matchit(treat ~ re74 + re75 + educ + black + hispan + age,
                 + data = lalonde, method = "nearest")

m.out <- matchit(treat ~ re74 + re75 + age + educ, data = lalonde,
                 + method = "optimal", ratio = 2)

R> m.out <- matchit(treat ~ age + educ + black + hispan + married +
                      + nodegree + re74 + re75, data = lalonde, method = "full")

m.data <- match.data(m.out)

