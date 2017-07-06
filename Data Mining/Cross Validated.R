library(readr)
 md <- read_csv("~/Downloads/MOTZASavingsFitnessScorePartialDummyData.csv", 
                                                       +     na = "NA")
library(MatchIt)
zz <- matchit(md$id ~ md$gender + md$Occupation, data=md, method="nearest", 
              distance="mahalanobis", replace=TRUE)


summary(m.out)
plot(m.out, type = "jitter")
plot(m.out, type = "hist")

