
# Read in the data
data_2013=read.csv("SUCCESS Initiative Query SFY 13 Federal Codes.csv",header=TRUE)

# Set up the x y variables
enrolled <- (data_2013$Months.Enrolled)
served <- (data_2013$Months.Served)
exitcode <- (data_2013$Federal.Code)
exitreason <-(data_2013$Federal.Exit.Reason)



# Scatter Plot
plot(served, enrolled)

# Fit lines
abline(lm(enrolled~served), col="red")
lines(lowess(served,enrolled), col="blue")
