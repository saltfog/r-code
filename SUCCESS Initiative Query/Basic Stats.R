

data_2013=read.csv("SUCCESS Initiative Query SFY 13.csv",header=TRUE)

#Race Spread

x <- (data_2013$Months.Enrolled)
y <- (data_2013$Months.Served)


plot(x,y)

