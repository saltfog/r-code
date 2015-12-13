set.seed(1233)

data_2013=read.csv("SUCCESS Initiative Query SFY 13.csv",header=TRUE)
data1 <- data.frame((data_2013$Months.Served), (data_2013$Months.Enrolled), (data_2013$Age.at.Exit))
#plot using ggplot2
require(ggplot2)
qplot( var1, data = data1, geom = "density" , group = pop, fill = pop, alpha=.3) + theme_bw( )

qplot( var1, data = data1, geom = "histogram" , group = pop, fill = pop, alpha=.3) + theme_bw( )

plot(data1$X.data_2013.Months.Served., data1$X.data_2013.Months.Enrolled. data1$X.data_2013.Age.at.Exit.)

qplot( data1, data_2013, geom = "histogram",  group = data1, fill = data1, alpha=.3) + theme_bw( )

ggplot(data1, aes(x=data1$X.data_2013.Months.Served., color=data1$X.data_2013.Months.Enrolled.,fill=data1$X.data_2013.Months.Enrolled.)) + geom_bar(position="dodge")
