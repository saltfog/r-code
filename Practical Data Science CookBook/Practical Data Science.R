

library(readr)
vehicles <- read_csv("~/r-code/Practical Data Science CookBook/vehicles.csv")

names(vehicles)
length(unique(vehicles[, "year"]))

table(vehicles$fuelType1)

vehicles$trany[vehicles$trany == ""] <- NA
vehicles$trany2 <- ifelse(substr(vehicles$trany, 1, 4) == "Auto", "Auto", "Manual")

vehicles$trany <- as.factor(vehicles$trany) 
table(vehicles$trany2)

#cross tabulations
with(vehicles, table(sCharger, year))

library(plyr)
mpgByYr <- ddply(vehicles, ~year, summarise, avgMPG = mean(comb08), avgHghy = mean(highway08), avgCity = mean(city08))

library(ggplot2)
ggplot(mpgByYr, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("All cars") ## geom_smooth: method="auto" and size of largest group is <1000, so using ## loess. Use 'method = x' to change the smoothing method.

#based off the plot one might think MPG has gotton better over the year but it can be miss leading as electic and hybrids have
#been introduced.

table(vehicles$fuelType1)

#lets only looks at gas cars
gasCars <- subset(vehicles, fuelType1 %in% c("Regular Gasoline", "Premium Gasoline", "Midgrade Gasoline") 
                  & fuelType2 == "" & atvType != "Hybrid") 

mpgByYr_Gas <- ddply(gasCars, ~year, summarise, avgMPG = mean(comb08)) 
ggplot(mpgByYr_Gas, aes(year, avgMPG)) + geom_point() + geom_smooth() + xlab("Year") + ylab("Average MPG") + ggtitle("Gasoline cars")
## geom_smooth: method="auto" and size of largest group is <1000, so using ## loess. Use 'method = x' to change the smoothing method.