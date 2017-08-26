#Association Rules Data Mining Basket Analysis
library(arules)
data("Groceries")

#Sparse formatting
Groceries

summary(Groceries)
# density total number of cells have no empty cells

9853 * 169 #number of total cells
9853 * 169 * .02609 #no empty cells

#Buying 1-5 items
#Right Skewed (Median -> Mean)
#Left Skewed (Mean <- Median)

#inspect first 10
inspect(Groceries[1:10])


#ASSOCIATION RULES
#Support needs to be high for support confidence, good predictive power
#Confidence / conditional probablity

itemFrequency(Groceries[ ,1:6])
itemFrequencyPlot(Groceries, support= .10) #high support 
itemFrequencyPlot(Groceries, top=20)

#High Support and Good Confidence

model1 <- apriori(Groceries, parameter = list(support=0.007,confidence=.02, minlen=2))
summary(model1)

inspect(model1[1:3]) #lift likely.

inspect(head(sort(model1, by="lift"),10))
inspect(head(sort(model1, by="confidence"),10))
inspect(head(sort(model1, by="support"),10))

inspect(head(sort(model1, by=c("lift", "confidence")),10))

arulesViz::plotly_arules(model1, method = "scatterplot", measure = c("support", "confidence", "lift"), shading = "lift")
arulesViz::plotly_arules(model1, method = "scatterplot", measure = c("support", "confidence", "lift"), shading = "support")
arulesViz::plotly_arules(model1, method = "scatterplot", measure = c("support", "confidence", "lift"), shading = "confidence")
