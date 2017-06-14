# Task 1

# General Objective
# To find the best interesting sequential rules
# Data sets
# • diab_trans.data
# • http://mlr.cs.umass.edu/ml/datasets/Diabetes does not exist
#   https://archive.ics.uci.edu/ml/datasets/diabetes correct location

# Data mining with associated rules.

# The Code field is deciphered as follows: 
#   
#   33 = Regular insulin dose 
#   34 = NPH insulin dose 
#   35 = UltraLente insulin dose 
#   48 = Unspecified blood glucose measurement 
#   57 = Unspecified blood glucose measurement 
#   58 = Pre-breakfast blood glucose measurement 
#   59 = Post-breakfast blood glucose measurement 
#   60 = Pre-lunch blood glucose measurement 
#   61 = Post-lunch blood glucose measurement 
#   62 = Pre-supper blood glucose measurement 
#   63 = Post-supper blood glucose measurement 
#   64 = Pre-snack blood glucose measurement 
#   65 = Hypoglycemic symptoms 
#   66 = Typical meal ingestion 
#   67 = More-than-usual meal ingestion 
#   68 = Less-than-usual meal ingestion 
#   69 = Typical exercise activity 
#   70 = More-than-usual exercise activity 
#   71 = Less-than-usual exercise activity 
#   72 = Unspecified special event

#Needed libraries
library(arules)
library(arulesSequences)

##################################  Data Prepartation ##################################
#read in the data
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)

diab.df <- diab.df[complete.cases(diab.df),]

#define the column names
colnames(diab.df) <- c("ID", "time", "eventID", "value")

#eventID formatting e.g. id_65 to 65
eventID_to_int <- function(frame) {
  apply(frame, 1, function(x) strtoi(unlist(strsplit(x[3], "_"))[2]))
}
diab.df$eventID <- eventID_to_int(diab.df)

# clean the data for use, throw out data we don't need
# events where id < 64 we don't want not transactional data items, 65+ we need. We want to create baskets for the events that do have values
# so that similar values are grouped together during rule mining.
num_baskets <- 3
out.df <- data.frame()
for (id in unique(diab.df$eventID)) {
  cat("processing eventID: ", id, "\n")
  baskets <- list(dim=(num_baskets+1))
  if(id >= 65) {
    next
  }

  sorted_values <- sort(diab.df$value[diab.df$eventID == id])
  len <- length(sorted_values)
  step <- len / num_baskets
  for (j in 1:(num_baskets)) {
    baskets <- append(baskets, sorted_values[j * step])
  }
  baskets <- append(baskets, len)

  sub <- subset(diab.df, eventID == id)
  for (row in 1:nrow(sub)) {
    s <- sub[row,]
    for (k in 1:(num_baskets)) {
      if ((s[4] >= baskets[k]) && (s[4] <= baskets[k+1])) {
        mod_sample <- s
        mod_sample[3] = 100 * s[3] + k # change event ID
        out.df  <- rbind(out.df, mod_sample)
        break
      }
    }
  }
}

unique(out.df$eventID)

################################## Putting the Data into Baskets ##################################

data.df <- out.df
data.df <- rbind(data.df, subset(diab.df, eventID > 64))

head(subset(data.df, eventID==5803))
data.df.sorted <- data.df[order(data.df["ID"], data.df["time"]),]

write.table(data.df.sorted, "diab_baskets.data", sep = ",", row.names = FALSE, col.names = FALSE)
write.table(data.df.sorted[,-c(4)], "diab_baskets_novalues.data", sep = ",", row.names = FALSE, col.names = FALSE)

diabSeq <- read_baskets(con = "diab_baskets_novalues.data", sep =",", info = c("sequenceID","eventID"))
seqParam = new ("SPparameter",support = 0.5, maxsize = 4, mingap=600, maxgap =172800, maxlen = 3 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

################################## Finding the rules using rule induction ##################################
#set the confidence at 80% but can be increased to 90%
seqRules = ruleInduction(patSeq,confidence = 0.8)

length(seqRules)
#Summary of the sequence rules
summary(seqRules)
#inspect the first 100 rules
inspect(head(seqRules,100))

#inspect all rules 531
inspect(seqRules,531)

################################## Summary Ouput ##########################################################
#top 10 rules by confidence, support and lift.
inspect(head(sort(seqRules, by=c("confidence", "support")),10))
inspect(head(sort(seqRules, by=c("support", "confidence")),10))
inspect(head(sort(seqRules, by ="lift"),10))


#######################################################################################################################
# Task 2
# To find the best classifier for a selected dataset.
# Datasets: • Wine Quality (two sets) - http://archive.ics.uci.edu/ml/datasets/Wine+Quality

# Algorithm used k-means

wine <- read.csv('winequality-white.csv', sep=';')
wine <- rbind(wine, read.csv('winequality-red.csv', sep=';'))
summary(wine)
wine<-scale(wine)
wss<-(nrow(wine)-1)*sum(apply(wine,2,var))
for(i in 1:15) wss[i]<-sum(kmeans(wine,centers=i)$withinss)
plot(1:15,wss,type='b',xlab="Number of Clusters",ylab='Within groups sum of squares')
fit1 <- kmeans(wine,6)
fit2 <- kmeans(wine,8)

table(fit1$cluster)
library(fpc)
plotcluster(wine, fit1$cluster)
aggregate(wine,by=list(fit1$cluster),FUN=mean)
mydata <- data.frame(wine, fit1$cluster)

table(fit2$cluster)
library(fpc)
plotcluster(wine, fit2$cluster)
aggregate(wine,by=list(fit2$cluster),FUN=mean)
mydata <- data.frame(wine, fit2$cluster)

library(cluster)
clusplot(wine, fit1$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
cluster.stats(fit1$cluster, fit2$cluster)

# Task 2 Alterative method Regression Trees and Models
wine <- read.csv('winequality-white.csv', sep=';', header = TRUE)
wine <- rbind(wine, read.csv('winequality-red.csv', sep=';', header = TRUE))
hist(wine$quality)
# The wine quality values appear to follow a fairly normal, bell-shaped distribution, centered around a value of six.
summary(wine)
#partition the data
wine_train <- wine[1:3248, ]
wine_test <- wine[3249:6497, ]

m.rpart <- rpart::rpart(quality ~ ., data = wine_train)
m.rpart
summary(m.rpart)

library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
###
# Because alcohol was used first in the tree, it is the single most important predictor of wine quality.
###
#adjustments to the diagram
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

p.rpart <- predict(m.rpart, wine_test)

#predicted values vs. actual values
summary(p.rpart)
summary(wine_test$quality)

#Summary
# k-means and regression trees where used mining the wine data set. The classifer method is the regression tree analysis.
# I could see the predict values vs the actual values

###############################################################################################################################
#Task 3
# Aim: Determine the best grouping according to the given evaluation method.
# • Data Description: http://archive.ics.uci.edu/ml/datasets/Cardiotocography
# • Additional constraints:
# – In the analysis the fields: Class and NSP should not be taken into consideration
# – Maximum number of groups 15 (the references grouping has 10 groups).
# – The reference grouping is defined  by the Class attribute.
# – Minimum 10 tests are required.

#clusplot takes about 10min to generate.

##################################  Data Prepartation ##################################
library(readxl)
CTG <- read_excel("~/Git-Repos/r-code/Data Mining/CTG-raw.xls")

ctg_noClass <- read.csv("cardioto_noClass_corr.csv",row.names = 1)
ctg_all <- read.csv("cardioto_all_corr.csv",row.names = 1)

distC = dist(ctg_noClass)
#10 Tests
card.kmeans = kmeans(distC,10)
res3 = table(ctg_all$CLASS,card.kmeans$cluster)
res3

# The Vector Matrix correlates with
# 1= A	calm sleep
# 2= B	REM sleep
# 3= C	calm vigilance
# 4= D	active vigilance
# 5= SH	shift pattern (A or Susp with shifts)
# 6= AD	accelerative/decelerative pattern (stress situation)
# 7= DE	decelerative pattern (vagal stimulation)
# 8= LD	largely decelerative pattern
# 9= FS	flat-sinusoidal pattern (pathological state)
# 10= SUSP	suspect pattern

################################## Clustering and Classes ##################################
library(fpc)
library(cluster)
distance <- dist(ctg_noClass, method = "binary") # we use binary matrix being 0 and 1

fit <- kmeans(distance, centers=10)
res = table(ctg_all$CLASS, fit$cluster )

fit <- kmeans(distance, centers=12)
res = table(ctg_all$CLASS, fit$cluster )

fit <- kmeans(distance, centers=8)
res = table(ctg_all$CLASS, fit$cluster )

fit <- kmeans(distance, centers=14)
res = table(ctg_all$CLASS, fit$cluster )

fit <- kmeans(distance, centers=14, iter.max=50)
res = table(ctg_all$CLASS, fit$cluster )

fit <- kmeans(distance, centers=14, iter.max=500)
res = table(ctg_all$CLASS, fit$cluster )

fit <- kmeans(distance, centers=14, iter.max=500, nstart=3)
res = table(ctg_all$CLASS, fit$cluster )

fit <- kmeans(distance, centers=14, iter.max=500, nstart=5)
res = table(ctg_all$CLASS, fit$cluster )

clusterTree <- hclust(distance)
clusters <- cutree(hclust(distance), 10)
res = table(ctg_all$CLASS, clusters)

View(res)

library(cluster)
clusplot(ctg_all, clusters, color=TRUE, shade=TRUE,labels=5, lines=0)

cluster.stats(distance, ctg_all$CLASS, fit$cluster)

#Summary
#Best groupings are the sleeping classes based off the clustering analysis
