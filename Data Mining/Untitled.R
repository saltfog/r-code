############ Laboratory task ###################
#calculation of accuracy
accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}

#data set for the laboratory task
#http://archive.ics.uci.edu/ml/datasets/Cardiotocography 

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_noClass_corr.csv','cardioto_noClass_corr.csv')
ctg_noClass <- read.csv("cardioto_noClass_corr.csv",row.names = 1)

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_all_corr.csv','cardioto_all_corr.csv')
ctg_all <- read.csv("cardioto_all_corr.csv",row.names = 1)


#simplified example
distC = dist(ctg_noClass)
card.kmeans = kmeans(distC,10)
res3 = table(ctg_all$CLASS,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

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
