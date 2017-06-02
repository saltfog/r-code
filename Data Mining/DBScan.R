install.packages("fpc")
install.packages("dbscan")

data("iris")
iris <- as.matrix(Attribute_DataSet[, 1:2])
dbscan::kNNdistplot(iris, k =  3)
abline(h = 0.4, lty = 2)

set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(iris, eps = 0.4, MinPts = 3)
# dbscan package
res.db <- dbscan::dbscan(iris, 0.4, 3)
library("factoextra")
fviz_cluster(res.fpc, iris, geom = "point", title(main = "Iris"))
print(res.db)
