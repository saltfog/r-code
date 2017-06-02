
iris <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/bezdekIris.data"), header = FALSE)

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

install.packages("ggvis")
library(ggvis)

iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()

iris %>% ggvis(~Petal.Length, ~Petal.Width, fill = ~Species) %>% layer_points()

library(class)

iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

summary(iris_norm)

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]

iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

iris_pred
