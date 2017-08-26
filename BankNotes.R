library(readxl)
banknote <- read_excel("~/Downloads/banknote.xlsx")

# Set a seed to make randomness reproducible
set.seed(420)

# Randomly sample 100 of 150 row indexes
indexes <- sample(
  x = 1:1371, 
  size = 50)

# Inspect the random indexes
print(indexes)

# Create a training set from indexes
train <- banknote[indexes, ]

# Create a test set from remaining indexes
test <- banknote[-indexes, ]

# Load the decision tree package
library(tree)

# Train a decision tree model
model <- tree(
  formula = fake ~ .,
  data = train)

# Inspect the model
summary(model)

# Visualize the decision tree model
plot(model)
text(model)

# Load color brewer library
library(RColorBrewer)

# Create a color palette
palette <- brewer.pal(3, "Set2")

# Create a scatterplot colored by species
plot(
  x = banknote$variance,
  y = banknote$curtosis,
  pch = 19,
  col = palette[as.numeric(banknote$fake)],
  main = "Variance Vs Skewness",
  xlab = "V",
  ylab = "S")

# Plot the decision boundaries
partition.tree(
  tree = model,
  label = "fake",
  add = TRUE)

# Predict with the model
predictions <- predict(
  object = model,
  newdata = test)

# Create a confusion matrix
table(
  x = predictions, 
  y = test$fake)

# Load the caret package
library(caret)

# Evaluate the prediction results
confusionMatrix(
  data = predictions, 
  reference = test$fake)

# Load color brewer library
library(RColorBrewer)

# Create a color palette
palette <- brewer.pal(3, "Set2")

