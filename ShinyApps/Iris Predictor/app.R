# Predicting with Machine Learning

# Set working directory
#setwd("~/r-code/ShinyApps/Iris Predictor")

# Load the data
data(iris)

# Set a seed to make randomness reproducible
set.seed(42)

# Randomly sample 100 of 150 row indexes
indexes <- sample(
  x = 1:150,
  size = 100)

# Inspect the random indexes
print(indexes)

# Create a training set from indexes
train <- iris[indexes, ]
save(train, file = "train")

# Create a test set from remaining indexes
test <- iris[-indexes, ]
save(test, file = "test")
# Load the decision tree package
library(tree)

# Train a decision tree model
model <- tree(
  formula = Species ~ .,
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
  x = iris$Petal.Length,
  y = iris$Petal.Width,
  pch = 19,
  col = palette[as.numeric(iris$Species)],
  main = "Iris Petal Length vs. Width",
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

# Plot the decision boundaries
partition.tree(
  tree = model,
  label = "Species",
  add = TRUE)

# Predict with the model
predictions <- predict(
  object = model,
  newdata = test,
  type = "class")

# Create a confusion matrix
table(
  x = predictions,
  y = test$Species)

# Load the caret package
library(caret)

# Evaluate the prediction results
confusionMatrix(
  data = predictions,
  reference = test$Species)

# Save the tree model
save(model, file = "Tree.RData")

# Save the training data
save(model, file = "Train.RData")


# Deploying to Production

# Load shiny
library(shiny)

# Load decision tree package
library(tree)

# Load training data
load("Train.RData")

# Load tree model
load("Tree.RData")

# Load color brewer library
library(RColorBrewer)

# Create a color palette
palette <- brewer.pal(3, "Set2")

# Create user interface code
ui <- fluidPage(
    titlePanel("Iris Species Predictor"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "petal.length",
                label = "Petal Length (cm)",
                min = 1,
                max = 7,
                step = 0.1,
                value = 4),
            sliderInput(
                inputId = "petal.width",
                label = "Petal Width (cm)",
                min = 0.0,
                max = 2.5,
                step = 0.1,
                value = 1.5)),
        mainPanel(
            textOutput(
                outputId = "text"),
            plotOutput(
                outputId = "plot"))))

# Create server code
server <- function(input, output) {
    output$text = renderText({
        
        # Create predictors
        predictors <- data.frame(
            Petal.Length = input$petal.length,
            Petal.Width = input$petal.width,
            Sepal.Length = 0,
            Sepal.Width = 0)
        
        # Make prediction
        prediction = predict(
            object = model,
            newdata = predictors,
            type = "class")
        
        # Create prediction text
        paste(
            "The predicted species is ",
            as.character(prediction))
    })
    
    output$plot = renderPlot({
        
        # Create a scatterplot colored by species
        plot(
            x = iris$Petal.Length, 
            y = iris$Petal.Width,
            pch = 19,
            col = palette[as.numeric(iris$Species)],
            main = "Iris Petal Length vs. Width",
            xlab = "Petal Length (cm)",
            ylab = "Petal Width (cm)")
        
        # Plot the decision boundaries
        partition.tree(
            model,
            label = "Species",
            add = TRUE)
        
        # Draw predictor on plot
        points(
            x = input$petal.length,
            y = input$petal.width,
            col = "blue",
            pch = 4,
            cex = 2,
            lwd = 3)
    })
}

# Create a shiny app
shinyApp(
    ui = ui,
    server = server)

