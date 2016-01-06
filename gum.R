rnorm(20)


# Use a different mean and standard deviation
x <- rnorm(20, mean=3.7, sd=.1)
cat(x,sep = "\n")
scatter.smooth(x, y = NULL)
print(x)

# To check that the distribution looks right, make a histogram of the numbers
x <- rnorm(400, mean=50, sd=10)
hist(x)
