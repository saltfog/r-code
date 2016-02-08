rnorm(20)


# Use a different mean and standard deviation
x <- rnorm(20, mean=3.7, sd=.1)
cat(x,sep = "\n")

print(x)

hist(x)

n <- read.csv("Gum.csv",header=TRUE)

summary(n)
mean(n)
min(n)
max(n)

plot(n)
