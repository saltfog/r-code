

gum <- read.csv("Science Project Gum.csv",header=TRUE)

head(gum)
summary(gum)


plot(gum$Eclipse,gum$Juicy.Fruit)

rnorm(20)
#> [1] -2.3308287 -0.9073857 -0.7638332 -0.2193786

# Use a different mean and standard deviation
rnorm(20, mean=5, sd=.5)
#> [1] 59.20927 40.12440 44.58840 41.97056

# To check that the distribution looks right, make a histogram of the numbers
x <- rnorm(20, mean=5, sd=.5)
hist(x)

summary(x)
plot(x)
