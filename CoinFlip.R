# Extra Credit

# Import Data
library(readr)
cointoss <- read_csv("~/Downloads/data.txt")

# Format data 
cointoss <-unlist(cointoss)

# Number of Heads 1-10
a <-sum(cointoss[1:10])
show(a)

# Number of Heads 1-50
b <-sum(cointoss[1:50])
show(b)

# Number of Heads 1-50
c <-sum(cointoss[1:150])
show(c)

# Install dependencies
library(RColorBrewer)

#Plot 1
colors <- brewer.pal(2, "Set1")
lik <- function(p) dbinom(n.heads, n.tosses, p)
n.tosses = 10
n.heads = 3
curve(lik, lwd = 2, col = colors[3], xlab = "p", ylab = "likelihood", bty = "l")

#Plot 2
colors <- brewer.pal(2, "Set1")
lik <- function(p) dbinom(n.heads, n.tosses, p)
n.tosses = 50
n.heads = 18
curve(lik, lwd = 2, col = colors[1], xlab = "p", ylab = "likelihood", bty = "l")

#Plot 3
colors <- brewer.pal(2, "Set1")
lik <- function(p) dbinom(n.heads, n.tosses, p)
n.tosses = 150
n.heads = 81
curve(lik, lwd = 2, col = colors[2], xlab = "p", ylab = "likelihood", bty = "l")


