temp <- read.csv("temp.csv", header = TRUE)

head(temp)
summary(temp$Year,temp$Maximum.temperature..Degree.C.)

# Create a 30 x 30 matrix (of 30 rows and 30 columns)
mymat <- matrix(nrow=4018, ncol=2)

# For each row and for each column, assign values based on position: product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Just show the upper left 10x10 chunk
mymat[1:4018]

