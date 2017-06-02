
# Hex Bin Package
source("http://bioconductor.org/biocLite.R")
biocLite("hexbin")

# Read in the data
data_2013=read.csv("2007 to now SI.csv",header=TRUE)

# High Density Scatterplot with Binning
library(hexbin)
enrolled <- (data_2013$enrollment_months)
served <- (data_2013$months_served)
bin <-hexbin(enrolled, served, xbins=36) 
plot(bin, main="Hexagonal Binning")

