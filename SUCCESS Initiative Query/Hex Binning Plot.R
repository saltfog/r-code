
# Hex Bin Package
source("http://bioconductor.org/biocLite.R")
biocLite("hexbin")

# Read in the data
data_2013=read.csv("SUCCESS Initiative Query SFY 13 Federal Codes.csv",header=TRUE)

# High Density Scatterplot with Binning
library(hexbin)
enrolled <- (data_2013$Months.Enrolled)
served <- (data_2013$Months.Served)
bin <-hexbin(enrolled, served, xbins=36) 
plot(bin, main="Hexagonal Binning")

