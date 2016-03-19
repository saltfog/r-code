
#read in data
dd=read.csv("delay-table.csv",header=TRUE)
summary(dd)

#replace string with integers
# Blank = 0
# Cannot Determine = 1
# Not Significant = 2
# Mild = 3
# Moderate = 4
# Servere = 5

library(sqldf)
#State wide Standard Score Total Eligble (3235)
sqldf("select providers_child_id
      from dd where eligibility_category = 3 and Highest_Delay = 0")

#State wide Standard Score and Severe (3162)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 3 and Highest_Delay = 'Severe'")

#State wide Standard Score and Moderate (67)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 3 and Highest_Delay = 'Moderate'")

#State wide Standard Score and Mild (3)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 3 and Highest_Delay = 'Mild'")

#State wide Standard Score and Not Signifcant (3)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 3 and Highest_Delay = 'Not Significant'")

#State wide Standard Score and Can not Determine (0)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 3 and Highest_Delay = 'Can not Determine'")

#State wide Standard Score and Not Used in Eligibility (0)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 3 and Highest_Delay = 'Not Used in Eligibility'")

#==============================================================================================================
#State wide ICO Total (944)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 1 and Highest_Delay = 0")

#State wide ICO and Severe (443)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 1 and Highest_Delay = 'Severe'")

#State wide ICO and Moderate (22)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 1 and Highest_Delay = 'Moderate'")

#State wide ICO and Mild (263)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 1 and Highest_Delay = 'Mild'")

#State wide ICO and Not Signifcant (208)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 1 and Highest_Delay = 'Not Significant'")

#State wide ICO and Can not Detemine (8)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 1 and Highest_Delay = 'Can not Determine'")

#State wide Standard Score and Not Used in Eligibility (0)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 1 and Highest_Delay = 'Not Used in Eligibility'")
#============================================================================================================
#State wide MED Dx Total (577)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 2 and Highest_Delay <> 0")

#State wide MED Dx and Severe (257)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 2 and Highest_Delay = 'Severe'")

#State wide MED Dx and Moderate (10)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 2 and Highest_Delay = 'Moderate'")

#State wide MED Dx and Mild (53)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 2 and Highest_Delay = 'Mild'")

#State wide MED Dx and Not Significant (229)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 2 and Highest_Delay = 'Not Significant'")

#State wide MED Dx and Can not Detemine (27)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 2 and Highest_Delay = 'Can not Determine'")

#State wide MED Dx and Not Used in Eligibility (0)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 2 and Highest_Delay = 'Not Used in Eligibility'")
#=================================================================================================

#State wide Total Not Eligble (1254)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 4 and Highest_Delay = 0")

#State wide Not Eligble and Severe (55)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 4 and Highest_Delay = 'Severe'")

#State wide Not Eligble and Moderate (0)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 4 and Highest_Delay = 'Moderate'")

#State wide Not Eligble and Mild (462)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 4 and Highest_Delay = 'Mild'")

#State wide Not Eligble and Not Signifcant (767)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 4 and Highest_Delay = 'Not Significant'")

#State wide Not Eligble and Can not Determine (6)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 4 and Highest_Delay = 'Can not Determine'")

#State wide Not Eligble and Not Used in Eligibility (0)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 4 and Highest_Delay = 'Not Used in Eligibility'")

#===============================================================================================

#State wide eligibility not determined (135)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 5 and Highest_Delay = 0")

#State wide eligibility not determined and Severe (3162)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 5 and Highest_Delay = 'Severe'")

#State wide eligibility not determined and Moderate (67)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 5 and Highest_Delay = 'Moderate'")

#State wide eligibility not determined and Mild (3)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 5 and Highest_Delay = 'Mild'")

#State wide eligibility not determined and Not Signifcant (3)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 5 and Highest_Delay = 'Not Significant'")

#State wide eligibility not determined and Can not Determine (0)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 5 and Highest_Delay = 'Can not Determine'")

#State wide eligibility not determined Not Used in Eligibility (0)
sqldf("select providers_child_id, Highest_Delay
      from dd where eligibility_category = 5 and Highest_Delay = 'Not Used in Eligibility'")

#============================================================================================


dat <- read.csv("delay-table.csv")
summary(dat)


# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 1) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(dat, aes(x = eligibility_category, fill=Highest_Delay))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=1) +
  stat_function(fun = f, size = 1,
                args = list(var = dat$eligibility_category))
