#imports
babies = read.table("babies.txt", header=TRUE)

#You can extract the baby birthweights from smoking and non-smoking mothers with the code:

bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

# QUESTION 1.1
# What is the average length of the confidence interval?
N=30
conf.int<-function(){
  sample.bwt.nonsmoke <- sample(bwt.nonsmoke,N)
  sample.bwt.smoke <- sample(bwt.smoke,N)
  bwt.test <- t.test(sample.bwt.nonsmoke,sample.bwt.smoke)
  return (bwt.test$conf.int[2] - bwt.test$conf.int[1])
}
mean(replicate(1000, conf.int()))

# QUESTION 1.2
# How often (what proportion of times) did the confidence intervals contain the population-level
#difference?
popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)
N=30
diff.compare<-function(){
  sample.bwt.nonsmoke <- sample(bwt.nonsmoke,N)
  sample.bwt.smoke <- sample(bwt.smoke,N)
  bwt.test <- t.test(sample.bwt.nonsmoke,sample.bwt.smoke)
  return (popdiff>bwt.test$conf.int[1] & popdiff<bwt.test$conf.int[2])
}
mean(replicate(1000, diff.compare()))

#difference between means
t.test(bwt.nonsmoke, bwt.smoke)
sample <- t.test(bwt.nonsmoke, bwt.smoke)
sample$p.value
sample$conf.int

# QUESTION 3.1
# What is the power at alpha=0.1?
N=15
reject <- function(N,alpha){
  sample.bwt.nonsmoke <- sample(bwt.nonsmoke,N)
  sample.bwt.smoke <- sample(bwt.smoke,N)
  pval <- t.test(sample.bwt.nonsmoke,sample.bwt.smoke)$p.value
  ifelse(pval < alpha,1,0)
}
mean(replicate(1000,reject(N,0.1)))

# QUESTION 3.2
# What is the power at alpha=0.05?
mean(replicate(1000,reject(N,0.05)))

# QUESTION 3.3
# What is the power at alpha=0.01?
mean(replicate(1000,reject(N,0.01)))

#======================================================================
# Week 3 Inference III
#======================================================================
babies = read.table("https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]
# QUESTION 1.1
# How often (what proportion of simulations) is the sample variance greater than 1.5 times the population variance?
pop.var = var(bwt.nonsmoke)
vars = replicate(1000, var(sample(bwt.nonsmoke,10)))
mean(vars > pop.var*1.5)
