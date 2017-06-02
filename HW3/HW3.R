#Babies Mothers (Smoke/Non-Smoke)

#read in data
babies = read.table("babies.txt", header=TRUE)

#split the data into two data sets bw smokers bw non smoking

bwt.nonsmoke = babies$bwt[babies$smoke == 0] # Boolean
bwt.smoke = babies$bwt[babies$smoke == 1]

#Now, we can look for the true population difference in means between 
#smoking and non-smoking birthweights.

mean(bwt.nonsmoke)-mean(bwt.smoke) #The population difference of mean birthweights
sd(bwt.nonsmoke)
sd(bwt.smoke)

#Suppose we obtain two samples, each of size N, 
#from non-smoking mothers (dat.ns) and smoking mothers (dat.s). 
#Following lecture, we compute the t-value, which we call tval.

X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/N+sd.s^2/N)
tval = (X.ns - X.s)/sd.diff

t.test(bwt.nonsmoke,bwt.smoke)$statistic #depends on the size of the sample

bwt.nonsmoke30 = bwt.nonsmoke[1:30]
bwt.smoke30 = bwt.smoke[1:30]

#test of the first 30 
t.test(bwt.nonsmoke30,bwt.smoke30)$statistic #depends on the size of the sample

tval = t.test(bwt.nonsmoke,bwt.smoke)$statistic
pval = 1- pnorm(abs(tval)) + pnorm(-abs(tval))
