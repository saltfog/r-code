
# Load Source Data
dataProfess <- read.csv("CourseEvals.csv",header=TRUE)

#(1)
# Remove duplcates and create new dataset called no_dups
no_dups<-dataProfess[!duplicated(dataProfess$prof_id),]

# Show quick summary of new no_dups dataset
summary(no_dups)
head(no_dups)

#(2 a) 94 Professors
max(no_dups$prof_id)

#(2 b) SD 1.60 / Mean 4.59
sd(no_dups$bty_avg)
mean(no_dups$bty_avg)

#(2 c) It looks to be normally distributed: bell shaped, unimodal, and symmetric.
#need ggplot
library(ggplot2)
ggplot(no_dups, aes(x=no_dups$bty_avg)) +
  geom_histogram(binwidth=1, colour="black", fill="white")

#(3 a) Quantitative. The mean of the beauty rating. 5
mean(no_dups$bty_avg)

#(3 b)
# Ha : μ New ≠ 5 H0 : μ New = 5

#(3 c) Yes. The sampling distribution is random, normally distributed, and independent
inference.means<-function(variable,sample.size,alpha,num.reps){
  
  samp.est<-rep(NA,num.reps)
  stdev<-rep(NA,num.reps)
  se.xbar<-rep(NA,num.reps)
  test.stat<-rep(NA,num.reps)
  p.val<-rep(NA,num.reps)
  decision<-rep(NA,num.reps)
  lcl<-rep(NA,num.reps)
  ucl<-rep(NA,num.reps)
  capture<-rep(NA,num.reps)
  true.mean<-mean(variable)
  
  for(i in 1:num.reps){
    samp<-sample(variable,sample.size)
    samp.est[i]<-mean(samp)
    stdev[i]<-sd(samp)
    se.xbar[i]<-stdev[i]/sqrt(sample.size)
    test.stat[i]<-(samp.est[i]-true.mean)/se.xbar[i]
    df<-sample.size-1
    p.val[i]<-2*pt(abs(test.stat[i]),df,lower.tail=FALSE)
    t.score<-qt(1-alpha/2,df)
    lcl[i]<-samp.est[i]-t.score*se.xbar[i]
    ucl[i]<-samp.est[i]+t.score*se.xbar[i]
    
    decision[i]<-ifelse(p.val[i]<=alpha,"reject Ho","fail to reject Ho")
    capture[i]<-ifelse(lcl[i]<=true.mean & ucl[i]>=true.mean,"yes","no")
  }
  
  results<-data.frame(samp.est=round(samp.est,4),
                      test.stat=round(test.stat,4),
                      p.val=round(p.val,4),
                      decision=decision,
                      lcl=round(lcl,4),
                      ucl=round(ucl,4),
                      capture=capture)
  return(results)
  result
}




plot.ci<-function(results,true.val){
  par(mar=c(4, 1, 2, 1), mgp=c(2.7, 0.7, 0),xpd=T)
  k <- length(results$lcl)
  xR <- c(min(results$lcl),max(results$ucl))
  yR <- c(0, 41*k/40)
  plot(xR, yR, type='n', xlab='', ylab='', axes=FALSE)
  cols<-ifelse(results$capture=="yes","white","firebrick2")
  segments(results$lcl,1:k,results$ucl,1:k,col=cols,lwd=4)
  points(results$samp.est,1:k,pch=20,col="black")
  segments(results$lcl,1:k,results$ucl,1:k,col="black")
  segments(true.val,0-42/40,true.val,42*k/40, lty=2, col="royalblue3")
  axis(1)
  text(true.val,yR[2],paste("true =",round(true.val,4)),col="royalblue3",pos=3)
}

plot.ci(results)

#(3 d) The test statistic is -2.4797.
View(inference.means(no_dups$bty_avg, 5,.1,1))

#3 e) The p-value is 0.01495.
View(inference.means(no_dups$bty_avg, 5,.1,1))

#(3 f) Yes Reject
View(inference.means(no_dups$bty_avg, 5,.05,1))

#(3 g) The beauty rating will most likely not be equal to 5.
View(inference.means(no_dups$bty_avg, 5,.1,1))

#(3 h) 95% confident that the true mean for the beauty rating is in the the interval 4.262359 to 4.918407. The interval shows that the true mean will most likely be lower than 5
View(inference.means(no_dups$bty_avg, 5,.1,1))  

#(4)
# example 2*pt(3,df=99) 1.996584
# Answers
# a) t=1,df =24 __0.3273___ b) t=2,df =24 __0.0569___ c) t=3,df =24 __0.0062___ d) t=1,df =99 __0.3197___ e) t=2,df =99 __0.0482___ f) t=3,df =99 __0.0034__

# As the test statistic increases, the p-value ( increases ) . Therefore, larger test statistics present (less ) evidence against the null hypothesis. As the degrees of freedom increase, the p- value ( increases )
# This is because as the degrees of freedom increase, the t distribution gets
# closer to a normal distribution. The same test statistic value with different degrees of freedom
#(can) result in a different conclusion for a specified level of significance (e.g., α = 0.05).

#(5)
# Answers
#a. 90%,df =24 qt(0.05,df=24) 1.710882 b. 95%,df =24 qt(0.025,df=24) 2.063899 c. 99%,df =24 qt(0.005,df=24) 2.79694 d. 90%,df =99 qt(0.05,df=99) 1.660391 e. 95%,df =99 qt(0.025,df=99) 1.984217 f. 99%,df =99 qt(0.005,df=99) 2.626405

#As the confidence level increases, the t-score ( increases ). 
#This means that higher confidence levels result in ( narrower ) confidence intervals. 
#As the degrees of freedom increase, the t-score ( decreases ) . 
#This means that for the same confidence level, larger degrees of freedom result in ( narrower ) confidence intervals.
