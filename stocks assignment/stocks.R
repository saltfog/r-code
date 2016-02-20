

install.packages("xts")
install.packages("xtsExtra", repos="http://R-Forge.R-project.org", type="source")

library(xts)
library(xtsExtra)

#Import data
data = read.csv("projec1.csv",header=TRUE)
summary(data)

group=c("YHOO","BIDU","SINA","SOUH","FB",
        "DATE","GRPN","TZOO","PRSS","HSTM")

#cutting date from the listing date of NASDAQ
CJD = data[ ,c("Dindex", "Pindex")]
CJD[,1] = as.Date(CJD[, 1], format='%m/%d/%Y')
Cdate = CJD[1,1]
win=c(60,120)

for (w in win){
  #get index data
  Xind = data[,c("Dindex","Pindex")]
  Xind[,1] = as.Date(Xind[,1], format='%m/%d/%Y')
  tcutin = min(which(Xind[,1]>=Cdate))
  Xindex = cbind(seq(-w,w,1), Xind[(tcutin-w):(tcutin+w), 2])
  Xindex[,2] = Xindex[,2]/Xindex[(w+1),2]*100
  
  for (g in group){
    dd = paste("D",g, sep="")
    pp = paste("P",g, sep="")
    X = data[,c(dd,pp)]
    X[,1] = as.Date(X[,1], format='%m/%d/%Y')
    tcut = min(which(X[,1]>=Cdate))
    
    #two dimentional vector of time and price
    XX = cbind(seq(-w,w,1), X[(tcut-w):(tcut+w), 2])
    
    #normalize price
    XX[,2] = XX[,2]/XX[(w+1),2]*100
    
    #detrend
    XX1 = XX[,2]/Xindex[,2]*100
    XX = cbind(XX, XX1)
    
    #save generated data
    gg = paste("X",g, sep="")
    assign(gg,XX)
  }
  
  ymin = 0
  ymax = 0
  i = 0
  for (g in group){
    gg = paste("X",g, sep="")
    XX = get(gg)
    i = i+1
    ymin[i] = min(XX[,3])
    ymax[i] = max(XX[,3])
  }
  ylimt = cbind(min(ymin), max(ymax))
  
  nn = paste("myplot",w,".png", sep="")
  png(file=nn, width = 718, height = 432)
  
  n = 0
  rcolor = rainbow(length(group))
  for (g in group){
    n = n+1
    ml = 1+n%%2
    gg = paste("X",g, sep="")
    XX = get(gg)
    if (n==1){
      plot(XX[,c(1,3)], lty=ml, col=rcolor[n], ylim=ylimt, xlab="time", ylab="percentage",
           type="l", lwd=2)}
    else {
      lines(XX[,c(1,3)], col=rcolor[n], lty=ml, lwd=2)
    }
    
  }
  
  #legend
  legend(x="topright",legend=group, col=rcolor, lty=c(2,1), ncol=2, lwd=2, cex=0.8)
  
  
  dev.off()
  
  
  #generate dataset with all previous sequence
  BigX = matrix(0, nrow=(2*w+1), ncol=length(group))
  colname=""
  i = 0
  for (g in group){
    i = i+1
    gg = paste("X",g, sep="")
    XX = get(gg)
    BigX[,i]=XX[,3]
    colname[i] = g
  }
  colnames(BigX)=colname
  
  myacf01=function(X){
    acf(X)$acf[2]
  }
  myacf02=function(X){
    acf(X)$acf[3]
  }
  myacf03=function(X){
    acf(X)$acf[4]
  }
  
  #Stats before t0
  mean0=apply(BigX[c(1:w),],2,mean)
  sd0=apply(BigX[c(1:w),],2,sd)
  acf010=apply(BigX[c(1:w),],2,myacf01)
  acf020=apply(BigX[c(1:w),],2,myacf02)
  acf030=apply(BigX[c(1:w),],2,myacf03)
  yy = paste("statbefore",w,sep="")
  assign(yy,cbind(mean0,sd0,acf010,acf020,acf030))
  
  #Stats after t0
  mean1=apply(BigX[c(-1:(-1-w)),],2,mean)
  sd1=apply(BigX[c(-1:(-1-w)),],2,sd)
  acf110=apply(BigX[c(-1:(-1-w)),],2,myacf01)
  acf120=apply(BigX[c(-1:(-1-w)),],2,myacf02)
  acf130=apply(BigX[c(-1:(-1-w)),],2,myacf03)
  yyy = paste("statafter",w, sep="")
  assign(yyy,cbind(mean1,sd1,acf110,acf120,acf130))
  
  #Mean difference
  yyyy = paste("meanDiff",w, sep="")
  assign(yyyy,(get(yyy)[,1]-get(yy)[,1]))
}
output60=cbind(statbefore60,statafter60,meanDiff60)
output120=cbind(statbefore120,statafter120,meanDiff120)

View(output60)
View(output120)

