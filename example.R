#GENERATE iRSARMA SAMPLE 
source("sample.irarma.R")
y<-sample.irarma(150, beta0=1.6655,phi=0.0532,theta=0,PHI=0,THETA=0,lambda0=-5.2539,lambda1=0.2075)

#iRSARMAX BEST MODEL
source("auto.irarma.R")
iRB=auto.irarma(y,steps=12,max.order=1,type="all")
iRB$first


#iRSARMA FIT
source("fit.R")
iR=irarma(y,ar=c(1),ma=c(1), AR=c(1),MA=c(1),S=12,steps=12,graph=T,print=T)


#iRSARMAX FIT
n=length(y)
level=matrix(rep(c(rep(0,round(n/2)),rep(1,n-round(n/2))),1), nrow=n, ncol=1, byrow=F)

iRX=irarma(y,steps=12,ar=c(1),ma=c(1), AR=c(1),MA=c(1),S=12,exvar=level,print=T,graph=T)