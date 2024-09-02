# y: time series
# S: seasonal periodicity
# exvar: covariate column matrix
# resid: 0 = real-observed; 1 = Standardized residuals; 2 = Deviance residuals; 3 = Quantile residuals; 4 = Randomized quantile residuals with uniform distribution
# steps: how many steps to forecast

EMV.irarma <- function(y,ar=c(0.0),ma=c(0.0),AR=c(0.0),MA=c(0.0),S=12,exvar=matrix(NA, nrow=1, ncol=1, byrow=F),resid=4,aclag=10,steps=12,validation=T,graph=T,print=T,check=F,link="log")
  
{
  k<-0 #default
  if(validation==T){
    n <- length(y)-steps 
  }else{
    n <- length(y) 
  }
  X<-matrix(rep(1,n), nrow=n, ncol=1, byrow=F) #default
  
  ##funções de ligação
  linktemp <- substitute(link)
  if (!is.character(linktemp))
  {
    linktemp <- deparse(linktemp)
    if (linktemp == "link")
      linktemp <- eval(link)
  }
  if (any(linktemp == c("logit", "probit", "cloglog", "cauchit", "identity", "log", "sqrt", "1/mu^2", "inverse")))
  {  
    stats <- make.link(linktemp)
  }else{
    stop(paste(linktemp, "link not available, available links are \"logit\", ",
               "\"probit\" and \"cloglog\""))
  } 
  
  link <- structure(list(link = linktemp, 
                         linkinv = stats$linkinv,
                         mu.eta = stats$mu.eta#derivada de mu em relação a eta2
  )
  )
  linkinv <- link$linkinv
  mu.eta <-  link$mu.eta
  
  #mean-based inflated rayleigh probability density function
  ir.pdf <- Vectorize(function(y,lambda,mu,log = FALSE){
    pdf=rep(0,length(y))
    for (i in 1:length(y)){
      if (y[i]==0){
        pdf[i]=lambda[i]
      }else{
        e=-(pi*(y[i]^2))/(4*(mu[i]^2))
        #if (e < (-.Machine$double.xmax)) e<-0#mu tende a zero
        pdf[i]=(1-lambda[i])*((pi*y[i])/(2*(mu[i]^2)))*exp(e)
      }
      if(is.na(pdf[i]) | is.nan(pdf[i]) | is.infinite(pdf[i])) pdf[i]<-1-.Machine$double.eps
      if(pdf[i]<.Machine$double.eps) pdf[i]<-.Machine$double.eps
      if (pdf[i]>.Machine$double.xmax) pdf[i]<-.Machine$double.xmax
    }
    #print(pdf)
    logden <- log(pdf)
    val <- ifelse(log, logden, exp(logden))
    
    return(val)
  }) 
  
  #mean-based inflated rayleigh cumulative distribution function 
  ir.cdf <- Vectorize(function(y,lambda,mu,log.p = FALSE){
    cdf=rep(0,length(y))
    for (i in 1:length(y)){
      e=-(pi*(y[i]^2))/(4*(mu[i]^2))
      #if (e < -.Machine$double.xmax) e<-0
      if (y[i]==0){
        cdf[i] <- lambda[i]
      }else{
        cdf[i] <- lambda[i]+(1-lambda[i])*(1-exp(e))
      }
      #print(cdf)
      if (is.na(cdf[i])) cdf[i]<-0
      if (cdf[i]<0) cdf[i]<-0
      if (cdf[i]>1) cdf[i]<-1
    }
    val <- ifelse(log.p, log(cdf), cdf)
    return(val)
  })
  #ir.cdf(100,0.1,2)
  
  #mean-based inflated rayleigh quantile function 
  ir.q<-Vectorize(function(u,lambda,mu)
  {
    n<- length(u) 
    Qu=c()
    for (i in 1:n){
      #print("lambda[i]");print(lambda[i])
      if(is.na(lambda[i]))
      {z$RMC=1
      warning("lambda error")
      return(z)
      }
      if(u[i]<=lambda[i]){Qu[i]=0}#acumulada quando y é zero
      else{
        Qu[i]<- (sqrt(-4*(mu[i]^2)*log(1-(u[i]-lambda[i])/(1-lambda[i]))/pi))
        if (is.na(Qu[i])) Qu[i]=0
        if (is.infinite(Qu[i])) Qu[i]=.Machine$double.xmax#se mu é infinito, y fica nan
        if (Qu[i]>.Machine$double.xmax) Qu[i]=.Machine$double.xmax
      }
    }
    return(Qu)
  })
  #ir.q(ir.cdf(0.5,0.1,2),0.1,2)
  
  #ir.cdf(ir.q(0.5,0.1,2),0.1,2) 
  
  Monti.test<-function (x, lag = 1, type = c("Ljung-Box"), fitdf = 0) ##Ljung-Box test as the Box.test function using pacf replacing acf
  {
    if (NCOL(x) > 1) 
      stop("x is not a vector or univariate time series")
    DNAME <- deparse(substitute(x))
    type <- match.arg(type)
    cor <- pacf(x, lag.max = lag, plot = FALSE, na.action = na.pass)
    n <- sum(!is.na(x))
    PARAMETER <- c(df = lag - fitdf)
    obs <- cor$acf[1:(lag )]
    if (type == "Ljung-Box") {
      METHOD <- "Monti test via Ljung-Box test"
      STATISTIC <- n * (n + 2) * sum(1/seq.int(n - 1, n - lag) * 
                                       obs^2)
      PVAL <- 1 - pchisq(STATISTIC, lag - fitdf)
    }
    names(STATISTIC) <- "X-squared"
    structure(list(statistic = STATISTIC, parameter = PARAMETER, 
                   p.value = PVAL, method = METHOD, data.name = DNAME), 
              class = "htest")
  }
  
  vector.root=function(order,coeff){
    if(sum(order)!=0){
      o=seq(1:max(order))
      p0=rep(0,max(order))
      for (i in 1:length(order)){
        p0[o==order[i]]=coeff[i]
      }
      return(c(1,p0))
    }else{return(c(1,0))}
  }
  #multiplicações no eta
  operator<-function(phi,PHI,ar,AR)
  {
    parameters<-c(phi,PHI)
    index<-c(ar,AR)
    j1<-1
    for(j in ar)
    {
      J1<-1
      for(J in AR)
      {      
        parameters<-c(parameters, -phi[j1]*PHI[J1])
        index<-c(index, (j+J))
        J1<-J1+1
      }
      j1<-j1+1
    }
    z<-c()
    z$parameters<-parameters
    z$index<-index
    return(z)
  }
  p <- max(ar)
  q <- max(ma)
  P <- max(AR)
  Q <- max(MA)
  m <- max(p,q,P,Q,S*P,S*Q,S*P+p,S*Q+q,na.rm=T)
  p1 <- length(ar)
  q1 <- length(ma)
  P1 <- length(AR)
  Q1 <- length(MA)
  
  ynew <-y[1:n]  
  
  loglik <- function(z) 
  {
    beta0 <- z[1:(k+1)]
    phi = z[(k+2):(p1+k+1)] 
    theta =z[(p1+k+2):(p1+q1+k+1)]
    PHI = z[(p1+q1+2):(p1+q1+P1+1)]
    THETA = z[(p1+P1+q1+2):(p1+P1+q1+Q1+1)]
    lambda0 <- z[(p1+P1+q1+Q1+2)] 
    #lambda1 <- z[(p1+P1+q1+Q1+3)]
    
    error<-rep(0,n) # E(error)=0 
    eta1<-rep(NA,n)
    eta2<-rep(NA,n)
    lambda <- rep(0,n)
    mu<-rep(NA,n)
    
    ar_par_index <- operator(phi,PHI,ar,S*AR)
    ma_par_index <- operator(theta,THETA,ma,S*MA)
    
    ar_par <- ar_par_index$parameters
    ar_ind <- ar_par_index$index
    
    ma_par <- ma_par_index$parameters
    ma_ind <- ma_par_index$index
    
    for(i in (m+1):n)
    {
      eta1[i] <- X[i,]%*%as.matrix(lambda0)# + sum(lambda1*(ynew[i-1]))
      eta2[i] <- X[i,]%*%as.matrix(beta0) + sum(ar_par*(ynew[i-ar_ind]) ) - sum(ma_par*error[i-ma_ind])
      error[i] <- ynew[i]-linkinv(eta2[i]) #residuals 
    }
    lambda <-exp(eta1[(m+1):n])/(exp(eta1[(m+1):n])+1)##inversa logit
    mu <-linkinv(eta2[(m+1):n])
    y1<-y[(m+1):n]
    
    l=c()
    llambda=c()
    for (i in 1:length(y1)){
      llambda[i]=log(1-lambda[i])
      if(is.nan(llambda[i])){llambda[i]=0}
      if(y1[i]==0){l[i]=log(lambda[i])}else{l[i]=sum(llambda[i]+log(pi)+log(y1[i])-log(2)-2*log(mu[i])-(pi*(y1[i]^2))/(4*(mu[i]^2)))}
    }     
    #print("sum(l)");print(sum(l))
    #print("sum(ir.pdf(y1,lambda,mu,log = T))");print(sum(ir.pdf(y1,lambda,mu,log = T)))
    return(sum(l))
    #return(sum(ir.pdf(y1,lambda,mu,log = T)))
  }#fim loglik
  
  score <- function(z) 
  {
    beta0 <- z[1:(k+1)]
    phi = z[(k+2):(p1+k+1)] 
    theta =z[(p1+k+2):(p1+q1+k+1)]
    PHI = z[(p1+q1+2):(p1+q1+P1+1)]
    THETA = z[(p1+P1+q1+2):(p1+P1+q1+Q1+1)]
    lambda0 <- z[(p1+P1+q1+Q1+2)] # lambda parameter
    #lambda1 <- z[(p1+P1+q1+Q1+3)]
    
    error<-rep(0,n) # E(error)=0 
    eta1<-rep(0,n)
    eta2<-rep(0,n)
    mu<-rep(0,n)
    lambda <- rep(0,n)
    
    ar_par_index <- operator(phi,PHI,ar,S*AR)
    ma_par_index <- operator(theta,THETA,ma,S*MA)
    
    ar_par <- ar_par_index$parameters
    ar_ind <- ar_par_index$index
    
    ma_par <- ma_par_index$parameters
    ma_ind <- ma_par_index$index
    
    for(i in (m+1):n)
    {
      eta1[i] <- X[i,]%*%as.matrix(lambda0) #+ sum(lambda1*(ynew[i-1]))
      eta2[i] <- X[i,]%*%as.matrix(beta0) + sum(ar_par*(ynew[i-ar_ind]) ) - sum(ma_par*error[i-ma_ind])
      error[i] <- ynew[i]-linkinv(eta2[i])
    }
    lambda <-exp(eta1[(m+1):n])/(exp(eta1[(m+1):n])+1)
    mu <- linkinv(eta2[(m+1):n])
    
    y1<-y[(m+1):n]
    
    ####START DERIVATIVE FROM LOG LIKELIHOOD WITH RESPECT TO MU
    ###########################################################################################################
    mustar=c()
    for (i in 1:length(y1)){
      if (y1[i]==0){mustar[i]=0}else{mustar[i]=(pi*(y1[i]^2))/(2*(mu[i]^3))-2/(mu[i])}
      if(is.na(mustar[i])){mustar[i]=0}
    }
    #print("mustar");print(mustar)
    ########################################################################################################### 
    ####END DERIVATIVE FROM LOG LIKELIHOOD WITH RESPECT TO MU   
    
    ##############################################################################
    ##### START DERIVATIVE FROM LOG LIKELIHOOD WITH RESPECT TO lambda
    Ulambda=c()
    for (i in 1:length(y1)){
      if (y1[i]==0){Ulambda[i]=1/lambda[i]}else{Ulambda[i]=-1/(1-lambda[i])}
      if (is.nan(Ulambda[i])) Ulambda[i]=0
      if (is.infinite(Ulambda[i])) Ulambda[i]=0
    }
    ##### END DERIVATIVE FROM LOG LIKELIHOOD WITH RESPECT TO lambda
    ##############################################################################
    
    mT1 <- diag(exp(eta1[(m+1):n])/((exp(eta1[(m+1):n])+1)^2))#derivada da inversa#
    mT2 <- diag(mu.eta(eta2[(m+1):n]))
    
    ylstar <- matrix((Ulambda),ncol=1)
    ymstar <- matrix((mustar),ncol=1)
    
    B0 <- matrix(rep(NA,(n-m)*length(beta0)),ncol=length(beta0))#intercepto e covariáveis  
    
    deta.dbeta0 <- matrix(0,ncol=k+1,nrow=n)
    
    for(i in 1:(n-m))
    {
      for(j in 1:length(beta0))
      {
        B0[i,j] <- X[i+m,j] 
      }
    }
    
    # L <- matrix(rep(NA,(n-m)),ncol=1)#lambda1
    # for(i in 1:(n-m))
    # {
    #   L[i,]= sum(ynew[i+m-1])
    # }  
    
    A <- matrix(rep(NA,(n-m)*(p1)),ncol=(p1))# ar
    deta.dphi <- matrix(0, ncol=p1,nrow=n)
    
    for(i in 1:(n-m))
    {
      for(j in 1:p1)
      {
        A[i,j]= (ynew[i+m-ar[j]] )-sum(PHI%*%(ynew[i+m-(S*AR+ar[j])]  ))
      }
    }  
    
    R <- matrix(rep(NA,(n-m)*q1),ncol=q1)# theta
    deta.dtheta<- matrix(0, ncol=q1,nrow=n)
    
    for(i in 1:(n-m))
    {
      for(j in 1:q1)
      {
        R[i,j] <- -sum(error[i+m-c(ma[j],ma[j]+(S*MA))]*c(1,-THETA))
      }
    }
    
    deta.dPHI <- matrix(0, ncol=P1,nrow=n)
    As <- matrix(rep(NA,(n-m)*(P1)),ncol=(P1))# SAR
    for(i in 1:(n-m))
    {
      for(j in 1:P1)
      {
        As[i,j] <- (ynew[i+m-(S*AR[j])]  )-(phi%*%(ynew[i+m-(S*AR[j]+ar)] ))
      }
    }  
    
    deta.dTHETA <- matrix(0, ncol=Q1,nrow=n)
    Rs <- matrix(rep(NA,(n-m)*Q1),ncol=Q1)# THETA SMA
    for(i in 1:(n-m))
    {
      for(j in 1:Q1)
      {
        Rs[i,j] <- -sum(error[i+m-c(S*MA[j],S*MA[j]+(ma))]*c(1,-theta))
      }
    }
    
    for(i in (m+1):n)
    {
      deta.dbeta0[i,]<-  B0[(i-m),] + ma_par%*%(mu.eta(eta2[i-ma_ind])*deta.dbeta0[i-ma_ind,])# ma_par%*%deta.dbeta0[i-ma_ind,]
      
      deta.dphi[i,]<- A[(i-m),] + ma_par%*%(mu.eta(eta2[i-ma_ind])*deta.dphi[i-ma_ind,]) #ma_par%*%deta.dphi[i-ma_ind,]
      
      deta.dPHI[i,]<- As[(i-m),] + ma_par%*%(mu.eta(eta2[i-ma_ind])*deta.dPHI[i-ma_ind,]) #ma_par%*%deta.dPHI[i-ma_ind,]
      
      deta.dtheta[i,]<- R[(i-m),] + ma_par%*%(mu.eta(eta2[i-ma_ind])*deta.dtheta[i-ma_ind,]) #ma_par%*%deta.dtheta[i-ma_ind,]
      
      deta.dTHETA[i,]<- Rs[(i-m),] +ma_par%*%(mu.eta(eta2[i-ma_ind])*deta.dTHETA[i-ma_ind,])  #ma_par%*%deta.dTHETA[i-ma_ind,]
    }
    
    mM0 <- deta.dbeta0[(m+1):n,]
    pp <- deta.dphi[(m+1):n,]
    qq <- deta.dtheta[(m+1):n,]
    PP <- deta.dPHI[(m+1):n,]
    QQ <- deta.dTHETA[(m+1):n,]
    
    Ubeta0 <-  t(mM0) %*% mT2 %*% ymstar
    Uphi <-    t(pp) %*% mT2 %*% ymstar
    Utheta <- t(qq) %*% mT2 %*% ymstar
    UPHI <-    t(PP) %*% mT2 %*% ymstar
    UTHETA <-  t(QQ) %*% mT2 %*% ymstar
    Ulambda0=t(B0) %*% mT1 %*% ylstar
   # Ulambda1=t(L) %*% mT1 %*% ylstar
    
    score<-c(Ubeta0,Uphi,Utheta, UPHI,UTHETA,Ulambda0#,Ulambda1
             )
    return(score)
  }#fim score
  
  y1<-y[(m+1):n] 
  
  reg <- c(0,rep(0,p1+q1+P1+Q1), length(y1[which(y1==0)])/length(y1)#,0
           ) 
  z=c()
  opt.error<- tryCatch(optim(reg, loglik, score, method = "BFGS",control = list(fnscale = -1)), error = function(e) return("error"))
  if(opt.error[1] == "error")
  {z$RMC=1
  #stop("optim error")
  warning("optim error")
  return(z)
  }
  
  opt <- optim(reg, loglik, score,method = "BFGS",hessian=T,control = list(fnscale = -1))#, maxit = maxit1, reltol = 1e-12))
  
  if (opt$conv != 0)
  {
    warning("FUNCTION DID NOT CONVERGE WITH ANALITICAL GRADIENT!")
    z$RMC=1
    return(z)
  }
  
  z$conv <- opt$conv
  coef <- (opt$par)[1:(p1+q1+P1+Q1+#3
                         2)]
  z$coeff <- coef
  
  beta0 <-coef[1] #intercept 
  phi<-coef[(2):(p1+1)] #ar
  theta <-coef[(p1+2):(p1+q1+1)] #ma
  PHI <-coef[(p1+q1+2):(p1+q1+P1+1)] #AR
  THETA <-coef[(p1+P1+q1+2):(p1+P1+q1+Q1+1)] #MA
  lambda0 <-coef[(p1+P1+q1+Q1+2)] # lambda parameter
  #lambda1 <-coef[(p1+P1+q1+Q1+3)]
  
  z$beta0 <- beta0
  z$phi <- phi
  z$theta <- theta
  z$Phi <- PHI
  z$Theta <- THETA
  z$lambda0 <- lambda0
  #z$lambda1 <- lambda1
  
  z$ar=ar
  z$ma=ma
  z$AR=AR
  z$MA=MA
  z$delta=m
  z$RMC=0
  z$roots=c(abs(polyroot(vector.root(z$ar,z$phi))),abs(polyroot(vector.root(z$ma,z$theta))),abs(polyroot(vector.root(z$AR,z$Phi))),abs(polyroot(vector.root(z$MA,z$Theta))))
  
 # if(any(z$roots<1)){warning("root(s) within the unity circle");z$RMC=1}
  
  errorhat <- rep(0,n) # E(error)=0
  etahat1 <- rep(0,n)
  etahat2 <- rep(0,n)
  muhat<- rep(0,n)
  lambdahat <- rep(0,n)
  
  ar_par_index <- operator(phi,PHI,ar,S*AR)
  ma_par_index <- operator(theta,THETA,ma,S*MA)
  
  ar_par <- ar_par_index$parameters
  ar_ind <- ar_par_index$index
  
  ma_par <- ma_par_index$parameters
  ma_ind <- ma_par_index$index
  
  for(i in (m+1):n)
  {
    etahat1[i] <- X[i,]%*%as.matrix(lambda0) #+ sum(lambda1*(ynew[i-1]))
    etahat2[i] <- X[i,]%*%as.matrix(beta0) + sum(ar_par*(ynew[i-ar_ind]) ) - sum(ma_par*errorhat[i-ma_ind])
    errorhat[i] <- ynew[i]-linkinv(etahat2[i])
  }
  lambdahat <-exp(etahat1[(m+1):n])/(exp(etahat1[(m+1):n])+1)
  z$lambdahat=lambdahat
  muhat <- linkinv(etahat2[(m+1):n])
  y1 <- y[(m+1):n]
  # z$fitted <- ts(c(rep(NA,m),muhat),start=start(y),frequency=frequency(y))
  z$fitted<-ts(c(rep(NA,m),ir.q(u=rep(0.5,length(lambdahat)),lambda=lambdahat,mu=muhat)),start=start(y),frequency=frequency(y)) 
  z$etahat2 <- etahat2
  z$errorhat <- errorhat
  
  ########################################################################
  ################### Analytic Fisher Information Matrix #################
  ########################################################################
  
  Fisher.inf<-function(y1,muhat,lambdahat)
  {
    ####START SECOND DERIVATIVE FROM LOG LIKELIHOOD IN RESPECT TO MU
    ###########################################################################################################
    muhatstar.sec=c()
    for (i in 1:length(y1)){
      if(y1[i]==0){muhatstar.sec[i] =0}else{
        # muhatstar.sec[i] =  2/(muhat[i]^2) - 3*pi*(y1[i]^2)/(2*(muhat[i]^4))#segunda derivada
        muhatstar.sec[i] = (2-6*(1-lambdahat[i]))/(muhat[i]^2)#esperanca segunda derivada
      }
      if(is.na(muhatstar.sec[i])|is.nan(muhatstar.sec[i]) |is.infinite(muhatstar.sec[i])){muhatstar.sec[i]=0}
    }
    ########################################################################################################### 
    ####END SECOND DERIVATIVE FROM LOG LIKELIHOOD IN RESPECTO TO MU   
    
    B0 <- matrix(rep(NA,(n-m)*length(beta0)),ncol=length(beta0))#intercepto
    for(i in 1:(n-m))
    {
      for(j in 1:length(beta0))
      {
        B0[i,j] <- X[i+m,j] 
      }
    }
    # L <- matrix(rep(NA,(n-m)),ncol=1)#lambda1
    # for(i in 1:(n-m))
    # {
    #   L[i,]= sum(ynew[i+m-1] )
    # }  
    A <- matrix(rep(NA,(n-m)*(p1)),ncol=(p1))# ar
    for(i in 1:(n-m))
    {
      for(j in 1:p1)
      {
        A[i,j]= (ynew[i+m-ar[j]] )-sum(PHI%*%(ynew[i+m-(S*AR+ar[j])]  ))
      }
    }    
    R <- matrix(rep(NA,(n-m)*q1),ncol=q1)# theta
    for(i in 1:(n-m))
    {
      for(j in 1:q1)
      {
        R[i,j] <- -sum(errorhat[i+m-c(ma[j],ma[j]+(S*MA))]*c(1,-THETA))
      }
    }
    As <- matrix(rep(NA,n*P1),ncol=(P1))# SAR
    for(i in (m+1):n)
    {
      for(j in 1:P1)
      {
        As[i,j] <- (ynew[i-(S*AR[j])])-sum(phi%*%(ynew[i-(S*AR[j]+ar)] ))
      }
    }    
    Rs <- matrix(rep(NA,n*Q1),ncol=Q1)# THETA SMA
    for(i in (m+1):n)
    {
      for(j in 1:Q1)
      {
        Rs[i,j] <- -sum(errorhat[i-c(S*MA[j],S*MA[j]+(ma))]*c(1,-theta))
      }
    }
    # aAs <- array(NA,c(p1,P1,n))# arSAR
    # for(i in (m+1):n)
    # {
    #   for(b in 1:P1)
    #   {
    #     for(a in 1:p1)
    #     {
    #       aAs[a,b,i] <- -ynew[i-(S*AR[b]+ar[a])]
    #     }
    #   }
    # }
    # 
    # RRs <- array(NA,c(q1,Q1,n))# theta(THETA SMA)
    # for(i in (m+1):n)
    # {
    #   for(b in 1:Q1)
    #   {
    #     for(a in 1:q1)
    #     {
    #       RRs[a,b,i] <- errorhat[i-(S*MA[b]+ma[a])]
    #     }
    #   }
    # }
    deta.dbeta0 <- matrix(0,ncol=1,nrow=n)
    deta.dphi <- matrix(0, ncol=p1,nrow=n)
    deta.dtheta<- matrix(0, ncol=q1,nrow=n)
    deta.dPHI <- matrix(0, ncol=P1,nrow=n)
    deta.dTHETA <- matrix(0, ncol=Q1,nrow=n)
    # deta.dbeta0beta0<- matrix(0, ncol=1,nrow=n)
    # deta.dbeta0phi<- matrix(0, ncol=p1,nrow=n)
    # deta.dbeta0theta<- matrix(0, ncol=q1,nrow=n)
    # deta.dbeta0PHI<- matrix(0, ncol=P1,nrow=n)
    # deta.dbeta0THETA<- matrix(0, ncol=Q1,nrow=n)
    # deta.dphiphi<-array(0,dim=c(p1,p1,n))
    # deta.dphitheta<-array(0,dim=c(p1,q1,n))
    # deta.dphiPHI<-array(0,dim=c(p1,P1,n))
    # deta.dphiTHETA<-array(0,dim=c(p1,Q1,n))
    # deta.dthetatheta<-array(0,dim=c(q1,q1,n))
    # deta.dthetaPHI<-array(0,dim=c(q1,P1,n))
    # deta.dthetaTHETA<-array(0,dim=c(q1,Q1,n))
    # deta.dPHItheta<-array(0,dim=c(P1,q1,n))
    # deta.dPHIPHI<-array(0,dim=c(P1,P1,n))
    # deta.dPHITHETA<-array(0,dim=c(P1,Q1,n))
    # deta.dTHETATHETA<-array(0,dim=c(Q1,Q1,n))
    for(i in (m+1):n)
    {
      deta.dbeta0[i,]<- B0[(i-m),] +  ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dbeta0[i-ma_ind,])
      deta.dphi[i,]<- A[(i-m),] +  ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dphi[i-ma_ind,])
      deta.dtheta[i,]<- R[(i-m),] +  ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dtheta[i-ma_ind,])
      deta.dPHI[i,]<- As[i,] +  ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dPHI[i-ma_ind,])
      deta.dTHETA[i,]<- Rs[i,] +  ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dTHETA[i-ma_ind,])
      # deta.dbeta0beta0[i,]<- 0 + ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dbeta0beta0[i-ma_ind,])
      # deta.dbeta0phi[i,]<- rep(0,p1) + ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dbeta0phi[i-ma_ind,])
      # for(a in 1:q1)
      # {
      #   deta.dbeta0theta[i,a] = deta.dbeta0[i-ma[a],] +ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dbeta0theta[i-ma_ind,a])
      # }
      # deta.dbeta0PHI[i,]<- rep(0,P1) +  ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dbeta0PHI[i-ma_ind,])
      # for(b in 1:Q1)
      # {
      #   deta.dbeta0THETA[i,b] = deta.dbeta0[i-MA[b]*S,] -sum(theta%*%deta.dbeta0[i-(ma+MA[b]*S),]) + sum(ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dbeta0THETA[i-ma_ind,b]))
      #                                                                                                      
      # }
      # 
      # for(b in 1:p1)
      # {
      #   for(a in 1:p1)
      #   {
      #     deta.dphiphi[a,b,i]<- 0 + ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dphiphi[a,b,i-ma_ind])
      #     
      #   }
      # }
      # for(b in 1:q1)
      # {
      #   for(a in 1:p1)
      #   {
      #     deta.dphitheta[a,b,i]= deta.dphi[i-ma[b],a] +ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dphitheta[a,b,i-ma_ind])
      #   }
      # }
      # for(b in 1:P1)
      # {
      #   for(a in 1:p1)
      #   {
      #     deta.dphiPHI[a,b,i]<- aAs[a,b,i] +  ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dphiPHI[a,b,i-ma_ind])
      #   }
      # }
      # for(b in 1:Q1)
      # {
      #   for(a in 1:p1)
      #   {
      #     deta.dphiTHETA[a,b,i]<- deta.dphi[i-MA[b]*S,a] -sum(theta%*%deta.dphi[i-(ma+MA[b]*S),a]) +  sum(ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dphiTHETA[a,b,i-ma_ind]))
      #   }
      # }
      # for(b in 1:q1)
      # {
      #   for(a in 1:q1)
      #   {
      #     deta.dthetatheta[a,b,i]= deta.dtheta[i-ma[a],b] +deta.dtheta[i-ma[b],a]+ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dthetatheta[a,b,i-ma_ind])
      #   }
      # }
      # for(b in 1:Q1)
      # {
      #   for(a in 1:q1)
      #   {
      #     deta.dthetaTHETA[a,b,i]= deta.dtheta[i-MA[b]*S,a]+RRs[a,b,i]-sum(theta%*%deta.dtheta[i-(ma+MA[b]*S),a])+deta.dTHETA[i-ma[a],b]-sum(THETA%*%deta.dTHETA[i-(ma[a]+MA*S),b])+sum(ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dthetaTHETA[a,b,i-ma_ind]))
      #   }
      # }
      # for(b in 1:P1)
      # {
      #   for(a in 1:P1)
      #   {
      #     deta.dPHIPHI[a,b,i]<- 0 +  sum(ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dPHIPHI[a,b,i-ma_ind]))
      #   }
      # }
      # for(b in 1:q1)
      # {
      #   for(a in 1:P1)
      #   {
      #     deta.dPHItheta[a,b,i]= deta.dPHI[i-ma[b],a] -sum(THETA%*%deta.dPHI[i-(ma[b]+MA*S),a])+sum(ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dPHItheta[a,b,i-ma_ind]))
      #   }
      # }
      # for(b in 1:Q1)
      # {
      #   for(a in 1:P1)
      #   {
      #     deta.dPHITHETA[a,b,i]= deta.dPHI[i-MA[b]*S,a] -sum(theta%*%deta.dPHI[i-(ma+MA[b]*S),a])+sum(ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dPHITHETA[a,b,i-ma_ind]))
      #   }
      # }
      # for(b in 1:Q1)
      # {
      #   for(a in 1:Q1)
      #   {
      #     deta.dTHETATHETA[a,b,i]= deta.dTHETA[i-MA[a]*S,b]-sum(theta%*%deta.dTHETA[i-(ma+MA[a]*S),b])+deta.dTHETA[i-MA[b]*S,a]-sum(theta%*%deta.dTHETA[i-(ma+MA[b]*S),a])+ sum(ma_par%*%(mu.eta(etahat2[i-ma_ind])*deta.dTHETATHETA[a,b,i-ma_ind]))
      #   }
      # }
    }
    
    mM0 <- matrix(deta.dbeta0[(m+1):n,],ncol=1,nrow=(n-m))
    pp <- matrix(deta.dphi[(m+1):n,], ncol=p1,nrow=(n-m))
    PP <- matrix(deta.dPHI[(m+1):n,], ncol=P1,nrow=(n-m))
    qq <- matrix(deta.dtheta[(m+1):n,], ncol=q1,nrow=(n-m))
    QQ <- matrix(deta.dTHETA[(m+1):n,], ncol=Q1,nrow=(n-m))
    # mM02<- matrix(deta.dbeta0beta0[(m+1):n,], ncol=1,nrow=(n-m))
    # B0p<- matrix(deta.dbeta0phi[(m+1):n,], ncol=p1,nrow=(n-m))
    # B0q=matrix(deta.dbeta0theta[(m+1):n,], ncol=q1,nrow=(n-m))
    # B0P<- matrix(deta.dbeta0PHI[(m+1):n,], ncol=P1,nrow=(n-m))
    # B0Q=matrix(deta.dbeta0THETA[(m+1):n,], ncol=Q1,nrow=(n-m))
    # pp2<- array(deta.dphiphi[,,(m+1):n],dim=c(p1,p1,(n-m)))
    # pq=array(deta.dphitheta[,,(m+1):n],dim=c(p1,q1,(n-m)))
    # pP <- array(deta.dphiPHI[,,(m+1):n],dim=c(p1,P1,(n-m)))
    # pQ=array(deta.dphiTHETA[,,(m+1):n],dim=c(p1,Q1,(n-m)))
    # qq2=array(deta.dthetatheta[,,(m+1):n],dim=c(q1,q1,(n-m)))
    # qQ=array(deta.dthetaTHETA[,,(m+1):n],dim=c(q1,Q1,(n-m)))
    # PP2<- array(deta.dPHIPHI[,,(m+1):n],dim=c(P1,P1,(n-m)))
    # Pq=array(deta.dPHItheta[,,(m+1):n],dim=c(P1,q1,(n-m)))
    # PQ=array(deta.dPHITHETA[,,(m+1):n],dim=c(P1,Q1,(n-m)))
    # QQ2=array(deta.dTHETATHETA[,,(m+1):n],dim=c(Q1,Q1,(n-m)))
    
    ####START SECOND DERIVATIVE FROM LOG LIKELIHOOD IN RESPECT TO lambda
    ###########################################################################################################
    Ulambdalambda=c()
    for (i in 1:length(y1)){
      if (y1[i]==0){Ulambdalambda[i]=-1/(lambdahat[i]^2)}else{Ulambdalambda[i]=-1/((1-lambdahat[i])^2)}
      if (is.nan(Ulambdalambda[i])) Ulambdalambda[i]=0
      if (is.infinite(Ulambdalambda[i])) Ulambdalambda[i]=0
    }
    ########################################################################################################### 
    ####END SECOND DERIVATIVE FROM LOG LIKELIHOOD IN RESPECT TO lambda
    
    ####START DERIVATIVE FROM [DERIVATIVE FROM LOG LIKELIHOOD IN RESPECT TO MU] IN RESPECT TO lambda
    ###########################################################################################################
    ##lambda is orthogonal in relation to the other parameters 
    Umulambda<-rep(0,(n-m))
    ########################################################################################################### 
    ####END DERIVATIVE FROM [DERIVATIVE FROM LOG LIKELIHOOD IN RESPECT TO MU] IN RESPECT TO lambda
    
    mT1 <- diag(exp(etahat1[(m+1):n])/((exp(etahat1[(m+1):n])+1)^2))
    # mT1 <- diag(-(lambdahat-1)*lambdahat)
    mT2 <- diag(mu.eta(etahat2[(m+1):n]))#diag(mu.eta(etahat2[(m+1):n]))
    mV1 <- diag(Ulambdalambda)
    mV2 <- diag(muhatstar.sec)
    mulambda<-diag(Umulambda)
    vI <- matrix(rep(1,n-m),ncol=1)
    
    KBB <- -(t(mM0)%*%mV2%*%(mT2^2)%*%mM0 )
    KBp <- -(t(mM0)%*%mV2%*%(mT2^2)%*%pp )
    KBq <- -(t(mM0)%*%mV2%*%(mT2^2)%*%qq)
    KBP <- -(t(mM0)%*%mV2%*%(mT2^2)%*%PP)
    KBQ <- -(t(mM0)%*%mV2%*%(mT2^2)%*%QQ)
    KBlambda0 <- -t(mM0)%*% mulambda %*% (mT1^2) %*% vI 
    #KBlambda1 <- -t(mM0)%*% mulambda %*% (mT1^2) %*% L
    
    KpB <- t(KBp)
    Kpp=matrix(rep(NA,p1*p1),ncol=p1)
    if(length(Kpp)==1){
      Kpp <- -(t(pp)%*%mV2%*%(mT2^2)%*%pp)
    }else{
      for(j in 1:p1){
        for(i in 1:p1){
          Kpp[i,j] <- -(t(as.matrix(pp[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(pp[,j]))
        }}
    }
    #print("Kpp");print(Kpp)
    Kpq=matrix(rep(NA,p1*q1),ncol=q1)
    #print("pq");print(pq)
    if(length(Kpq)==1){
      Kpq <- -(t(pp)%*%mV2%*%(mT2^2)%*%qq)
    }else{
      for(j in 1:q1){
        for(i in 1:p1){
          Kpq[i,j] <- -(t(as.matrix(pp[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(qq[,j]))
        }
      }
    }
    #print("Kpq");print(Kpq)
    KpP=matrix(rep(NA,p1*P1),ncol=P1)
    if(length(KpP)==1){
      KpP<- -(t(pp)%*%mV2%*%(mT2^2)%*%PP)
    }else{
      for(j in 1:P1){
        for(i in 1:p1){
          KpP[i,j] <- -(t(as.matrix(pp[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(PP[,j]))
        }
      }}
    KpQ=matrix(rep(NA,p1*Q1),ncol=Q1)
    #print("pQ");print(pQ)
    if(length(KpQ)==1){
      KpQ <- -(t(pp)%*%mV2%*%(mT2^2)%*%QQ)
    }else{
      for(j in 1:Q1){
        for(i in 1:p1){
          KpQ[i,j] <- -(t(as.matrix(pp[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(QQ[,j]))
        }
      }
    }
    Kplambda0 <- -t(pp) %*% mulambda %*% (mT1^2) %*% vI   
    #Kplambda1 <- -t(pp)%*% mulambda %*% (mT1^2) %*% L
    
    KqB <- t(KBq)
    Kqp <- t(Kpq)
    Kqq=matrix(rep(NA,q1*q1),ncol=q1)
    if(length(Kqq)==1){
      Kqq <- -(t(qq)%*%mV2%*%(mT2^2)%*%qq)
    }else{
      for(j in 1:q1){
        for(i in 1:q1){
          Kqq[i,j] <- -(t(as.matrix(qq[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(qq[,j]))
        }}
    }
    #print("Kqq");print(Kqq)
    KqQ=matrix(rep(NA,q1*Q1),ncol=Q1)
    if(length(KqQ)==1){
      KqQ <- -(t(qq)%*%mV2%*%(mT2^2)%*%QQ)
    }else{
      for(j in 1:Q1){
        for(i in 1:q1){
          KqQ[i,j] <- -(t(as.matrix(qq[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(QQ[,j]))
        }
      }}
    Kqlambda0 <- -t(qq)%*% mulambda %*% (mT1^2) %*% vI
    #Kqlambda1 <- -t(qq)%*% mulambda %*% (mT1^2) %*% L
    KPB <- t(KBP)
    KPp <- t(KpP)
    KPq=matrix(rep(NA,P1*q1),ncol=q1)
    if(length(KPq)==1){
      KPq <- -(t(PP)%*%mV2%*%(mT2^2)%*%qq)
    }else{
      for(j in 1:q1){
        for(i in 1:P1){
          KPq[i,j] <- -(t(as.matrix(PP[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(qq[,j]))
        }
      }}
    #print("KPq");print(KPq)
    KqP <- t(KPq)
    KPP=matrix(rep(NA,P1*P1),ncol=P1)
    if(length(KPP)==1){
      KPP <- -(t(PP)%*%mV2%*%(mT2^2)%*%PP)
    }else{
      for(j in 1:P1){
        for(i in 1:P1){
          KPP[i,j] <- -(t(as.matrix(PP[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(PP[,j]))
        }}
    }
    # print("KPP");print(KPP)
    KPQ=matrix(rep(NA,P1*Q1),ncol=Q1)
    if(length(KPQ)==1){
      KPQ <- -(t(PP)%*%mV2%*%(mT2^2)%*%QQ)
    }else{
      for(j in 1:Q1){
        for(i in 1:P1){
          KPQ[i,j] <- -(t(as.matrix(PP[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(QQ[,j]))
        }
      }}
    #print("KPQ");print(KPQ)
    KPlambda0 <- -t(PP)%*% mulambda %*% (mT1^2) %*% vI
    #KPlambda1 <- -t(PP)%*% mulambda %*% (mT1^2) %*% L
    
    KQB <- t(KBQ)
    KQp <- t(KpQ)
    KQq <- t(KqQ)
    KQP <- t(KPQ)
    KQQ=matrix(rep(NA,Q1*Q1),ncol=Q1)
    if(length(KQQ)==1){
      KQQ <- -(t(QQ)%*%mV2%*%(mT2^2)%*%QQ)
    }else{
      for(j in 1:Q1){
        for(i in 1:Q1){
          KQQ[i,j] <- -(t(as.matrix(QQ[,i]))%*%mV2%*%(mT2^2)%*%as.matrix(QQ[,j]))
        }}
    }
    KQlambda0 <- -t(QQ)%*% mulambda %*% (mT1^2) %*% vI
    #KQlambda1 <- -t(QQ)%*% mulambda %*% (mT1^2) %*% L
    
    Klambda0B <- t(KBlambda0)
    Klambda0p <- t(Kplambda0)
    Klambda0q <- t(Kqlambda0)
    Klambda0P <- t(KPlambda0)
    Klambda0Q <- t(KQlambda0)
    #Klambda0lambda1 <- -t(B0)%*% mV1 %*% (mT1^2) %*% L
    Klambda0lambda0 <- -t(B0) %*%  mV1 %*%(mT1^2) %*% vI
    
    # Klambda1B <- t(KBlambda1)
    # Klambda1p <- t(Kplambda1)
    # Klambda1q <- t(Kqlambda1)
    # Klambda1P <- t(KPlambda1)
    # Klambda1Q <- t(KQlambda1)
    # Klambda1lambda0 <- t(Klambda0lambda1)
    # Klambda1lambda1 <- -t(L) %*%  mV1 %*%(mT1^2) %*% L
    
    K <- rbind(
      cbind(KBB,KBp,KBq,KBP,KBQ,KBlambda0#,KBlambda1
            ),
      cbind(KpB,Kpp,Kpq,KpP,KpQ,Kplambda0#,Kplambda1
            ),
      cbind(KqB,Kqp,Kqq,KqP,KqQ,Kqlambda0#,Kqlambda1
            ),
      cbind(KPB,KPp,KPq,KPP,KPQ,KPlambda0#,KPlambda1
            ),
      cbind(KQB,KQp,KQq,KQP,KQQ,KQlambda0#,KQlambda1
            ),
      cbind(Klambda0B,Klambda0p,Klambda0q,Klambda0P,Klambda0Q,Klambda0lambda0#,Klambda0lambda1
            )#,
      #cbind(Klambda1B,Klambda1p,Klambda1q,Klambda1P,Klambda1Q,Klambda1lambda0,Klambda1lambda1)
    )
    return(K)
  }
  K<-Fisher.inf(y1,muhat,lambdahat)
  
  Ksolve<- tryCatch(solve(K), error = function(e) return("error"))
  
  if(Ksolve[1] == "error")
  {z$RMC=1#used at Monte-Carlo simulation for discard from the sample
  warning("Analytic Fisher Information Matrix is not positive semi-definite")
  return(z)#if Analytic Fisher Information Matrix is not positive semi-definite, do not calculate
  }else{sol=try(solve(K))}
  
  v<-diag(sol)#Variância assintótica dos esimadores
  
  for (i in 1:length(v))
  {
    if(is.na(v[i]) | is.nan(v[i]) | v[i]<0 )  {
      z$RMC=1
      warning("Analytic Fisher Information Matrix is not positive semi-definite")
      return(z)#if Analytic Fisher Information Matrix is not positive semi-definite, do not calculate
    }
  }
  
  z$zstat<-z$coeff/sqrt(v)
  #print("Estatísticas Z do Teste de Wald")
  #print(z$zstat)
  #print("Resultado a nível 5%")
  resp<-rep(0,length(z$zstat))
  for (i in 1:length(resp)){
    if(abs(z$zstat[i])>qnorm(0.975))
    {
      resp[i] <- "H0 rejected"
    } else {resp[i] <- "H0 not rejected"}
  }
  #print(resp)
  #print("Intervalos de confiança")
  LI<-z$coeff-qnorm(0.975)*sqrt(v)
  LS<-z$coeff+qnorm(0.975)*sqrt(v)
  z$LI=LI
  z$LS=LS
  z$pvalues<-(1-pnorm(abs(z$zstat)))*2
  
  first_col<-c("intercept",ar, ma, AR, MA, "lambda0 estimator"#, "lambda1 estimator"
               )
  result <- matrix(c(first_col,round(c(z$coeff,z$zstat,LI,LS,z$pvalues),8),resp), nrow=length(opt$par), ncol=7, byrow=F)
  colnames(result) <- c("Estimator","MLE","Wald's Statistic","Lower bound","Upper bound","p-value","Wald'S Test result")
  rownames(result)<-c("", rep("ar",length(ar)), rep("ma",length(ma)), rep("AR",length(AR)), rep("MA",length(MA)),""#,""
                      )
  
  #print(result,quot=F)
  z$coef.result<-result
  z$loglik <- opt$value
  z$maic <- -2*(z$loglik)*(n/(n-m))+2*(length(opt$par)) 
  z$mbic <- -2*(z$loglik)*(n/(n-m))+(length(opt$par))*log(n)
  
  ytofit<-ts(c(y[1:n]),start=start(y),frequency=frequency(y))
  
  
  ###########################
  
  z$serie <- y
  
  ########################################################################
  ######################   residuals   ######################
  ########################################################################
  
  z$resid0 <- y[(m+1):n]-z$fitted[(m+1):n]
  
  ########################################################################
  ######################   standardized residuals   ######################
  ########################################################################
  
  z$resid1 <- (z$resid)/sqrt((1-lambdahat)*(z$fitted[(m+1):n]^2)*(4/pi-1+lambdahat))
  
  ########################################################################
  ########################   deviance residuals   ########################
  ########################################################################
  l_tilde <- (ir.pdf(y[(m+1):n], lambdahat, y[(m+1):n], log = TRUE))#y[(m+1):n] where was mu
  l_hat <- (ir.pdf(y[(m+1):n], lambdahat, z$fitted[(m+1):n], log = TRUE))#z$fitted[(m+1):n] where was mu
  for (i in 1:(n-m)){
    if(is.infinite(l_tilde[i])){l_tilde[i]=0}#log(.Machine$double.eps)
    if(is.infinite(l_hat[i])){l_hat[i]=0}#log(.Machine$double.eps)
  }
  
  dt <- (l_tilde-l_hat)
  dt[which(dt<0)]<-0
  
  r2a<-sign(y[(m+1):n]-z$fitted[(m+1):n])
  r2b<-sqrt(2*(dt))
  z$resid2<-r2a*r2b#deviance residuals
  
  
  z$deviance <- 2*sum(dt)
  z$dof.dev=(n-m-length(opt$par)+2)#desconsidera intercepto do eta e lambda da distribuição
  z$p_deviance <- 1 - pchisq(z$deviance, z$dof.dev)
  z$deviance.star <- 2*sum(dt)*n/(n-m)
  
  ########################################################################
  ########################   quantile residuals   ########################
  ########################################################################
  
  z$resid3 <- as.vector(qnorm(ir.cdf(y[(m+1):n],lambdahat, muhat,log.p = FALSE ) ))
  
  ########################################################################
  ######## randomized quantile residuals with uniform distribution  ######
  ########################################################################
  
  ui<-rep(NA,n)
  for(i in (m+1):n)
  {
    if(y[i]==0) ui[i] <- runif(1,0,lambdahat[i-m])
    if(y[i]!=0) ui[i] <- ir.cdf(y[i],lambdahat[i-m], muhat[i-m],log.p = FALSE)
  }
  z$resid4 <- qnorm(ui[(m+1):n])
  
  ########################################################################
  
  if(resid==0) {
    residual=z$resid0
  }
  if(resid==1) {
    residual=z$resid1
  }
  
  if(resid==2) {
    residual=z$resid2
  }
  if(resid==3) {
    residual=z$resid3
  }
  if(resid==4) {
    residual=z$resid4
  }
  
  z$residual<-residual
  
  measures.fitted=function(yfit,residual){
    ams=c()
    ########################################################################
    ####################### fitted accuracy measures #######################
    ########################################################################  
    mae<-sum(abs(y[(m+1):n]-yfit[(m+1):n]))/(n-m)
    
    sq<-rep(NA,n)
    
    den.MdRAE<-c()
    MdRAE.<-c()
    for(i in (m+1):n)
    {
      sq[i]<-(y[i]-yfit[i])^2
      den.MdRAE[i]=abs(y[i]-y[i-S]) #seasonal, if not, -1 not -S, If our model’s fir equals to the benchmark’s forecast then the result is 1. If the benchmarks fit are better than ours then the result will be above > 1. If ours is better than it’s below 1.
      MdRAE.[i]<-abs(y[i]-yfit[i])/den.MdRAE[i]
      if(y[i]==0 & y[i-S]==0 & yfit[i]==0){MdRAE.[i]=0}
      if(y[i]==0 & y[i-S]==0 & yfit[i]!=0){MdRAE.[i]=1.01}
    }  
    
    mse<-sum(sq[(m+1):n])/(n-m)
    
    rmse<-sqrt(mse)
    
    MdRAE<-median(MdRAE.[(m+1):n])
    MAEnaive<-sum(abs(y[(m+1):(n)]-y[(m+1-S):(n-S)]))/(n-m)#seasonal, if not, -1 not -S
    MASE<-mae/MAEnaive#Its value greater than one (1) indicates the algorithm is performing poorly compared to the naïve benchmark.
    
    #Mean directional accuracy
    sign.y<-sign(y[(m+1):(n)]-y[(m):(n-1)])
    sign.f<-sign(yfit[(m+1):(n)]-y[(m):(n-1)])
    MDA.cont<-0
    for (i in (1):(n-m)){  
      if(sign.y[i]==sign.f[i]){MDA.cont<-MDA.cont+1}  
    }
    MDA<-MDA.cont/(n-m)
    
    accuracy<-matrix(round(c(mae,rmse,MdRAE,MASE,MDA),4), nrow=1, ncol=5, byrow=T)
    colnames(accuracy) <- c("MAE","RMSE","MdRAE","MASE","MDA")
    rownames(accuracy) <- c("")
    
    ########################################################################
    ########################   residual analysis   #########################
    ########################################################################
    
    #null hypothesis: non-autocorrelation
    
    if(AR[1]!=0 | MA[1]!=0){
      aclag <- 2*S
    }
    
    ljungbox<- Box.test(residual, lag = aclag, type = "Ljung-Box", fitdf = (p1+q1+P1+Q1))
    boxpierce<-Box.test(residual, lag = aclag, type = c( "Box-Pierce"), fitdf = (p1+q1+P1+Q1))#Box-Pierce test
    monti<- Monti.test(residual, lag = aclag, type = "Ljung-Box", fitdf = (p1+q1+P1+Q1))
    
    ams$boxpierce<-boxpierce$statistic
    ams$p_boxpierce<-boxpierce$p.value
    
    ams$ljungbox<-ljungbox$statistic
    ams$p_ljungbox<-ljungbox$p.value
    
    ams$monti<-monti$statistic
    ams$p_monti<-monti$p.value
    
    #null hypothesis: non-heteroscedasticity (constant variance)
    
    library(FinTS)
    arch.error<- tryCatch(ArchTest(residual, lags=10), error = function(e) return("error")) 
    if(arch.error[1] !=
       "error")
    {
      arch<-ArchTest(residual, lags=10) 
      ams$arch<-arch$statistic
      ams$p_arch<-arch$p.value
    }else{ams$arch<-NA;ams$p_arch<-NA}
    
    
    #null hypothesis: normality
    library(tseries)
    jarquebera<-jarque.bera.test(residual)
    ams$jarquebera<-jarquebera$statistic
    ams$p_jarquebera<-jarquebera$p.value
    
    library(nortest)
    a.error<- tryCatch(ad.test(residual), error = function(e) return("error")) 
    if(a.error[1] !=
       "error")
    {
      andersondarling=ad.test(residual)
      ams$andersondarling<-andersondarling$statistic
      ams$p_andersondarling<-andersondarling$p.value
      diagnostic<-matrix(round(c(ams$boxpierce,ams$ljungbox,ams$monti,ams$jarquebera,ams$andersondarling,ams$arch,
                                 ams$p_boxpierce,ams$p_ljungbox,ams$p_monti,ams$p_jarquebera,ams$p_andersondarling,ams$p_arch
      ),4), nrow=2, ncol=6, byrow=T)
      colnames(diagnostic) <- c("Box-Pierce test","Ljung-Box tes","Monti test","Jarque-Bera test","Anderson-Darling test","Arch test")
      rownames(diagnostic) <- c("Statistic","P-value")
    }else{
      diagnostic<-matrix(round(c(ams$boxpierce,ams$ljungbox,ams$monti,ams$jarquebera,ams$arch,
                                 ams$p_boxpierce,ams$p_ljungbox,ams$p_monti,ams$p_jarquebera,ams$p_arch
      ),4), nrow=2, ncol=5, byrow=T)
      colnames(diagnostic) <- c("Box-Pierce test","Ljung-Box tes","Monti test","Jarque-Bera test","Arch test")
      rownames(diagnostic) <- c("Statistic","P-value") 
    }    
    ams$accuracy=accuracy
    ams$residual=residual
    ams$diagnostic=diagnostic
    return(ams)
  }
  
  z$accuracyfitted= measures.fitted(z$fitted,residual)$accuracy
  z$residualfitted=measures.fitted(z$fitted,residual)$residual
  z$diagnosticfitted=measures.fitted(z$fitted,residual)$diagnostic
  if(is.na(z$diagnosticfitted[2,ncol(z$diagnosticfitted)])){z$RMC=1}
  mresult<-matrix(round(c(z$loglik,z$maic,z$mbic),4),nrow=3,ncol=1)
  rownames(mresult)<-c("Log-likelihood","AIC","BIC")
  colnames(mresult)<-c("")
  z$mresult<-mresult
  
  ########################################################################
  ########################  out of sample forecast  ######################
  ########################################################################
  if(steps!=0){
    eta1_prev <- c(ynew,rep(NA,steps))
    eta2_prev <- c(ynew,rep(NA,steps))
    y_prev <- c(ynew,rep(NA,steps))
    X_prev<-matrix(rep(1,(n+steps)), nrow=(n+steps), ncol=1, byrow=F)
    lambdaf<-muf<-NA
    
    for(i in 1:steps) 
    {
      eta1_prev[n+i] <- X_prev[n+i,]%*%as.matrix(z$lambda0) #+ sum(z$lambda1*(y_prev[n+i-1]))
      lambdaf[i] <-exp(eta1_prev[n+i])/(exp(eta1_prev[n+i])+1)
      eta2_prev[n+i] <- X_prev[n+i,]%*%as.matrix(z$beta0) + sum(ar_par*(y_prev[n+i-ar_ind]) ) - sum(ma_par*errorhat[n+i-ma_ind])
      muf[i]<-linkinv(eta2_prev[n+i]) 
      y_prev[n+i] <-ir.q(rep(0.5,1),lambda=lambdaf[i],mu=muf[i])
      errorhat[n+i] <- 0 # residuals on the original scale y-mu  
    }
    z$forecast<-ts(c(rep(NA,n),y_prev[(n+1):(n+steps)]),start=start(y),frequency=frequency(y))    
    
    #### rolling window forecast
    
    yr_prev <- c(z$fitted,rep(NA,steps))
    eta2_rw <- c(ynew,rep(NA,steps))
    murwf<-NA
    for(i in 1:steps)
    {
      eta2_rw[n+i] <- X_prev[n+i,1]*z$beta0 + sum(ar_par*(y[n+i-ar_ind]) ) - sum(ma_par*errorhat[n+i-ma_ind])
      murwf[i] <- linkinv(eta2_rw[n+i])
      yr_prev[n+i] <-ir.q(rep(0.5,1),lambda=lambdaf[i],mu=murwf[i])
    }
    z$rollingforecast<-ts(c(rep(NA,n),yr_prev[(n+1):(n+steps)]),start=start(y),frequency=frequency(y))     
    
    }
  ########################################################################
  ########################   forecast analysis   #########################
  ########################################################################
  
  measures.forecast=function(yforecast,steps){
    #print("yforecast");print(yforecast)
    ams=c()
    maef<-sum(abs(y[(n+1):(n+steps)]-yforecast[(n+1):(n+steps)]))/(steps)
    sqf<-rep(NA,steps)
    den.MdRAEf<-c()
    MdRAEf.<-c()
    for(i in 1:steps)
    {
      #print("y[n+i]");print(y[n+i])
      #print("yforecast[n+i]");print(yforecast[n+i])
      sqf[i]<-(y[n+i]-yforecast[n+i])^2
      #print("sqf[i]");print(sqf[i])
      den.MdRAEf[i]=abs(y[n+i]-y[n+i-S]) #seasonal, if not, -1 not -S, If our model’s fir equals to the benchmark’s forecast then the result is 1. If the benchmarks fit are better than ours then the result will be above > 1. If ours is better than it’s below 1.
      MdRAEf.[i]<-abs(y[n+i]-yforecast[n+i])/den.MdRAEf[i]
      if(y[n+i]==0 & y[n+i-S]==0& yforecast[n+i]==0){MdRAEf.[i]=0}
      if(y[n+i]==0 & y[n+i-S]==0& yforecast[n+i]!=0){MdRAEf.[i]=1.01}
    }  
    
    msef<-sum(sqf)/steps
    
    rmsef<-sqrt(msef)
    
    MdRAEf<-median(MdRAEf.)
    
    MAEnaivef<-sum(abs(y[(n+1):(n+steps)]-y[(n+1-S):(n+steps-S)]))/steps
    MASEf<-maef/MAEnaivef
    
    #Mean directional accuracy
    sign.y<-sign(y[(n+1):(n+steps)]-y[(n):(n+steps-1)])
    sign.f<-sign(yforecast[(n+1):(n+steps)]-y[(n):(n+steps-1)])
    MDA.cont<-0
    for (i in 1:steps){
      
      if(sign.y[i]==sign.f[i]){MDA.cont<-MDA.cont+1}
      
    }
    MDAf<-MDA.cont/steps
    
    ams$accuracyforecast<-matrix(round(c(maef,rmsef,MdRAEf,MASEf,MDAf),4), nrow=1, ncol=5, byrow=T)
    colnames(ams$accuracyforecast) <- c("MAE","RMSE","MdRAE","MASE","MDA")
    rownames(ams$accuracyforecast) <- c("")
    return(ams$accuracyforecast)
  }
  
  if(steps!=0){
    if(validation==T){
      accuracytraditionalforecast<-accuracyrollingwindow<-matrix(rep(NA,5*steps),nrow=steps, ncol=5, byrow=T)
      colnames(accuracytraditionalforecast) <- colnames(accuracyrollingwindow) <- c("MAE","RMSE","MdRAE","MASE","MDA")
      rownames(accuracytraditionalforecast) <- rownames(accuracyrollingwindow) <- 1:steps
      for (i in 1:steps){
        accuracytraditionalforecast[i,]<-measures.forecast(y_prev,steps=i)
        accuracyrollingwindow[i,]<-measures.forecast(yr_prev,steps=i)
      }
      z$accuracyforecast<-accuracytraditionalforecast
      z$accuracyrollingwindow<-accuracyrollingwindow
    }
  }
  
  ###################################################
  ######### GRAPHICS ################################
  if(graph==T)
  {
    t<-seq(-5,n+6,by=1)
    w1<-5
    h1<-4
    #postscript(file = "resid_v_ind.pdf",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
    pdf("resid_v_ind.pdf",width=5, height=4)
    {
      par(mfrow=c(1,1))
      par(mar=c(2.8, 2.7, 1, 1))
      par(mgp=c(1.7, 0.45, 0))
      plot(residual,main=" ",xlab="Index",ylab="Residuals", pch = "+",ylim=c(-4,4))
      lines(t,rep(-3,n+12)#length(residual))
            ,lty=2,col=1)
      lines(t,rep(3,n+12)#length(residual))
            ,lty=2,col=1)
      lines(t,rep(-2,n+12)#length(residual))
            ,lty=3,col=1)
      lines(t,rep(2,n+12)#length(residual))
            ,lty=3,col=1)
    }
    dev.off()
    
    #postscript(file = "resid_v_fitted.eps",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
    pdf("resid_v_fitted.pdf",width=5, height=4)
    {
      par(mfrow=c(1,1))
      par(mar=c(2.8, 2.7, 1, 1)) 
      par(mgp=c(1.7, 0.45, 0))
      plot(as.vector(z$fitted[(m+1):n]),as.vector(residual), main=" ", pch = "+",
           xlab="Fitted values",ylab="Residuals",ylim=c(-4,4))
      lines(t,rep(-3,n+12),lty=2,col=1)
      lines(t,rep(3,n+12),lty=2,col=1)
      lines(t,rep(-2,n+12),lty=3,col=1)
      lines(t,rep(2,n+12),lty=3,col=1)
    }
    dev.off()
    
    
    #postscript(file = "obs_v_fit.eps",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
    pdf("obs_v_fit.pdf",width=5, height=4)### abre no navegador google chrome só
    {
      par(mfrow=c(1,1))
      par(mar=c(2.8, 2.7, 1, 1)) 
      par(mgp=c(1.7, 0.45, 0))
      plot(as.vector(z$fitted), as.vector(ytofit), main=" ", pch = "+",
           xlab="Fitted values",ylab="Observed data",
           xlim=c(0.95*min(y),max(y)*1.05),
           ylim=c(0.95*min(y),max(y)*1.05))
      lines(c(-0.2,1.2),c(-0.2,1.2),lty=2)
    }
    dev.off()
    
    #postscript(file = "resid_density.eps",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
    pdf("resid_density.pdf",width=5, height=4)
    {
      par(mfrow=c(1,1))
      par(mar=c(1.5, 2.7, 1, 1)) 
      par(mgp=c(1.7, 0.45, 0))
      densidade<-density(residual)
      plot(densidade,ylab="Density",main=" ",xlab=" ",ylim=c(0,1.15*max(densidade$y)))
      lines(densidade$x,dnorm(densidade$x),lty=2)
      legend("topleft",c("Exact distribution of residuals","Normal approximation"),#pch=vpch,
             pt.bg="white", lty=c(1,2), bty="n")
    }
    dev.off()
    
    #postscript(file = "resid_FAC.eps",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
    pdf("resid_FAC.pdf",width=5, height=4)
    {
      par(mfrow=c(1,1))
      par(mar=c(2.8, 2.7, 1, 1)) 
      par(mgp=c(1.7, 0.45, 0))
      acf(residual,ylab="ACF",xlab="Lag") 
    }
    dev.off()
    
    #postscript(file = "resid_FACP.eps",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
    pdf("resid_FACP.pdf",width=5, height=4)
    {
      par(mfrow=c(1,1))
      par(mar=c(2.8, 2.7, 1, 1)) 
      par(mgp=c(1.7, 0.45, 0))
      pacf(residual,ylab="PACF",xlab="Lag")
    }
    dev.off()
    
    #postscript(file = "qq_plot.eps",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
    pdf("qq_plot.pdf",width=5, height=4)
    {  
      par(mfrow=c(1,1))
      par(mar=c(2.8, 2.7, 1, 1)) 
      par(mgp=c(1.7, 0.45, 0))
      qqnorm(residual, pch = "+",
             xlim=c(0.95*min(residual),max(residual)*1.05),
             ylim=c(0.95*min(residual),max(residual)*1.05),
             main="",xlab="Normal quantiles",ylab="Empirical quantiles")
      lines(c(-10,10),c(-10,10),lty=2)
    }
    dev.off()
    
    #postscript(file = "adjusted.eps",horizontal=F,paper="special",width = w1, height = h1,family = "Times")
    pdf("adjusted.pdf",width=5, height=4)
    {
      par(mfrow=c(1,1))
      par(mar=c(2.8, 2.7, 1, 1)) # margens c(baixo,esq,cima,direia)
      par(mgp=c(1.7, 0.45, 0))
      plot(ytofit,type="l",ylab="Serie",xlab="Time")
      lines(z$fitted,col="blue",lty=2)
      legend("bottomleft",c("Observed data","Fitted values"),#pch=vpch,
             pt.bg="white", lty=c(1,2), bty="n",col=c(1,"blue"))
    }
    dev.off()
    if(steps!=0){
      #postscript(file = "forecast.eps",horizontal=F,paper="special",width = 6, height = 4.7,family = "Times")
      if(steps!=0){
        pdf("fittedforecast.pdf",width=5, height=4)
        {
          fim<-end(y)[1]+end(y)[2]/12
          par(mfrow=c(1,1))
          par(mar=c(2.8, 2.7, 1, 1)) # margens c(baixo,esq,cima,direia)
          par(mgp=c(1.7, 0.45, 0))
          plot(c(z$fitted,y_prev[(n+1):(n+steps)]),type="l",col="blue",lty=2, ylim=c(min(y),max(y)),ylab="Serie",xlab="Time")
          abline(v=fim,lty=2)
          abline(v=n,lty=2)
          lines(as.vector(y))
          legend("bottomleft",c("Observed data","Fitted and forecast values"),#pch=vpch,
                 pt.bg="white", lty=c(1,2), bty="n",col=c(1,"blue"))
        }
        dev.off()
        
        pdf("forecast.pdf",width=5, height=4)
        {
          fim<-end(y)[1]+end(y)[2]/12
          par(mfrow=c(1,1))
          par(mar=c(2.8, 2.7, 1, 1)) # margens c(baixo,esq,cima,direia)
          par(mgp=c(1.7, 0.45, 0))
          plot(y_prev[(n+1):(n+steps)],type="l",col="blue",lty=2, ylim=c(min(y),max(y)),ylab="Serie",xlab="Time")
          abline(v=fim,lty=2)
          abline(v=n,lty=2)
          lines(as.vector(y[(n+1):(n+steps)]))
          legend("bottomleft",c("Observed data","Forecast values"),#pch=vpch,
                 pt.bg="white", lty=c(1,2), bty="n",col=c(1,"blue"))
        }
        dev.off()
      }
    }
  }#END GRAPHICS
  
  if(print==T){
    print("iRSARMA",quote=F)
    print(z$coef.result,quote=F)
    message("")
    print(c("Log-likelihood =",round(z$loglik,4)),quote=F)
    print(c("MAIC =",round(z$maic,4),"MBIC =",round(z$mbic,4)),quote=F)
    print(c("Deviance =",round(z$deviance,4)," DF:",z$dof.dev,"Deviance* =",round(z$deviance.star,4)),quote=F)
    message("")  
    if(resid==0) {
      print("Residuals:",quote=F)
    }
    if(resid==1) {
      print("Standardized residuals:",quote=F)
    }
    
    if(resid==2) {
      print("Deviance residuals:",quote=F)
    }
    if(resid==3) {
      print("Quantile residuals:",quote=F)
    }
    if(resid==4) {
      print("Randomized quantile residuals with uniform distribution:",quote=F)
    }
    print(summary(z$residual))
    message("")
    print(z$diagnosticfitted)
    message("")
    print("Fitted accuracy",quote=F)
    print(z$accuracyfitted)
    message("")
    if(steps!=0 & validation==T){
      print("Traditional forecast accuracy:",quote=F)
      print(z$accuracyforecast)
    message("")
    print("Rolling window forecast accuracy:",quote=F)
    print(z$accuracyrollingwindow)
    }
  }
  
  if(check==TRUE){
    opt2 <- optim(reg, loglik, method = "BFGS", hessian = T, control = list(fnscale = -1))
    library(rootSolve)
    print("verificando derivadas")
    print(gradient(score,opt2$par))
    print(hessian(loglik,opt2$par))
    print("rbind(score(opt$par),gradient(loglik,opt$par))")
    print(rbind(score(opt$par),gradient(loglik,opt$par)))
    print("rbind(score(opt2$par),gradient(loglik,opt2$par))")
    print(rbind(score(opt2$par),gradient(loglik,opt2$par)))
    # print("-hessiana = Matriz de informação observada condicional")
    # print(round(K,4))
    # print("hessiana numerica")
    # print(round(-opt$hessian,4))
    # print("comparando meu cálculo com hessiana da estimação numérica")
    # print(round((K+opt2$hessian),2))
    # print("comparando meu cálculo com hessiana numérica da estimação analítica")
    # print(round((K+opt$hessian),2))
    # print("soma diferença hessiana otimização numérica")
    # print(round(sum(abs(K+opt2$hessian)),2))
    # print("soma diferença hessiana numérica otimização analítica")
    # print(round(sum(abs(K+opt$hessian)),2))
  }
  return(z)
  
}#fim estimação
