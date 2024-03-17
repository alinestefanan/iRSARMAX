# n: tamanho da amostra
# freq: frequencia anual de observacoes (12 para observacoes mensais)
# S: periodicidade sazonal

sample.irarma <- function(n,beta0=c(0.0),phi=c(0.0),theta=c(0.0),PHI=c(0.0),THETA=c(0.0),lambda0=0.1,lambda1=NA,S=12,ar=c(0.0),ma=c(0.0),AR=c(0.0),MA=c(0.0),freq=12,link="log")
{

  if(phi[1]!=0 & ar[1]==0)
  {
    ar <- 1:length(phi)
  }
  
  if(theta[1]!=0 & ma[1]==0)
  {
    ma <- 1:length(theta)
  }
  
  if(PHI[1]!=0 & AR[1]==0)
  {
    AR <- 1:length(PHI)
  }
  
  if(THETA[1]!=0 & MA[1]==0)
  {
    MA <- 1:length(THETA)
  }
  
  p <- max(ar)
  q <- max(ma)
  P <- max(AR)
  Q <- max(MA)
  m <- 2*max(p,q,P,Q,S*P,S*Q,S*P+p,S*Q+q,na.rm=T)#tirar 2x?
  X<-matrix(rep(1,(n+m)), nrow=(n+m), ncol=1, byrow=F) #default

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
    stop(paste(linktemp, "link not available"))
  } 
  
  link <- structure(list(link = linktemp, 
                         linkfun = stats$linkfun,
                         linkinv = stats$linkinv,
                         mu.eta = stats$mu.eta,#derivada de mu em relação a eta
                         diflink = function(t) 1/(stats$mu.eta(stats$linkfun(t)))
  )
  )
  
  linkfun <- link$linkfun
  linkinv <- link$linkinv
  mu.eta <-  link$mu.eta
  diflink <- link$diflink
  
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
 
  #gerar amostra mean-based inflated rayleigh
  ir.rq<-function(n,mu,lambda)
  {
    u<- runif(n)
    y=c()
    for (i in 1:n){
      if(u[i]<=lambda[i]){y[i]=0}#acumulada quando y é zero
      else{y[i]<- (sqrt(-4*(mu[i]^2)*log(1-(u[i]-lambda[i])/(1-lambda[i]))/pi))
        if (is.na(y[i])) y[i]=0
        if (is.infinite(y[i])) y[i]=.Machine$double.xmax#se mu é infinito, y fica nan
        if (y[i]>.Machine$double.xmax) y[i]=.Machine$double.xmax
      }
    }
    return(y)
  }
  
  ar_par_index <- operator(phi,PHI,ar,S*AR)
  ma_par_index <- operator(theta,THETA,ma,S*MA)
  ar_par <- ar_par_index$parameters
  ar_ind <- ar_par_index$index
  ma_par <- ma_par_index$parameters
  ma_ind <- ma_par_index$index

  ynew <-rep(beta0[1],(n+m)) # primeiro valor aleatório= beta0[1]#ynew
  mu <- linkinv(ynew)#ynew
  lambda<-rep(lambda0,(n+m))
  error<-rep(0,n+m) # E(error)=0 
  eta1<-eta2<-c()
  y<- rep(0,n+m) 
  
  critical<-c()
if (is.na(lambda1) | lambda1==0){
  for(i in (m+1):(n+m)) 
  {
    eta1[i] <- X[i,]%*%as.matrix(lambda0) #+ sum(lambda1*(ynew[i-1]))
    eta2[i] <- X[i,]%*%as.matrix(beta0) + sum(ar_par*(ynew[i-ar_ind])) - sum(ma_par*error[i-ma_ind])
    
    lambda[i] <-exp(eta1[i])/(exp(eta1[i])+1)##inversa logit
    mu[i] <-linkinv(eta2[i])
    if (is.na(mu[i])) mu[i]=0
    if (mu[i]>.Machine$double.xmax){mu[i]=.Machine$double.xmax}
    
    y[i] <- ir.rq(1, mu[i], lambda[i])###gerando amostra reparametrizada
    ynew[i] <-y[i] 
    error[i] <- ynew[i]-linkinv(eta2[i]) #residuals
  }
}else{
    for(i in (m+1):(n+m)) 
    {
      eta1[i] <- X[i,]%*%as.matrix(lambda0) + sum(lambda1*(ynew[i-1]))
      eta2[i] <- X[i,]%*%as.matrix(beta0) + sum(ar_par*(ynew[i-ar_ind])) - sum(ma_par*error[i-ma_ind])
      
      lambda[i] <-exp(eta1[i])/(exp(eta1[i])+1)##inversa logit
      mu[i] <-linkinv(eta2[i])
      if (is.na(mu[i])) mu[i]=0
      if (mu[i]>.Machine$double.xmax){ mu[i]=.Machine$double.xmax}
      y[i] <- ir.rq(1, mu[i], lambda[i])###gerando amostra reparametrizada
      ynew[i] <-y[i] 
      error[i] <- ynew[i]-linkinv(eta2[i]) #residuals
    }
}

  return( ts(y[(m+1):(n+m)],frequency=freq) )
}


