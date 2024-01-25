# y: série temporal
# S: periodicidade sazonal
# exvar: matriz de colunas de covariáveis
# aclag
# link: "logit", "probit" or "cloglog"
# steps: quantos passos a frente prever

irarma <- function(y,ar=c(0.0),ma=c(0.0),AR=c(0.0),MA=c(0.0),S=12,exvar=matrix(NA, nrow=1, ncol=1, byrow=F),aclag=10, resid=4,steps=12,graph=T,check=T,print=T,link="log")
  
{
  if (ar[1]!=0 && ma[1]!=0 && AR[1]!=0 && MA[1]!=0){
    if(#is.matrix(exvar) && is.na(exvar[1,1])==F ||
      is.na(exvar[1])==F)
    {
      source("irarmaSARMAX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarmaSARMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
        }else{
        source("irarmaSARMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irarmaSARMA.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarmaSARMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarmaSARMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#armaSARMA
  else if (ar[1]!=0 && ma[1]!=0 && AR[1]==0 && MA[1]==0){
    if(#is.matrix(exvar) && is.na(exvar[1,1])==F || 
      is.na(exvar[1])==F)
    {
      source("irarmaX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarmaX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarmaX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irarma.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarma0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarma0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#arma
  else if (ar[1]!=0 && ma[1]==0 && AR[1]==0 && MA[1]==0){ 
    if(#is.matrix(exvar) && is.na(exvar[1,1])==F || 
      is.na(exvar[1])==F)
    {
      source("irarX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irar.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irar0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irar0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#ar
  else if (ar[1]==0 && ma[1]!=0 && AR[1]==0 && MA[1]==0){ 
    if(#is.matrix(exvar) && is.na(exvar[1,1])==F || 
      is.na(exvar[1])==F)
    {
      source("irmaX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irmaX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link) 
      }
      }else{
        source("irmaX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link) 
      }
    }else{
      source("irma.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irma0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irma0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#ma
  else if (ar[1]!=0 && ma[1]==0 && AR[1]!=0 && MA[1]==0){ 
    if(is.na(exvar[1])==F)
    {
      source("irarSARX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarSARX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarSARX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irarSAR.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarSAR0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarSAR0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#arSAR
  else if (ar[1]==0 && ma[1]==0 && AR[1]!=0 && MA[1]!=0){
    if(is.na(exvar[1])==F)
    {
      source("irSARMAX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irSARMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irSARMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irSARMA.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irSARMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irSARMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#SARMA
  else if (ar[1]==0 && ma[1]==0 && AR[1]!=0 && MA[1]==0){
    if(is.na(exvar[1])==F)
    {
      source("irSARX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irSARX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irSARX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irSAR.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irSAR0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irSAR0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#SAR
  else if (ar[1]!=0 && ma[1]==0 && AR[1]!=0 && MA[1]!=0){
    if(is.na(exvar[1])==F)
    {
      source("irarSARMAX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarSARMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarSARMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irarSARMA.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarSARMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarSARMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#arSARMA
  else if (ar[1]!=0 && ma[1]!=0 && AR[1]==0 && MA[1]!=0){
    if(is.na(exvar[1])==F)
    {
      source("irarmaSMAX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarmaSMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarmaSMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irarmaSMA.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarmaSMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarmaSMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#armaSMA
  else if (ar[1]==0 && ma[1]==0 && AR[1]==0 && MA[1]!=0){
    if(is.na(exvar[1])==F)
    {
      source("irSMAX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irSMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irSMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irSMA.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irSMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irSMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#SMA
  else if (ar[1]!=0 && ma[1]!=0 && AR[1]!=0 && MA[1]==0){
    if(is.na(exvar[1])==F)
    {
      source("irarmaSARX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarmaSARX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarmaSARX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irarmaSAR.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarmaSAR0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarmaSAR0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#armaSAR
  else if (ar[1]==0 && ma[1]!=0 && AR[1]!=0 && MA[1]!=0){
    if(is.na(exvar[1])==F)
    {
      source("irmaSARMAX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irmaSARMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irmaSARMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irmaSARMA.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irmaSARMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irmaSARMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#maSARMA
  else if (ar[1]!=0 && ma[1]==0 && AR[1]==0 && MA[1]!=0){
    if(is.na(exvar[1])==F)
    {
      source("irarSMAX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarSMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarSMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irarSMA.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irarSMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irarSMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#arSMA
  else if (ar[1]==0 && ma[1]!=0 && AR[1]!=0 && MA[1]==0){
    if(is.na(exvar[1])==F)
    {
      source("irmaSARX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irmaSARX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irmaSARX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irmaSAR.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irmaSAR0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irmaSAR0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#maSAR
  else if (ar[1]==0 && ma[1]!=0 && AR[1]==0 && MA[1]!=0){
    if(is.na(exvar[1])==F)
    {
      source("irmaSMAX.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irmaSMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irmaSMAX0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,exvar=exvar,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }else{
      source("irmaSMA.R")
      IR=EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      if(IR$RMC==0){if(IR$pvalues[length(IR$coeff)]<=0.05){return(IR)}else{
        source("irmaSMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
      }else{
        source("irmaSMA0.R")
        EMV.irarma(y=y,ar=ar,ma=ma,AR=AR,MA=MA,S=S,aclag=aclag, resid=resid,steps=steps,graph=graph, check=check, print=print,link=link)
      }
    }
  }#maSMA
  else {
    warning("NO ARMA STRUCTURE INFORMED")
  }
}



