coh_tv<-function(dat1,dat2,times,norm,sigmethod="none",nrand=1000,scale.min=2,scale.max.input=NULL,sigma=1.05,f0=1)
{
  require(wsyn)
  #**error checking
  wsyn:::errcheck_times(times,"coh")
  wsyn:::errcheck_wavparam(scale.min,scale.max.input,sigma,f0,times,"coh")
  
  wasvect1<-FALSE
  if (is.matrix(dat1) && dim(dat1)[1]>1)
  {
    wsyn:::errcheck_stdat(1:dim(dat1)[2],dat1,"coh")
  }else
  {
    if (!is.matrix(dat1)){wasvect1<-TRUE}
    wsyn:::errcheck_tsdat(1:length(dat1),dat1,"coh") 
    dat1<-matrix(dat1, nrow=1, ncol=length(dat1))
  }
  wasvect2<-FALSE
  if (is.matrix(dat2) && dim(dat2)[1]>1)
  {
    wsyn:::errcheck_stdat(1:dim(dat2)[2],dat2,"coh")
  }else
  {
    if (!is.matrix(dat2)){wasvect2<-TRUE}
    wsyn:::errcheck_tsdat(1:length(dat2),dat2,"coh")
    dat2<-matrix(dat2, nrow=1, ncol=length(dat2))
  }
  if (!isTRUE(all.equal(dim(dat1),dim(dat2))))
  {
    stop("Error in coh: dimensions of dat1 and dat2 must agree") 
  }
  
  if (!(norm %in% c("none","phase","powall","powind")))
  {
    stop("Error in coh: bad value for norm")
  }
  if (!(sigmethod %in% c("none","fftsurrog1","fftsurrog2","fftsurrog12",
                         "aaftsurrog1","aaftsurrog2","aaftsurrog12")))
  {
    stop("Error in coh: bad value for sigmethod")
  }  
  
  
  #**get wavelet transforms
  h<-wsyn:::warray(dat1,times,scale.min,scale.max.input,sigma,f0)
  W1<-h$wavarray
  timescales<-h$timescales
  h<-wsyn:::warray(dat2,times,scale.min,scale.max.input,sigma,f0)
  W2<-h$wavarray
  
  #**normalize
  W1<-wsyn:::normforcoh(W1,norm)
  W2<-wsyn:::normforcoh(W2,norm)
  
  #**compute coherence
  coher<-apply(X=W1*Conj(W2),FUN=mean,MARGIN=c(2,3),na.rm=T)
  
  #**for return
  wtopt<-list(scale.min=scale.min,scale.max.input=scale.max.input,
              sigma=sigma,f0=f0)
  
  #**now do the different cases for how significance is computed
  
  #*no significance requested by user - just return
  if (sigmethod=="none")
  {
    #prepare result  
    if (wasvect1){dat1<-as.vector(dat1)}
    if (wasvect2){dat2<-as.vector(dat2)}
    result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,norm=norm,wtopt=wtopt,
                 timescales=timescales,coher=coher,signif=NA,ranks=NA,bandp=NA)
    class(result)<-c("coh","coh_tv","list")
    return(result)    
  }
  
  #figure out what kind of surrogates to use
  if (sigmethod %in% c("fftsurrog1","fftsurrog2","fftsurrog12"))
  {
    surr<-"fft"
  }else
  {
    surr<-"aaft"
  }
  
  #surrogate the specified time series and take transforms and normalize
  f<-function(x,times,scale.min,scale.max.input,sigma,f0)
  {
    return(wsyn:::warray(x,times,scale.min,scale.max.input,sigma,f0)$wavarray)
  }
  sW1<-rep(list(W1),times=nrand)
  sW2<-rep(list(W2),times=nrand)
  if (sigmethod %in% c("fftsurrog1","fftsurrog12","aaftsurrog1","aaftsurrog12"))
  {
    sdat1<-wsyn::surrog(dat1,nrand,surrtype=surr,syncpres=TRUE)
    sW1<-lapply(FUN=f,X=sdat1,times=times,scale.min=scale.min,scale.max.input=scale.max.input,sigma=sigma,f0=f0) #take transforms
    sW1<-lapply(X=sW1,FUN=wsyn:::normforcoh,norm=norm) #normalize
  }
  if (sigmethod %in% c("fftsurrog2","fftsurrog12","aaftsurrog2","aaftsurrog12"))
  {
    sdat2<-wsyn::surrog(dat2,nrand,surrtype=surr,syncpres=TRUE)
    sW2<-lapply(FUN=f,X=sdat2,times=times,scale.min=scale.min,scale.max.input=scale.max.input,sigma=sigma,f0=f0) #take transforms
    sW2<-lapply(X=sW2,FUN=wsyn:::normforcoh,norm=norm) #normalize
  }
  
  #now compute coherences
  scoher<-array(complex(real=NA,imaginary=NA), dim=c(nrand,length(times),length(timescales)))
  for (counter in 1:nrand)
  {
    scoher[counter,,]<-apply(X=sW1[[counter]]*Conj(sW2[[counter]]),FUN=mean,MARGIN=c(2,3),na.rm=T)
  }
  
  gt<-matrix(NA,nrow(coher),ncol(coher))
  for (counter1 in 1:dim(coher)[1])
  {
    for (counter2 in 1:dim(coher)[2])
    {
      gt[counter1,counter2]<-sum(Mod(scoher[,counter1,counter2])<=Mod(coher[counter1,counter2]))/nrand
    }
  }
  
  #assemble the significance results
  signif<-list(sigmethod=surr,nsurrog=nrand,scoher=scoher,gt=gt)
  
  #prepare result  
  if (wasvect1){dat1<-as.vector(dat1)}
  if (wasvect2){dat2<-as.vector(dat2)}
  result<-list(dat1=dat1,dat2=dat2,times=times,sigmethod=sigmethod,
               norm=norm,wtopt=wtopt,timescales=timescales,coher=coher,
               signif=signif,ranks=NA,bandp=NA)
  class(result)<-c("coh_tv","list")
  return(result)    
}