#Function to simulate yield using the Beverton-Holt Yield Per recruit model
#with varying cf, cm, and minimum length


#Need to add checks for input arguments



ypr<-function(cfmin = 0.05,
              cfmax = 0.95,
              cfinc = 0.05,
              cmmin = 0.05,
              cmmax = 0.95,
              cminc = 0.05,
              lengthmin = 100,
              lengthmax = 600,
              lengtinc= 50,
              initialN=100,
              linf=2000,
              K=0.50,
              t0=-0.616,
              LWalpha=-5.453,
              LWbeta=3.10,
              Mage=15){


  #Set up cfvector
  cfvect <- seq(from=cfmin,to=cfmax,by=cfinc)
  #Set up cm vector
  cmvect <- seq(from=cmmin,to=cmmax,by=cminc)
  #Set up length vector
  MLvect <- seq(from=lengthmin,to=lengthmax,by=lengtinc)

  res_out<-list(length=length(MLvect))



  for(z in 1:length(MLvect)){
    exploitation<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    yield<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Nharvest<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Ndie<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    wt<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    avgl<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Nt<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Fmort<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Mmort<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Zmort<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    S<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))

    for(y in 1:length(cfvect)){ #Row
      for(x in 1:length(cmvect)){ #Column

        Res_1<-ypr_func(cf=cfvect[y],
                        cm=cmvect[x],
                        minlength=MLvect[z],
                        initialN=initialN,
                        linf=linf,
                        K=K,
                        t0=t0,
                        LWalpha=LWalpha,
                        LWbeta=LWbeta,
                        Mage=Mage)

        #Pull out results and add to appropriate location in matrix
        exploitation[y,x] = Res_1$exploitation
        yield[y,x] = Res_1$yield
        Nharvest[y,x] = Res_1$Nharvest
        Ndie[y,x] = Res_1$Ndie
        wt[y,x] = Res_1$wt
        avgl[y,x] = Res_1$avgl
        Nt[y,x] = Res_1$Nt
        Fmort[y,x] = Res_1$Fmort
        Mmort[y,x] = Res_1$Mmort
        Zmort[y,x] = Res_1$Zmort
        S[y,x] = Res_1$S
      }
    }
    res_out[[z]] <-list(exploitation,yield,Nharvest,Ndie,wt,avgl,Nt,Fmort,Mmort,Zmort,S,cfvect,cmvect,MLvect)
    names(res_out[[z]]) <- c("exploitation","yield","Nharvest","Ndie","wt","avgl","Nt","Fmort","Mmort","Zmort","S","cfvect","cmvect","MLvect")
  }

  #Create a list of lists. Each main list is the Minimum Length.
  #Within each list item are matrices of cf x cm
  return(res_out)

}
