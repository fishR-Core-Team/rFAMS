
#THis is working with a vector of cf



# umin=0.05
# umax=1.00
# uinc=0.05  #Used to create a vector of exploitation from min to max by uinc
# minlength=355                  #Minimum length limit
# initialN=100                   #Number of new recruits entering the fishery
# linf=1000                      #Point estimate of Linf from LVB
# K=0.50                         #Point estimate of K from LVB
# t0=-0.616                      #Point estimate of t0 from LVB
# LWalpha=-5.453             #Point estimate of alpha from LW regression
# LWbeta=3.10                    #Point estimate of beta from LW regression
# Mage=10                        #Maximum age of fish in population
# Z=0.44743
#
# cfmin=0.05
# cfmax=0.60
# cfinc=0.05
# cm=0.10
# minlength=355
# initialN=100
# linf=1000
# K=0.50
# t0=-0.616
# LWalpha=-5.453
# LWbeta=3.10
# Mage=10


#Function to simulate expected yield using the Beverton-Holt Yield Per Recruit model
#' @param umin The minimum exploitation rate used to calculate yield
#' @param umax The maximum exploitation rate used to calculate yield
#' @param uinc The increment used for the range of exploitaiton rate to calculate yield
#' @param minlenth The minimum lenght limit for harvest
#' @param initialN The initial number of new recruits entering the fishery
#' @param cm optional conditional natural mortality. If blank will calculate cm using the average natural moratlity from five different models
#' @param linf Point estimate of Linf from the LVB model
#' @param k Point estimate of k from the LVB model
#' @param t0 Point estimate of t0 from the LVB model
#' @param LWalpha Point estimate of alpha from the length-weight regression
#' @param LWbeta Point estimate of beta from the length-weight regression
#' @param Mage integer of maximum age in the population
#' @param Z Point estimate of instantaneous total mortality, Z, from catch curve analysis


#Returns the following values in a list
#uvec is a vector of exploitation rates used in the YPR equation
#yiedlall is a vector of yield estimates for each exploitation rate
#Nharvall is a vector of number of fish harvested estimates for each exploitation rate
#Ndieall is a vector of number of fish that die for each exploitation rate
#wtall is a vector of average weight of harvested fish for each exploitation rate
#avglall is a vector of average TL of harvested fish for each exploitation rate
#M_dist is a vector of natural mortality for each exploitation rate. For this function there is only one M so they should all be the same
#cm_dist is a vector of conditional natural mortality for each exploitation rate. For this function there is only one cm so they should all be the same
#nt_dist is a vector of the number of recruits entering the fishery at the specified minimum length at time t for each exploitation rate. For this function there is only one cm so they should all be the same
#Fmort_dist is a vector of instantaneous fishing mortality for each exploitation rate.
#Z_dist is a vector of instantatnious total mortality for each exploitation rate.

ypr<-function(cfmin=0.05,cfmax=0.60,cfinc=0.05,cm=0.10,minlength,initialN=100,linf,K,t0,LWalpha,LWbeta,Mage){
  if (missing(minlength))
    stop("Need to specify minimum length.")
  if (missing(linf))
    stop("Need to specify Linf.")
  if (missing(K))
    stop("Need to specify K.")
  if (missing(t0))
    stop("Need to specify t0.")
  if (missing(LWalpha))
    stop("Need to specify Length-weight intercept, alpha.")
  if (missing(LWbeta))
    stop("Need to specify Length-weight slope, beta.")
  if (missing(Mage))
    stop("Need to specify a maximum age.")


  cfvec<-seq(from=cfmin,to=cfmax,by=cfinc)
  #cmvec<-seq(from=cmmin,to=cmmax,by=cminc)

  yieldall<-c()
  Nharvall<-c()
  Ndieall<-c()
  wtall<-c()
  avglall<-c()
  # linf_dist<-c()
  # K_dist<-c()
  # t0_dist<-c()
  M_dist<-c()
  cm_dist<-c()
  nt_dist<-c()
  Z_dist<-c()
  Fmort_dist<-c()
  uvect<-rep(NA,length(cfvec))


  for (j in 1:(length(cfvec))){
    yield<-c()
    NharvOut<-c()
    NdieOut<-c()
    wtOut<-c()
    avglOut<-c()
    linfOut<-c()
    KOut<-c()
    t0Out<-c()
    MOut<-c()
    MallOut<-c()
    maxmorall<-c()
    minmorall<-c()
    ntOut<-c()
    ZOut<-c()
    FmortOut<-c()



    #time of initial age used for estimates of natural mortality
    ti <- 1

    #time for fish to recruit to a minimum length limit
    TL <- minlength  #Minimum length limit

    #maximum theoretical weight derived from L-inf and weight to length regression
    #log10 transformation to linearize it
    Winf <-  10^(LWalpha + log10(linf) * LWbeta)

    #Value for incomplete beta functions and other calculations
    # t0 <- rnorm(1, mean = t0vec[1], sd = t0vec[2])

    maxage <- Mage  #maximum age
    #slope of the weight-length relation + 1
    #distribution comes from LW Bayes model
    Q <- LWbeta + 1


    Fmort <- -1 * log(-1*(cfvec[j]-1))
    M <- -1 * log(-1*(cm-1))
    Z <- Fmort+M
    S <- exp(-Z)

    uvect[j] <-(Fmort*(1-S))/Z

    #Instantaneous Rate of total mortality
    # Z <- rnorm(1, mean = Zvec[1], sd = Zvec[2])
    #S <- exp(Z)
    #rbeta(1000,5.33971,6.89622)
    # if(is.null(S)){
    #    S<-rbeta(1,a,b)
    #  }
    #  else{
    #    S<-S
    #  }
    #  Z<-log(S)

    #Instantaneous rate of natural mortality
    #
    # if(is.null(cm)){
    #   #M <- 0.30
    #   hoenig <- exp(1.46 - 1.01 * log(maxage))
    #   jensen <- 1.50 * K
    #   perterson_W <- 1.92 * (Winf^-0.25)  #converts pounds to grams
    #   quinnderiso <- -log(0.01)/maxage
    #   chenwatanabe <- (1/(maxage-ti)) *
    #     log(exp(K*maxage)-exp(K*t0))/(exp(K*ti)-exp(K*t0))
    #
    #   if (is.na(chenwatanabe)) { chenwatanabe=0
    #         } else if (chenwatanabe<0){chenwatanabe=0
    #         } else if (chenwatanabe>1){chenwatanabe=1
    #         } else {chenwatanabe=chenwatanabe}
    #
    #   if (is.na(hoenig)) {hoenig=0
    #   } else if (hoenig<0){hoenig=0
    #   } else if (hoenig>1){hoenig=1
    #   } else {hoenig=hoenig}
    #
    #   if (is.na(jensen)) {jensen=0
    #     } else if (jensen<0){jensen=0
    #     } else if (jensen>1){jensen=1
    #     } else {jensen=jensen}
    #
    #   if (is.na(perterson_W)) {perterson_W=0
    #   } else if (perterson_W<0){perterson_W=0
    #   } else if (perterson_W>1){perterson_W=1
    #   } else {perterson_W=perterson_W}
    #
    #   if (is.na(quinnderiso)) {quinnderiso=0
    #   } else if (quinnderiso<0){quinnderiso=0
    #   } else if (quinnderiso>1){quinnderiso=1
    #   } else {quinnderiso=quinnderiso}
    #
    #   maxmor<-max(c(hoenig,jensen,perterson_W,quinnderiso,chenwatanabe))
    #   minmor<-min(c(hoenig,jensen,perterson_W,quinnderiso,chenwatanabe))
    #
    #   #if (minmor<0){minmor=0}else{minmor=minmor}
    #   if (is.na(minmor)) {minmor=0
    #   } else if (minmor<0){minmor=0
    #   } else {minmor=minmor}
    #
    #   #if (maxmor>1){maxmor=1}else{maxmor=maxmor}
    #
    #   if (is.na(maxmor)) {maxmor=0
    #   } else if (maxmor<0){maxmor=0
    #   } else {maxmor=maxmor}
    #
    #   calcmor<-matrix(c(hoenig,jensen,perterson_W,quinnderiso,chenwatanabe),nrow=1,ncol=5)
    #
    #   #M<-runif(1,max=maxmor,min=minmor)
    #   M <- (hoenig+jensen+perterson_W+quinnderiso+chenwatanabe)/5
    #   maxmorall<-c(maxmorall,maxmor)
    #   minmorall<-c(minmorall,minmor)
    #   cm_calc<-1-exp(-M)
    # }else{
    #   M<- -1 * log((cm-1)*-1)
    #   maxmorall<-c(maxmorall,M)
    #   minmorall<-c(minmorall,M)
    #   cm_calc<-cm
    #   calcmor<-matrix(rep(M,5),nrow=1,ncol=5)
    # }
    # u <- uvec[j]
    #
    # #Instantaneous Rate of fishing mortality
    # Fmort <- (u*-Z)/(1-S)
    # if (Fmort<0){Fmort=0}else{Fmort=Fmort}
    # if (Fmort>1){Fmort=1}else{Fmort=Fmort}



    #Number of recruits entering the population
    N0 <- initialN

    #time in years to recruit to the fishery (tr - to)

    if(TL<linf){ tr <- (log(1-TL/linf))/-K + t0
    } else{ tr <- (log(1-TL/(TL+.1)))/-K + t0
    }

    r <- tr - t0

    #Number of recruits entering the fishery at some minimum length at time (t):
    Nt <- N0 * exp(-M * tr)

    if (Nt<0) {Nt=0
    } else if (Nt>N0){Nt=N0
    } else {Nt=Nt}

    Y <- ((Fmort*Nt*exp(Z*r) * Winf) / K) *
      (beta(Z/K, Q)  * pbeta(exp(-K*r), Z/K, Q) -
         beta(Z/K, Q)  * pbeta(exp(-K*(maxage-t0)), Z/K, Q))

    #Uses Ibeta function from zipfR pacakge - only for testing
    #Y <- ((Fmort*Nt*exp(Z*r) * Winf) / K) * (Ibeta(exp(-K*r), Z/K, Q) - Ibeta(exp(-K*(maxage-t0)), Z/K, Q))


    if (is.na(Y)==T) {Y=NA
    } else if (is.infinite(Y)==T) {Y=NA
    } else if (Y<0){Y=0
    } else {Y=Y}


    #number of fish harvested
    Nharv<-(Nt*Fmort)/Z

    if (Nharv<1){
      Nharv=0
      wt=NA
      avgl=NA
      Y=0
    } else if (Nharv>Nt) {
      Nharv=Nt
      wt <- Y/Nharv
      avgl <- 10^((log10(wt) - LWalpha)/LWbeta)
    } else {
      Nharv=Nharv
      wt <- Y/Nharv
      avgl <- 10^((log10(wt) - LWalpha)/LWbeta)
    }

    if (is.na(wt)==F)
    { if (wt<1)
    {wt=NA}
      else
      {wt=wt}
    }

    if (is.na(avgl)==F)
    { if (avgl<1)
    {avgl=NA}
      else if (avgl<minlength)
      {avgl=minlength}
      else
      {avgl=avgl}
    }


    Ndie<-(Nt*M)/Z

    if (Ndie<0)
    {Ndie=0
    } else if (Ndie>Nt){Ndie=Nt
    } else {Ndie=Ndie}



    yieldall<-c(yieldall,Y)
    Nharvall<-c(Nharvall,Nharv)
    Ndieall<-c(Ndieall,Ndie)
    wtall<-c(wtall,wt)
    avglall<-c(avglall,avgl)
    M_dist<-c(M_dist,M)
    nt_dist<-c(nt_dist,Nt)
    Fmort_dist<-c(Fmort_dist,Fmort)
    Z_dist<-c(Z_dist,Z)


  }

  AllRes<-list(uvect=uvect,yieldall=yieldall,Nharvall=Nharvall,Ndieall=Ndieall,
               wtall=wtall,avglall=avglall,
               M_dist=M_dist,
               nt_dist=nt_dist,Fmort_dist=Fmort_dist)

  return(AllRes)
}
