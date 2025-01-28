#' @title Function to simulate expected yield using the Beverton-Holt Yield Per Recruit model for single input parameters
#'
#' @description Function to estimate yield using the Beverton-Holt YPR model. This main function accepts only single values for cf, cm, and minlength. Use the wrapper ypr() function for specifying range of cf, cm, and minlength
#'
#' @param cf A numeric representing conditional fishing mortality
#' @param cm A numeric representing conditional natural mortality
#' @param minlength A numeric representing the minimum length limit for harvest in mm
#' @param N0 A numeric representing the initial number of new recruits entering the fishery OR a vector or list that contains named values for each \code{N0}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, \code{LWbeta}, and \code{maxage}
#' @param Linf A numeric representing the point estimate of the asymptotic mean length (L-infinity) from the von Bertalanffy growth model in mm
#' @param K A numeric representing the point estimate of the Brody growth coefficient from the von Bertalanffy growth model
#' @param t0 A numeric representing the point estimate of the x-intercept (i.e., theoretical age at a mean length of 0) from the von Bertalanffy growth model
#' @param LWalpha A numeric representing the point estimate of alpha from the length-weight regression on the log10 scale.
#' @param LWbeta A numeric representing the point estimate of beta from the length-weight regression on the log10 scale.
#' @param maxage An integer representing maximum age in the population in years
#'
#' @details Details will be filled out later
#'
#' @return the following calculated and input values in a data.frame
#' \itemize{
#' \item yield is the calculated yield
#' \item exploitation is the exploitation rate
#' \item Nharvest is the number of harvested fish
#' \item Ndie is the number of fish that die of natural deaths.
#' \item Nt is the number of fish at time t (time they become harvestable size)
#' \item avgwt is the average weight of fish harvested
#' \item avglen is the average length of fish harvested
#' \item tr is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery)
#' \item Fmort is the estimated instantaneous rate of fishing mortality
#' \item Mmort is the estimated  instantaneous rate of natural mortality
#' \item Zmort is the estimated  instantaneous rate of total mortality
#' \item S is the estimated total survival
#' \item cf A numeric representing conditional fishing mortality
#' \item cm A numeric representing conditional natural mortality
#' \item minlength A numeric representing the minimum length limit for harvest in mm
#' \item N0 A numeric representing the initial number of new recruits entering the fishery
#' \item Linf A numeric representing the point estimate of Linf from the LVB model in mm
#' \item K A numeric representing the point estimate of k from the LVB model
#' \item t0 A numeric representing the point estimate of t0 from the LVB model
#' \item LWalpha A numeric representing the point estimate of alpha from the length-weight regression on the log10 scale.
#' \item LWbeta A numeric representing the point estimate of beta from the length-weight regression on the log10 scale.
#' \item maxage An integer representing of maximum age in the population in years
#' }
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' # Estimate yield with fixed parameters
#' Res_1 <- ypr_func(cf=0.45,cm=0.25,
#'                   minlength=355,
#'                   N0=100,
#'                   Linf=2000,K=0.50,t0=-0.616,
#'                   LWalpha=-5.453,LWbeta=3.10,
#'                   maxage=15)
#' Res_1
#'
#' # Same, but with named vector in N0
#' parms <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
#' Res_2 <- ypr_func(cf=0.45,cm=0.25,minlength=355,N0=parms)
#' Res_2
#'
#' # Same, but with named list in N0
#' parms <- list(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
#' Res_3 <- ypr_func(cf=0.45,cm=0.25,minlength=355,N0=parms)
#' Res_3
#'
#' @rdname ypr_function
#' @export


#This is working for harvesting IN slot only, matches using FAMS yield model but not modified version

ypr_slot_func <- function(recruitmentTL,lowerSL,upperSL,undercf,incf,abovecf,cm, #cmmin,cmmax,cminc,
                     N0,Linf=NULL,K=NULL,t0=NULL,
                     LWalpha=NULL,LWbeta=NULL,maxage=NULL){
  # ---- Check inputs
  #iCheckMLH(minlength)
  #iCheckcf(cf)
  #iCheckcm(cm)
  # if (length(N0)>1) {
  #   pnms <- c('N0','Linf','K','t0','LWalpha','LWbeta', 'maxage')
  #   if (length(N0)!=7) STOP("'N0' must contain only one value for 'N0' or 7 named\n",
  #                           "values for: ",paste(pnms,collapse=", "))
  #   if (is.null(names(N0))) STOP("'N0' must have named values for: ",
  #                                paste(pnms,collapse=", "))
  #   if (!all(names(N0) %in% pnms)) STOP("'N0' must have named values for all of: ",
  #                                       paste(pnms,collapse=", "))
  #   Linf <- N0[["Linf"]]
  #   K <- N0[["K"]]
  #   t0 <- N0[["t0"]]
  #   LWalpha <- N0[["LWalpha"]]
  #   LWbeta <- N0[["LWbeta"]]
  #   maxage <- N0[["maxage"]]
  #   N0 <- N0[["N0"]]
  # }
  # iCheckN0(N0)
  # iCheckLinf(Linf)
  # iCheckK(K)
  # iCheckt0(t0)
  # iCheckLWa(LWalpha)
  # iCheckLWb(LWbeta)
  # iCheckMaxAge(maxage)

  #cm=seq(cmmin,cmmax,cminc)

  #yield under slot is matching up
  #Max age at lower slot
  tmax_lowerSL <- ((log(1-lowerSL/Linf))/-K)+t0

  #Recruitment length for capturing harvest between when anglers will harvest a fish to lower slot
  yieldunder <- ypr_func(minlength=recruitmentTL,cf=undercf,cm=cm,
                         N0=N0,Linf=Linf,K=K,t0=t0,
                         LWalpha=LWalpha,LWbeta=LWbeta,maxage=tmax_lowerSL)

  yieldunder$Nt*exp(-yieldunder$Zmort*0.682) #This is it
  yieldunder$Nt*exp(-yieldunder$Zmort* (tmax_lowerSL-yieldunder$tr))

  #yield in slot
  #Max age at upper slot
  tmax_upperSL <- ((log(1-upperSL/Linf))/-K)+t0

  tl <- ((log(1-lowerSL/Linf))/-K)+t0
  l <- tl-yieldunder$tr
  # Number recruiting to fishery based on time at lower slot limit ...
  #    FAMS equation 6:3
  #Nl <- yieldunder$Nt*exp(-yieldunder$Zmort*l)+ yieldunder$Ndie
  Nl <- yieldunder$Nt*exp(-yieldunder$Zmort*tl)
  yieldunder

  Winf <- 10^(LWalpha+log10(Linf)*LWbeta)

  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  #Need in cf for Fmort
  Fmort <- -1*log(1-incf)
  Mmort <- -1*log(1-cm)
  Zmort <- Fmort+Mmort
  # Annual survival rate (S)
  S <- exp(-Zmort)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation <- (1-S)*(Fmort/Zmort)

  # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  #   needed adjustment if minlength<Linf
  # and amount of time (years) to recruit to the fishery (r) ... defined in FAMS
  # if (minlength<Linf) tr <- ((log(1-minlength/Linf))/-K)+t0
  # else tr <- ((log(1-minlength/(minlength+.1)))/-K)+t0
  # r <- tr-t0

  Nt = yieldunder$Nt*exp(-yieldunder$Zmort* (tmax_lowerSL-yieldunder$tr))
  P <- Zmort/K
  Q <- LWbeta+1
  X <- exp(-K*(tmax_lowerSL-t0))
  Xi <- exp(-K*(tmax_upperSL-t0))


  #This works using Number still alive from FAMS (100-NharvBelow - NdieBelow)
  Y <- ((Fmort*Nt*exp(Zmort*(tmax_lowerSL-t0))*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  ypr_func(minlength=lowerSL,cf=incf,cm=cm,
           N0=Nt,Linf=Linf,K=K,t0=t0,
           LWalpha=LWalpha,LWbeta=LWbeta,maxage=tmax_upperSL)

  #Nl <- N0*exp(-yieldunder$Zmort*tl)
  #N0 is NOT CORRECT! Need to figure out how many survive past underslot
  #The numbers were correct before - try to copy/paste all the Number harvest and die code here


  yieldin <- ypr_func(minlength=lowerSL,cf=incf,cm=cm,
                         N0=Nl,Linf=Linf,K=K,t0=t0,
                         LWalpha=LWalpha,LWbeta=LWbeta,maxage=tmax_upperSL)

  #yield over slot
  yieldover <- ypr_func(minlength=upperSL,cf=abovecf,cm=cm,
                         N0=N0,Linf=Linf,K=K,t0=t0,
                         LWalpha=LWalpha,LWbeta=LWbeta,maxage=maxage)

  #Combinde dataframe for output
  data.frame(
    TotalYield = yieldunder$yield+yieldin$yield+yieldover$yield,
    TotalNharv = yieldunder$Nharvest+yieldin$Nharvest+yieldover$Nharvest,
    TotalNdie = yieldunder$Ndie+yieldin$Ndie+yieldover$Ndie,
    yieldUnder=yieldunder$yield,
    yieldIn=yieldin$yield,
    yieldAbove=yieldover$yield,
    exploitationUnder=yieldunder$exploitation,
    exploitationIn=yieldin$exploitation,
    exploitationAbove=yieldover$exploitation,
    NharvestUnder=yieldunder$Nharvest,
    NharvestIn=yieldin$Nharvest,
    NharvestAbove=yieldover$Nharvest,
    NdieUnder=yieldunder$Ndie,
    NdieIn=yieldin$Ndie,
    NdieAbove=yieldover$Ndie,
    NtUnder=yieldunder$Nt,
    NtIn=yieldin$Nt,
    NtAbove=yieldover$Nt,
    avgwtUnder=yieldunder$avgwt,
    avgwtIn=yieldin$avgwt,
    avgwtAbove=yieldover$avgwt,
    avglenUnder=yieldunder$avglen,
    avglenIn=yieldin$avglen,
    avglenAbove=yieldover$avglen,
    trUnder=yieldunder$tr,
    trIn=yieldin$tr,
    trOver=yieldover$tr,
    FmortUnder=yieldunder$Fmort,
    FmortIn=yieldin$Fmort,
    FmortAbove=yieldover$Fmort,
    MmortUnder=yieldunder$Mmort,
    MmortIn=yieldin$Mmort,
    MmortAbove=yieldover$Mmort,
    ZmortUnder=yieldunder$Zmort,
    ZmortIn=yieldin$Zmort,
    ZmortAbove=yieldover$Zmort,
    SUnder=yieldunder$S,
    SIn=yieldin$S,
    SAbove=yieldover$S,
    undercf=yieldunder$cf,
    incf=yieldin$cf,
    overcf=yieldover$cf,
    undercm=yieldunder$cm,
    incm=yieldin$cm,
    overcm=yieldover$cm,
    recruitmentTL=recruitmentTL,
    lowerSL=lowerSL,
    upperSL=upperSL,
    N0=N0,
    Linf=Linf,
    K=K,
    t0=t0,
    LWalpha=LWalpha,
    LWbeta=LWbeta,
    maxage=maxage
  )

  #outyield<-list(yieldunder,yieldin,yieldover)

  # # ---- Prep intermediate calculations needed to calculate Yield
  # # Maximum theoretical weight derived from L-inf and weight to length regression
  # #   log10 transformation to linearize it
  # Winf <- 10^(LWalpha+log10(Linf)*LWbeta)
  #
  # # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  # FmortIn <- -1*log(1-incf)
  #
  # Mmort <- -1*log(1-cm)
  #
  # ZmortIn <- FmortIn+Mmort
  #
  # # Annual survival rate (S)
  # SIn <- exp(-ZmortIn)
  # # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  #
  # exploitationIn <- (1-SIn)*(FmortIn/ZmortIn)
  #
  # # Number of fish entering fishery at recruitment size############
  # # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  # # and amount of time (years) to recruit to the fishery (r) ... defined in FAMS
  # tr <- ((log(1-lowerSL/Linf))/-K)+t0
  # r <- tr-t0
  # # Number recruiting to size that anglers will harvest. Harvestable with protective slot.
  # #    FAMS equation 6:3
  # Nt <- N0*exp(-Mmort*tr)
  # ############
  #
  # #Age leaving slot
  # tmax <- ((log(1-upperSL/Linf))/-K)+t0
  #
  # # # Number of fish entering slot############
  # # # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  # # # and amount of time (years) to recruit to the fishery (r) ... defined in FAMS
  # # tl <- ((log(1-lowerSL/Linf))/-K)+t0
  # # l <- tl-tr
  # #
  # # # Number recruiting to fishery based on time at lower slot limit ...
  # # #    FAMS equation 6:3
  # # Nl <- Nt*exp(-ZmortUnder*l)
  # # ############
  # #
  # #
  # # # Number of fish leaving slot############
  # # # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  # # # and amount of time (years) to recruit to the fishery (r) ... defined in FAMS
  # # tu <- ((log(1-upperSL/Linf))/-K)+t0
  # # u <- tu-tl
  # #
  # # # Number recruiting to beyond the slot limit ...
  # # #    FAMS equation 6:3
  # # Nu <- Nl*exp(-ZmortIn*u)
  # # ############
  #
  # # Convenience calculations for beta function below ... per FAMS definitions
  # Pin <- ZmortIn/K
  #
  # Q <- LWbeta+1
  #
  # Xin <- exp(-K*r)
  #
  # Xi <- exp(-K*(tmax-t0))
  #
  # # ---- Compute yield
  # #Uses Ibeta function from zipfR pacakge - only for testing
  # #Y <- ((Fmort*Nt*exp(Zmort*r)*Winf)/K)*(Ibeta(exp(-K*r),Zmort/K,Q)-Ibeta(exp(-K*(maxage-t0)),Zmort/K,Q))
  #
  # # FAMS equation 6:1
  # #Yield Under Slot
  # # YUnder <- ((FmortUnder*(Nt)*exp(ZmortUnder*r)*Winf)/K)*
  # #   (beta(Punder,Q)*stats::pbeta(Xunder,Punder,Q)-beta(Punder,Q)*stats::pbeta(Xi,Punder,Q))
  #
  #
  # #Yield In Slot
  # YIn <- ((FmortIn*Nt*exp(ZmortIn*(r))*exp(Mmort*t0)*Winf)/K)*
  #   (beta(Pin,Q)*stats::pbeta(Xin,Pin,Q)-beta(Pin,Q)*stats::pbeta(Xi,Pin,Q))
  #
  #
  #
  # # Adjust Y to NA if NA or infinite, to 0 if negative, otherwise keep as calculated
  # if (is.na(YIn) || is.infinite(YIn)) YIn <- NA
  # else if (YIn<0) YIn <- 0
  #
  #
  # # ---- Other calculations made in FAMS
  # # Number of fish harvested ... FAMS equation 6:4
  # NharvIn <- (Nt-N0)*(FmortIn/ZmortIn)
  #
  # # Adjust Nharv to Nharv if Nharv is greater than Nt, otherwise keep as calcd
  # #   not clear that FAMS does this
  # if (NharvIn>Nt) NharvIn <- Nt
  #
  # # Number of fish that died naturally ... FAMS equation 6:5
  # #NdieUnder <- (Nt-Nl)*(Mmort/ZmortUnder)
  # NdieIn <- (Nt-N0)*(Mmort/ZmortIn)
  # #NdieAbove <- Nu*(Mmort/ZmortAbove)
  #
  # # Adjust Ndie to 0 if negative or Nt if greater than Nt, otherwise keep as calcd
  # #   not clear that FAMS does this
  # # if (NdieUnder<0) NdieUnder <- 0
  # # else if (NdieUnder>Nt) NdieUnder <- Nt
  #
  # if (NdieIn<0) NdieIn <- 0
  # else if (NdieIn>Nr) NdieIn <- Nr
  #
  # # if (NdieAbove<0) NdieAbove <- 0
  # # else if (NdieAbove>Nu) NdieAbove <- Nu
  #
  # # Mean weight of harvested fish ... FAMS equation 6:6
  # #avgwtUnder <- YUnder/NharvUnder
  # avgwtIn <- YIn/NharvIn
  # #avgwtAbove <- YAbove/NharvAbove
  #
  # # Mean length of harvest fish ... from mean weight and weight-length parameters
  # #avglenUnder <- 10^((log10(avgwtUnder) - LWalpha)/LWbeta)
  # avglenIn <- 10^((log10(avgwtIn) - LWalpha)/LWbeta)
  # #avglenAbove <- 10^((log10(avgwtAbove) - LWalpha)/LWbeta)
  #
  # # # Adjust non-NA mean lengths less than min length to min length
  # #Removing - I don't think this will make sense if we change to slot length limits
  # # if (!is.na(avglenUnder)) if (avglenUnder<minlength) avglenUnder <- minlength
  # # if (!is.na(avglenIn)) if (avglenIn<minlength) avglenIn <- minlength
  # # if (!is.na(avglenAbove)) if (avglen<minlength) avglenAbove <- minlength
  #
  # # ---- Return data.frame with both output values and input parameters
  # data.frame(
  #   #TotalYield = YUnder+YIn+YAbove,
  #   #TotalNharv = NharvUnder+NharvIn+NharvAbove,
  #   #TotalNdie = NdieUnder+NdieIn+NdieAbove,
  #   #yieldUnder=YUnder,
  #   yieldIn=YIn,
  #   #yieldAbove=YAbove,
  #   #exploitationUnder=exploitationUnder,
  #   exploitationIn=exploitationIn,
  #   #exploitationAbove=exploitationAbove,
  #   #NharvestUnder=NharvUnder,
  #   NharvestIn=NharvIn,
  #   #NharvestAbove=NharvAbove,
  #   #NdieUnder=NdieUnder,
  #   NdieIn=NdieIn,
  #   #NdieAbove=NdieAbove,
  #   Nt=Nt,
  #   #Nl=Nl,
  #   #Nu=Nu,
  #   #avgwtUnder=avgwtUnder,
  #   avgwtIn=avgwtIn,
  #   #avgwtAbove=avgwtAbove,
  #   #avglenUnder=avglenUnder,
  #   avglenIn=avglenIn,
  #   #avglenAbove=avglenAbove,
  #   tr=tr,
  #   #tl=tl,
  #   #tu=tu,
  #   #FmortUnder=FmortUnder,
  #   FmortIn=FmortIn,
  #   #FmortAbove=FmortAbove,
  #   Mmort=Mmort,
  #   #ZmortUnder=ZmortUnder,
  #   ZmortIn=ZmortIn,
  #   #ZmortAbove=ZmortAbove,
  #   #SUnder=SUnder,
  #   SIn=SIn,
  #   #SAbove=SAbove,
  #   #undercf=undercf,
  #   incf=incf,
  #   #overcf=overcf,
  #   cm=cm,
  #   #recruitmentTL=recruitmentTL,
  #   #lowerSL=lowerSL,
  #   #upperSL=upperSL,
  #   N0=N0,
  #   Linf=Linf,
  #   K=K,
  #   t0=t0,
  #   LWalpha=LWalpha,
  #   LWbeta=LWbeta,
  #   maxage=maxage
  # )
}
