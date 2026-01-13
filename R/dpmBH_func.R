#' @title Simulate yield using the dynamic pool model.
#'
#' @description Estimate yield-at-age using the Beverton-Holt Yield-per-Recruit (YPR) model for a single year-class. This main function accepts  a minimum length limit for harvest (\code{minLL}), a vector for conditional fishing mortality (\code{cf}), a vector of conditional natural mortality (\code{cm}), a vector of recruitment abundance (\code{rec}), and life history parameters (\code{lhparams}).
#'
#' @param minLL A single numeric representing the minimum length limit for harvest in mm.
#' @param cf A matrix of conditional fishing mortality where each row represents a year and each column represents age. Ages are age-0 through maximum age.
#' @param cm A matrix of conditional natural mortality where each row represents a year and each column represents age. Ages are age-0 through maximum age.
#' @param rec A single numeric representing initial recruitment abundance.
#' @param lhparms A named vector or list that contains values for each `N0`, `tmax`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See \code{\link{makeLH}} for definitions of these life history parameters. Also see details.
#' @param matchRicker A logical that indicates whether the yield function should match that in Ricker (). Defaults to \code{TRUE}. The only reason to changed to \code{FALSE} is to try to match output from FAMS. See the "YPR_FAMSvRICKER" article.
#'
#' @details Details will be filled out later
#'
#' @return A data.frame with the following calculated values:
#' \itemize{
#' \item \code{age} is the age of the year class
#' \item \code{length} is the mean length at age calculated using the von Bertalanffy growth model and provided parameters
#' \item \code{weight} is the mean weight at age calculated using the log10 length-weight regression using the provided parameters
#' \item \code{N_start} is the number of individuals at age at the start of the year.
#' \item \code{exploitation} is the exploitation rate.
#' \item \code{expect_nat_death} is the expectation of natural death.
#' \item \code{cf} is the conditional fishing mortality at age.
#' \item \code{cm} is the conditional natural mortality at age
#' \item \code{F} is the instantaneous rate of fishing mortality.
#' \item \code{M} is the instantaneous rate of natural mortality.
#' \item \code{Z} is the instantaneous rate of total mortality.
#' \item \code{S} is the (total) annual rate of survival
#' \item \code{tr} is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery).
#' \item \code{Nt} is the number of fish at time tr (time they become harvestable size).
#' \item \code{biomass} is the total biomass at age (g)
#' \item \code{N_harvest} is the total number harvested at age
#' \item \code{N_die} is the total number that die at age
#' \item \code{yield} is the estimated yield (in g).
#' }
#'
#' For convenience the data.frame also contains the model input values (\code{minLL}, \code{N0}, \code{N0}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, \code{LWbeta}, and \code{tmax}).
#'
#' The data.frame also contains a \code{notes} value which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are as follows:
#'
#' \itemize{
#' \item \code{minLL>=Linf}: The minimum length limit (minLL) being explored was greater than the given asymptotic mean length (Linf). For the purpose (only) of computing the time at recruitment to the fishery (tr) the Linf was set to minLL+0.1.
#' \item \code{tr<t0}: The age at recruitment to the fishery (tr) was less than the hypothetical time when the mean length is zero (t0). The fish can't recruit to the fishery prior to having length 0 so tr was set to t0. This also assures that the time it takes to recruit to the fishery is greater than 0.
# \item \code{Nt<0}: The number of fish recruiting to the fishery was less than 0. There cannot be negative fish, so Nt was then set to 0.
# \item \code{Nt>N0}: The number of fish recruiting to the fishery was more than the number of fish recruited to the populations. Fish cannot be added to the cohort, so Nt was set to N0.
#' \item \code{Y=Infinite}: The calculated yield (Y) was inifinity, which is impossible and suggests some other propblem. Yield was set to NA.
#' \item \code{Y<0}: The calculated yield (Y) was negative, which is impossible. Yield was set to 0.
#' \item \code{Nharv<0}: The calculated number of fish harvested (Nharv) was negative, which is not possible. Number harvested was set to 0.
# #\item \code{Nharv>Nt}: The calculated number of fish harvested (Nharv) was greater than the number of fish recruiting to the fishery, which is impossible. The number harvested was set to the number recruiting to the fishery.
#' \item \code{Ndie<0}: The calculated number of fish recruiting to the fishery that died naturally (Ndie) was negative, which is not possible. Number that died was set to 0.
# #\item \code{Ndie>Nt}: The calculated number of fish recruiting to the fishery that died naturally (Ndie) was greater than the number of fish recruiting to the fishery, which is impossible. The number that died was set to the number recruiting to the fishery.
#' \item \code{agvglen<minLL}: The average length of harvested fish was less than the given minimum length limit being explored, which is not possible (with only legal harvest). The average length was set to the minimum length limit.
#' }
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @references
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada. Was (is?) from \url{https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/1485.pdf}.
#'
#' Slipke, J.W., and M.J. Maceina. 2014. Fishery analysis and modeling simulator. v1.64. American Fisheries Society, Bethesda, MD.
#'
#' @examples
#' lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,LWalpha=-5.2147,LWbeta=3.153)
#'
#' # simulate yield from a single year-class
#' cm <- rep(0.18,(lhparms$tmax+1))
#' cf <- c(rep(0,3), rep(0.33,(lhparms$tmax+1) - 3))
#'
#' Res_1 <- dpmBH_func(minLL=400,cm=cm,cf=cf,rec=1000,lhparms=lhparms,matchRicker=FALSE)
#'
#' Res_1
#'
#' @rdname dpmBH_func
#' @export

dpmBH_func <- function(minLL,cf,cm,rec,lhparms,matchRicker=FALSE){
  # ---- Check inputs
  #inputs checked in dpmBH() function

  # Extract individual life history values
  N0 <- rec
  tmax <- lhparms[["tmax"]]
  Linf <- lhparms[["Linf"]]
  K <- lhparms[["K"]]
  t0 <- lhparms[["t0"]]
  LWalpha <- lhparms[["LWalpha"]]
  LWbeta <- lhparms[["LWbeta"]]

  # prepare vectors for holding results
  notes <- NULL
  age <- c(rep(0,tmax+1))
  length <- c(rep(0,tmax+1))
  weight <- c(rep(0,tmax+1))
  N_start <- c(rep(0,tmax+1))
  exploitation <- c(rep(0,tmax+1))
  expect_nat_death  <- c(rep(0,tmax+1))
  F <- c(rep(0,tmax+1))
  M <- c(rep(0,tmax+1))
  Z <- c(rep(0,tmax+1))
  S <- c(rep(0,tmax+1))
  biomass <-c(rep(0,tmax+1))
  abundvec <- c(rec,rep(0,tmax))
  N_harvest <- c(rep(0,tmax+1))
  N_die <- c(rep(0,tmax+1))
  yield <- c(rep(0,tmax+1))

  # ---- Prep intermediate calculations needed to calculate Yield
  # Maximum theoretical weight derived from L-inf and weight to length regression
  #   log10 transformation to linearize it
  Winf <- 10^(LWalpha+log10(Linf)*LWbeta)
  length <- Linf * (1- exp(-K * (seq(0,tmax) -t0)))

  for(x in 1:(tmax+1)){

  #Check length at start of year, if less than minLL then F = 0 (regardless of input), exploitation = 0, and yield = 0.
  if(length[x] < minLL && length[x+1]<minLL){
      F[x] <- 0
      M[x] <- -1*log(1-cm[x])
      Z[x] <- F[x]+M[x]
      # Annual survival rate (S)
      S[x] <- exp(-Z[x])
      exploitation[x] <-0
      # Expectation of natural death (u) ... rearrange of FAMS equation 4:14
      if(M[x]==0){
        expect_nat_death[x] <-0
      }else{
        expect_nat_death[x] <- (1-S[x])*(M[x]/Z[x])
      }
      yield[x] <- 0
      N_harvest[x] <- 0
      N_die[x] <- abundvec[x] - (abundvec[x]*exp(-M[x])) #number that die naturally during year fish reach harvestable size
  }else{
    # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
    F[x] <- -1*log(1-cf[x])
    M[x] <- -1*log(1-cm[x])
    Z[x] <- F[x]+M[x]
    # print(F[x])
    # print(M[x])
    # print(cf[x])
    # print(cm[x])
    # Annual survival rate (S)
    S[x] <- exp(-Z[x])
    # Exploitation rate (u) ... rearrange of FAMS equation 4:14
    if(F[x]==0){
      exploitation[x] <-0
    }else{
      exploitation[x] <- (1-S[x])*(F[x]/Z[x])
    }
    # Expectation of natural death (u) ... rearrange of FAMS equation 4:14
    if(M[x]==0){
      expect_nat_death[x] <-0
    }else{
      expect_nat_death[x] <- (1-S[x])*(M[x]/Z[x])
    }

    # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
    #   needed adjustment if minlength<Linf
    # and amount of time (years) to recruit to the fishery (r) ... defined in FAMS
    if (minLL<Linf) {
      tr <- ((log(1-minLL/Linf))/-K)+t0
    }else {
      tr <- ((log(1-minLL/(minLL+.1)))/-K)+t0}

    r = tr - floor(tr) #Time to reach fishery, partial year

    Nr <- abundvec[x]

    #remove fish lost to natural mortality up to time r
    if(x == (floor(tr)+1) && r > 0){
      Nr <- Nr * exp(-M[x] * (r))
      age_enter_fishery <- tr
    }else if (x>floor(tr)){
      age_enter_fishery <- x -1  #This doesn't make sense - works only
    }else{
      age_enter_fishery <- NA
    }

    # Adjust Nr if less than 0 or greater than start, otherwise keep Nr as calculated
    #    not clear that this is done in FAMS
    if (Nr<0) {
      Nr <- 0
    }else if (Nr>N0) {
      Nr <- N0}

    #Calculations for YPR
    P <- Z[x]/K
    Q <- LWbeta+1
    X <- exp(-K*(age_enter_fishery-t0)) #age at entering the fishery - t0
    Xi <- exp(-K*(x-t0)) #max age in fishery - t0

    # FAMS equation 6:1
      yield[x] <- (((F[x])*Nr*exp(Z[x]*(age_enter_fishery-t0))*Winf)/K)*
        (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

     if(is.nan(yield[x])){yield[x] <- 0}

      #... if matchRicker then yield is "corrected" to match equation 10.22 in Ricker
      if (matchRicker) yield[x] <- yield[x]*exp(M[x]*t0)

    if((x-1)==floor(tr) && r >0){
      #Remove fish from natural mortality first then fishing
      removed_under <- abundvec[x]- (abundvec[x] * exp(-M[x] * r))
      remain_for_harvest <-(abundvec[x]-removed_under)
      # N_harvest[x] <- remain_for_harvest - (remain_for_harvest *exp(-F[x]* (x-tr))) #1-r is simply number of years in the fishery
      # remain_for_harvest <- remain_for_harvest-N_harvest[x]
      # N_die[x]<- remain_for_harvest - (remain_for_harvest*exp(-M[x]*(x-tr))) + removed_under
      #Calculate remaining loss to harvest and natural deaths
      if(F[x] == 0){
        N_harvest[x] = 0
      }else{
        N_harvest[x] <- (remain_for_harvest - (remain_for_harvest*exp(-Z[x]*(x-tr)))) * ((F[x]*(x-tr))/(Z[x]*(x-tr)))
      }

      if(M[x] == 0 & F[x] == 0){
        N_die[x] = 0
      }else{
        N_die[x] <- (remain_for_harvest - (remain_for_harvest*exp(-Z[x]*(x-tr)))) * ((M[x]*(x-tr))/(Z[x]*(x-tr))) +removed_under
      }
    }else{
      if(x>tr){
      N_harvest[x] <- (abundvec[x] - (abundvec[x]*exp(-Z[x]))) * (F[x]/Z[x])
      }else{
      N_harvest[x] <-0
      }
      N_die[x] <- (abundvec[x] - (abundvec[x]*exp(-Z[x]))) * (M[x]/Z[x])
    }

    # if(x == (floor(tr)+1) && r > 0){
    #   #When age at entering fishery is between x and x+1
    #   N_harvest[x] <- (Nr - (Nr*exp(-Z[x]* (1-r)))) * (F[x]/Z[x]) #1-r is simply number of years in the fishery
    #   N_die[x] <- (Nr - (Nr*exp(-Z[x]* (1-r)))) * (M[x]/Z[x])
    # }else{
    #   #When age at entering fishery is x
    #   N_harvest[x] <- (Nr - (Nr*exp(-Z[x]* (1)))) * (F[x]/Z[x]) #1-r is simply number of years in the fishery
    #   N_die[x] <- (Nr - (Nr*exp(-Z[x]* (1)))) * (M[x]/Z[x])
    # }

  }

    if(x<(tmax+1)){
      abundvec[x+1] <- abundvec[x] - N_harvest[x] - N_die[x]

      if(is.nan(abundvec[x+1])) { abundvec[x+1]=0}
      if(is.na(abundvec[x+1])) { abundvec[x+1]=0}
    }

    if(length[x]>0){
      weight[x] <- (10^(LWalpha+log10(length[x])*LWbeta))
      biomass[x] <- weight[x] * abundvec[x]
    }else{
      length[x] <- 0
      weight[x] <- 0
      biomass[x] <- 0
    }

    N_start<-abundvec[x]
    age[x] <- x-1
}

  #Calculate number of fish that reach harvestable size
  #Nt = N0
  # for(y in 1:(floor(tr))){
  #   Nt = Nt *exp(-M[y])
  # }
  # Nt = Nt * exp(-M[floor(tr)] * r)
  # Nt = N0 - Nt
  #
  # for(y in 1:((floor(tr))-1)){
  #   Nt <- Nt - N_die[y] #Number of fish that reach harvestable size
  # }
  # Nt <- Nt - N_die[floor(tr)]

  # ---- Return data.frame with both output values and input parameters
  data.frame(
    age = age,
    length=length,
    weight=weight,
    N_start=abundvec,
    exploitation=exploitation,
    expect_nat_death=expect_nat_death,
    cf=cf,
    cm=cm,
    F = F,
    M = M,
    Z = Z,
    S=S,
    #tr=rep(tr,length(age)),
    #Nt=rep(Nt,length(age)),
    biomass= biomass,
    N_harvest=N_harvest,
    N_die=N_die,
    yield=yield,
    minLL=minLL,
    N0=N0,
    Linf=Linf,
    K=K,
    t0=t0,
    LWalpha=LWalpha,
    LWbeta=LWbeta,
    tmax=tmax,
    notes=paste(notes,collapse="; ")
  )
}
