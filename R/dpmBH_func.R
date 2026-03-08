#' @title Simulate yield under minimum length regulations using the dynamic pool model.
#'
#' @description An INTERNAL function used by \code{\link{dpmBH_MinLL}} to estimate yield under minimum length limit regulations using the Dynamic Pool (DPM) model with a provided minimum length limit for harvest (`minLL`), vector for conditional fishing mortality (`cf`), vector of conditional natural mortality (`cm`), vector of recruitment abundance (`rec`). This is the base function for \code{\link{dpmBH_MinLL}}, is NOT exported, and is NOT expected to be used directly by the user.
#'
#' @inheritParams dpmBH_MinLL
#'
#' @details See details in \code{\link{dpmBH_MinLL}}.
#'
#' @return A one row data.frame with the items described for the first data.frame returned by \code{\link{dpmBH_MinLL}}.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @keywords internal

dpmBH_func <- function(minLL,cf,cm,rec,lhparms,matchRicker){
  # ----- Extract individual life history values
  N0 <- rec
  tmax <- lhparms[["tmax"]]
  Linf <- lhparms[["Linf"]]
  K <- lhparms[["K"]]
  t0 <- lhparms[["t0"]]
  LWalpha <- lhparms[["LWalpha"]]
  LWbeta <- lhparms[["LWbeta"]]
  # !!!!! Note that checks fof all inputs were made in dpmBH_MinLL()

  # ----- Drop names from cf, cm, and rec vectors if they exist. This allows
  #       those matrices in dpmBH_MinLL() to be named but eliminates those names
  #       being inappropriately carried over to the results from here
  if (!is.null(names(cf))) names(cf) <- NULL
  if (!is.null(names(cm))) names(cm) <- NULL
  if (!is.null(names(rec))) names(rec) <- NULL

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


  # ---- Return data.frame with both output values and input parameters
  data.frame(
    age = age,
    length=length,
    weight=weight,
    nstart=abundvec,
    exploitation=exploitation,
    expect_nat_death=expect_nat_death,
    cf=cf,
    cm=cm,
    F = F,
    M = M,
    Z = Z,
    S=S,
    biomass= biomass,
    nharvest=N_harvest,
    ndie=N_die,
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
