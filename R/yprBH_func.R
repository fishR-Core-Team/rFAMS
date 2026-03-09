#' @title Simulate expected yield under minimum length regulations using the Beverton-Holt Yield-per-Recruit model
#'
#' @description An INTERNAL function used by \code{\link{yprBH_MinLL}} to estimate yield under minimum length limit regulations using the Beverton-Holt Yield-per-Recruit (YPR) model with one value each of `minLL`, `cf`, and `cm`. This is the base function for \code{\link{yprBH_MinLL}}, is NOT exported, and is NOT expected to be used directly by the user.
#'
#' @inheritParams yprBH_MinLL
#' @param minLL A SINGLE numeric representing the minimum length limit for harvest in mm.
#' @param cf A SINGLE numeric representing conditional fishing mortality.
#' @param cm A SINGLE numeric representing conditional natural mortality.
#'
#' @details See details in \code{\link{yprBH_MinLL}}.
#'
#' @return A one row data.frame with the items described in \code{\link{yprBH_MinLL}}.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @keywords internal

yprBH_func <- function(minLL,cf,cm,lhparms,loi,matchRicker){
  # ----- Extract individual life history values
  N0 <- lhparms[["N0"]]
  tmax <- lhparms[["tmax"]]
  Linf <- lhparms[["Linf"]]
  K <- lhparms[["K"]]
  t0 <- lhparms[["t0"]]
  LWalpha <- lhparms[["LWalpha"]]
  LWbeta <- lhparms[["LWbeta"]]
  # ----- Prepare notes vector
  notes <- NULL
  # !!!!! Note that checks fof all inputs were made in yprBH_MinLL()

  # ---- Prep intermediate calculations needed to calculate Yield
  # Maximum theoretical weight derived from L-inf and weight to length regression
  #   log10 transformation to linearize it
  Winf <- 10^(LWalpha+log10(Linf)*LWbeta)

  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  F <- -1*log(1-cf)
  M <- -1*log(1-cm)
  Z <- F+M
  # Annual survival rate (S)
  S <- exp(-Z)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation <- (1-S)*(F/Z)


  # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  #   needed adjustment if minLL>Linf
  if (minLL>=Linf) {
    WARN("The set mininmum length limit of harvest (=",minLL,") is greater than\n",
         "  the Linf (=",Linf,"). The time to recruit to the fishery (tr) was\n",
         "  adjusted. There will be very little harvest and the YPR calculations\n",
         "  may not be robust.")
    notes <- c(notes,"minLL>=Linf")
    tr <- ((log(1-minLL/(minLL+.1)))/-K)+t0
  } else tr <- ((log(1-minLL/Linf))/-K)+t0

  #   needed adjustment if tr<to (b/c r<0) b/c X in beta() (below) can not be <0
  #     and it does not make sense to recruit before length=0
  if ((tr-t0)<0) {
    WARN("The age at recruitment to the fishery (tr; =",tr,") is less than t0.\n",
         "Fish can't be available to the fishery until after t0; thus tr was\n",
         "set to t0. Check your growth parameter values (Linf, K, and t0) and\n",
         "your minLL values.")
    notes <- c(notes,"tr<t0")
  }

if(!is.null(loi[1])){
  #Get vector of time to length's of interest
  tloi <- rep(NA,length(loi))
  Nloi <- rep(NA,length(loi))

  for(x in 1:length(loi)){
    #Time to length of interest
    if(loi[x] > Linf){
      WARN("The specified length of interest (=",loi[x],") is greater than the\n",
           "Linf (=",Linf,") which produces an error. Please select a length\n",
           "of interest below Linf.")
      notes <- c(notes,paste0("loi=",loi[x],">Linf"))
    } else {

      tloi[x] <- ((log(1-loi[x]/Linf))/-K)+t0
      if(tloi[x] < tr){ #time to reach length of interest is less than time to recruit then only M applied
        Nloi[x] <- N0*exp(-M*tloi[x])
      } else { #else apply M and F
        Nloi[x] <- N0*exp(-M*tr)
        Nloi[x] <- Nloi[x]*exp(-(F+M)*(tloi[x]-tr))
      }
    }
  }

  #assign column names
  names(Nloi) <- paste0("nAt", loi)
}

  # Amount of time (years) to recruit to the fishery (r) ... defined in FAMS
  r <- tr-t0

  # Number recruiting to fishery based on time at minimum length (tr) ...
  #    FAMS equation 6:3
  Nt <- N0*exp(-M*tr)
  # Adjust Nt if less than 0 or greater than start, otherwise keep Nt as calculated
  #    not clear that this is done in FAMS
  if (Nt<0) {
    Nt <- 0
    notes <- c(notes,"Nt<0")
  } else if (Nt>N0) {
    Nt <- N0
    notes <- c(notes,"Nt>N0")
  }

  # Convenience calculations for beta function below ... per FAMS definitions
  P <- Z/K
  Q <- LWbeta+1
  X <- exp(-K*r)
  Xi <- exp(-K*(tmax-t0))

  # ---- Compute yield
  # Y is FAMS equation 6:1 ...
  #   see testing for internal iIbeta() to note how it matches other packages
  Y <- ((F*Nt*exp(Z*r)*Winf)/K)*(iIbeta(X,P,Q)-iIbeta(Xi,P,Q))
  # ... if matchRicker then Y is "corrected" to match equation 10.22 in Ricker
  if (matchRicker) Y <- Y*exp(M*t0)

  # Adjust Y to NA if infinite, to 0 if negative, otherwise keep as calculated
  if (is.infinite(Y)) {
    Y <- NA
    notes <- c(notes,"Y=Infinite")
  } else if (Y<0) {
    Y <- 0
    notes <- c(notes,"Y<0")
  }

  # ---- Other calculations made in FAMS
  # Number of fish harvested ... FAMS equation 6:4
  Nharv <- Nt*(F/Z)

  # Adjust Nharv to Nharv if Nharv is greater than Nt, otherwise keep as calcd
  #   not clear that FAMS does this
  if (Nharv<0) {
    Nharv <- 0
    notes <- c(notes,"Nharv<0")
  } else if (Nharv>Nt) {
    Nharv <- Nt
    notes <- c(notes,"Nharv>Nt")
  }

  # Number of fish that died naturally ... FAMS equation 6:5
  Ndie <- Nt*(M/Z)

  # Adjust Ndie to 0 if negative or Nt if greater than Nt, otherwise keep as calcd
  #   not clear that FAMS does this
  if (Ndie<0) {
    Ndie <- 0
    notes <- c(notes,"Ndie<0")
  } else if (Ndie>Nt) {
    Ndie <- Nt
    notes <- c(notes,"Ndie>Nt")
  }

  # Mean weight of harvested fish ... FAMS equation 6:6
  avgwt <- Y/Nharv

  # Mean length of harvest fish ... from mean weight and weight-length parameters
  avglen <- 10^((log10(avgwt) - LWalpha)/LWbeta)

  # Adjust non-NA mean lengths less than min length to min length
  if (!is.na(avglen)) if (avglen<minLL) {
    avglen <- minLL
    notes <- c(notes,"agvglen<minLL")
  }

  # ---- Return data.frame with both output values and input parameters
  tmp1 <- data.frame(
    yield=Y,
    nharvest=Nharv,
    ndie=Ndie,
    nt= Nt,
    tr=tr,
    avgwt=avgwt,
    avglen=avglen)
  tmp2 <- data.frame(
    exploitation=exploitation,
    F=F,
    M=M,
    Z=Z,
    S=S,
    cf=cf,
    cm=cm,
    minLL=minLL,
    N0=N0,
    Linf=Linf,
    K=K,
    t0=t0,
    LWalpha=LWalpha,
    LWbeta=LWbeta,
    tmax=tmax,
    notes=paste(notes,collapse="; "))

  if (!is.null(loi[1])) outdf <- cbind(tmp1,t(Nloi),tmp2)
  else outdf <- cbind(tmp1,tmp2)

  outdf
}
