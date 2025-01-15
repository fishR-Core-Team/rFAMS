#' @title Simulate expected yield using the Beverton-Holt Yield-per-Recruit model for single input parameters
#'
#' @description Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model. This main function accepts only single values for minimum length limit for harvest (`minLL`) and instantaneous fishing (`F`) and natural (`M`) mortality rates. This function is generally an internal functions for \code{\link{yprBH_minLL}} and will generally not be called by the user.
#'
#' @param minLL A single numeric representing the minimum length limit for harvest in mm.
#' @param F A single numeric representing instantaneous fishing mortality.
#' @param M A single numeric representing instantaneous natural mortality.
#' @param lhparms A named list or vector that contains values for each `N0`, `Linf`, `K`, `t0`, `LWalpha`, `LWbeta`, and `maxage`. This is best constructed with \code{\link{makeLH}}.
#' @param matchRicker A logical that indicates whether the yield function should match that in Ricker (1975). Defaults to `TRUE`. The only reason to changed to `FALSE` is to try to match output from FAMS. See the "YPR_FAMSvRICKER" article.
#'
#' @details MORE Details will be filled out later
#'
#' @return A data.frame with the following calculated values:
#'
#' * `yield` is the estimated yield (in g).
#' * `Nharvest` is the number of harvested fish.
#' * `Ndie` is the number of fish that die of natural deaths.
#' * `Nt` is the number of fish at time tr (time they become harvestable size).
#' * `avgwt` is the average weight of fish harvested.
#' * `avglen` is the average length of fish harvested.
#' * `tr` is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery).
#'
#' The data.frame also contains a `notes` value which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are described in the documentation for \code{\link{yprBH_minLL}}.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @seealso \code{\link{yprBH_minLL}} for simulating yield with (possibly) multiple values of `minLL`, `F`, and `M`.
#'
#' @examples
#' # Life history parameters to be used below
#' LH <- makeLH(N0=100,maxage=15,Linf=600,K=0.30,t0=-0.6,LWalpha=-5.453,LWbeta=3.10)
#'
#' # Mortalities to be used below (single values of each)
#' morts <- makeMort(cf=0.45,cm=0.25)
#'
#' # Estimate yield for single values of minLL, cf, and cm
#' yprBH_minLL_1(minLL=355,F=morts$F,M=morts$M,lhparms=LH)
#'
#' @rdname yprBH_minLL_1
#' @export

yprBH_minLL_1 <- function(minLL,F,M,lhparms,matchRicker=TRUE) {
  # ----- Prepare inputs
  # Check minLL
  iCheckMLH(minLL)
  # ## morts and lhparams were checked in makeMort and makeLH

  # Compute Z
  Z <- F+M

  # Extract individual life history values
  N0 <- lhparms[["N0"]]
  maxage <- lhparms[["maxage"]]
  Linf <- lhparms[["Linf"]]
  K <- lhparms[["K"]]
  t0 <- lhparms[["t0"]]
  LWalpha <- lhparms[["LWalpha"]]
  LWbeta <- lhparms[["LWbeta"]]

  # prepare notes vector
  notes <- NULL

  # ---- Prep intermediate calculations needed to calculate Yield
  # Maximum theoretical weight derived from L-inf and weight to length regression
  #   log10 transformation to linearize it
  Winf <- 10^(LWalpha+log10(Linf)*LWbeta)

  # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  #   needed adjustment if minLL>Linf
  if (minLL>=Linf) {
    WARN("The set mininmum length limit of harvest (=",minLL,") is greater than\n",
         "  the asymptotic mean length (=",Linf,"). The time to recruit to the\n",
         "  fishery was adjusted. There will be very little harvest and the\n",
         "  YPR calculations may not be robust.")
    notes <- c(notes,"minLL>=Linf")
    tr <- ((log(1-minLL/(minLL+.1)))/-K)+t0
  } else tr <- ((log(1-minLL/Linf))/-K)+t0

  #   needed adjustment if tr<to (b/c r<0) b/c X in beta() (below) can not be <0
  #     and it does not make sense to recruit before length=0
  if ((tr-t0)<0) {
    WARN("The age at recruitment to the fishery (tr; =",tr," is less than t0.\n",
         "Fish can't be available to the fishery until after t0; thus tr was\n",
         "set to t0. Check your growth parameter values (Linf, K, and t0) and\n",
         "your minLL values.")
    notes <- c(notes,"tr<t0")
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
  Xi <- exp(-K*(maxage-t0))

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
  data.frame(
    yield=Y,
    Nharvest=Nharv,
    Ndie=Ndie,
    Nt= Nt,
    avgwt=avgwt,
    avglen=avglen,
    tr=tr,
    notes=paste(notes,collapse="; ")
  )
}
