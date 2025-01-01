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

ypr_func <- function(minlength,cf,cm,
                     N0,Linf=NULL,K=NULL,t0=NULL,
                     LWalpha=NULL,LWbeta=NULL,maxage=NULL){
  # ---- Check inputs
  iCheckMLH(minlength)
  iCheckcf(cf)
  iCheckcm(cm)
  if (length(N0)>1) {
    pnms <- c('N0','Linf','K','t0','LWalpha','LWbeta', 'maxage')
    if (length(N0)!=7) STOP("'N0' must contain only one value for 'N0' or 7 named\n",
                            "values for: ",paste(pnms,collapse=", "))
    if (is.null(names(N0))) STOP("'N0' must have named values for: ",
                                 paste(pnms,collapse=", "))
    if (!all(names(N0) %in% pnms)) STOP("'N0' must have named values for all of: ",
                                        paste(pnms,collapse=", "))
    Linf <- N0[["Linf"]]
    K <- N0[["K"]]
    t0 <- N0[["t0"]]
    LWalpha <- N0[["LWalpha"]]
    LWbeta <- N0[["LWbeta"]]
    maxage <- N0[["maxage"]]
    N0 <- N0[["N0"]]
  }
  iCheckN0(N0)
  iCheckLinf(Linf)
  iCheckK(K)
  iCheckt0(t0)
  iCheckLWa(LWalpha)
  iCheckLWb(LWbeta)
  iCheckMaxAge(maxage)

  # ---- Prep intermediate calculations needed to calculate Yield
  # Maximum theoretical weight derived from L-inf and weight to length regression
  #   log10 transformation to linearize it
  Winf <- 10^(LWalpha+log10(Linf)*LWbeta)

  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  Fmort <- -1*log(1-cf)
  Mmort <- -1*log(1-cm)
  Zmort <- Fmort+Mmort
  # Annual survival rate (S)
  S <- exp(-Zmort)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation <- (1-S)*(Fmort/Zmort)

  # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  #   needed adjustment if minlength<Linf
  # and amount of time (years) to recruit to the fishery (r) ... defined in FAMS
  if (minlength<Linf) tr <- ((log(1-minlength/Linf))/-K)+t0
    else tr <- ((log(1-minlength/(minlength+.1)))/-K)+t0
  r <- tr-t0

  # Number recruiting to fishery based on time at minimum length (tr) ...
  #    FAMS equation 6:3
  Nt <- N0*exp(-Mmort*tr)
  # Adjust Nt if less than 0 or greater than start, otherwise keep Nt as calculated
  #    not clear that this is done in FAMS
  if (Nt<0) Nt <- 0
    else if (Nt>N0) Nt <- N0

  # Convenience calculations for beta function below ... per FAMS definitions
  P <- Zmort/K
  Q <- LWbeta+1
  X <- exp(-K*r)
  Xi <- exp(-K*(maxage-t0))

  # ---- Compute yield
  #Uses Ibeta function from zipfR pacakge - only for testing
  #Y <- ((Fmort*Nt*exp(Zmort*r)*Winf)/K)*(Ibeta(exp(-K*r),Zmort/K,Q)-Ibeta(exp(-K*(maxage-t0)),Zmort/K,Q))

  # FAMS equation 6:1
  Y <- ((Fmort*Nt*exp(Zmort*r)*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # Adjust Y to NA if NA or infinite, to 0 if negative, otherwise keep as caculated
  if (is.na(Y) || is.infinite(Y)) Y <- NA
    else if (Y<0) Y <- 0

  # ---- Other calculations made in FAMS
  # Number of fish harvested ... FAMS equation 6:4
  Nharv <- Nt*(Fmort/Zmort)

  # Adjust Nharv to Nharv if Nharv is greater than Nt, otherwise keep as calcd
  #   not clear that FAMS does this
  if (Nharv>Nt) Nharv <- Nt

  # Number of fish that died naturally ... FAMS equation 6:5
  Ndie <- Nt*(Mmort/Zmort)

  # Adjust Ndie to 0 if negative or Nt if greater than Nt, otherwise keep as calcd
  #   not clear that FAMS does this
  if (Ndie<0) Ndie <- 0
    else if (Ndie>Nt) Ndie <- Nt

  # Mean weight of harvested fish ... FAMS equation 6:6
  avgwt <- Y/Nharv

  # Mean length of harvest fish ... from mean weight and weight-length parameters
  avglen <- 10^((log10(avgwt) - LWalpha)/LWbeta)

  # Adjust non-NA mean lengths less than min length to min length
  if (!is.na(avglen)) if (avglen<minlength) avglen <- minlength

  # ---- Return data.frame with both output values and input parameters
  data.frame(
    yield=Y,
    exploitation=exploitation,
    Nharvest=Nharv,
    Ndie=Ndie,
    Nt= Nt,
    avgwt=avgwt,
    avglen=avglen,
    tr=tr,
    Fmort=Fmort,
    Mmort=Mmort,
    Zmort=Zmort,
    S=S,
    cf=cf,
    cm=cm,
    minlength=minlength,
    N0=N0,
    Linf=Linf,
    K=K,
    t0=t0,
    LWalpha=LWalpha,
    LWbeta=LWbeta,
    maxage=maxage
  )
}
