#' @title Function to simulate expected yield using the Beverton-Holt Yield Per Recruit model for single input parameters
#'
#' @description Function to estimate yield using the Beverton-Holt YPR model. This main function accepts only single values for cf, cm, and minlength. Use the wrapper ypr() function for specifying range of cf, cm, and minlength
#'
#' @param cf A numeric representing conditional fishing mortality
#' @param cm A numeric representing conditional natural mortality
#' @param minlength A numeric representing the minimum length limit for harvest in mm
#' @param N0 A numeric representing the initial number of new recruits entering the fishery
#' @param linf A numeric representing the point estimate of Linf from the LVB model in mm
#' @param K A numeric representing the point estimate of k from the LVB model
#' @param t0 A numeric representing the point estimate of t0 from the LVB model
#' @param LWalpha A numeric representing the point estimate of alpha from the length-weight regression on the log10 scale.
#' @param LWbeta A numeric representing the point estimate of beta from the length-weight regression on the log10 scale.
#' @param maxage An integer representing maximum age in the population in years
#'
#' @details Details will be filled out later
#'
#' @return the following calculated and input values in a data.frame
#' \itemize{
#' \item exploitation is the exploitation rate
#' \item yield is the calculated yield
#' \item Nharvest is the number of harvested fish
#' \item Ndie is the number of fish that die of natural deaths.
#' \item wt is the average weight of fish harvested
#' \item avgl is the average length of fish harvested
#' \item Nt is the number of fish at time t (time they become harvestable size)
#' \item tr is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery)
#' \item Fmort is the estimated instantaneous rate of fishing mortality
#' \item Mmort is the estimated  instantaneous rate of natural mortality
#' \item Zmort is the estimated  instantaneous rate of total mortality
#' \item S is the estimated total survival
#' \item cf A numeric representing conditional fishing mortality
#' \item cm A numeric representing conditional natural mortality
#' \item minlength A numeric representing the minimum length limit for harvest in mm
#' \item N0 A numeric representing the initial number of new recruits entering the fishery
#' \item linf A numeric representing the point estimate of Linf from the LVB model in mm
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
#' Res_1 <- ypr_func(cf = 0.45,
#'                   cm = 0.25,
#'                   minlength = 355,
#'                   N0 = 100,
#'                   linf = 2000,
#'                   K = 0.50,
#'                   t0 = -0.616,
#'                   LWalpha = -5.453,
#'                   LWbeta = 3.10,
#'                   maxage = 15)
#' Res_1
#'
#' @rdname ypr_function
#' @export

ypr_func <- function(minlength,cf,cm,N0,linf,K,t0,LWalpha,LWbeta,maxage){
  # Check inputs
  iCheckcf(cf)
  iCheckcm(cm)
  iCheckMLH(minlength)
  iCheckN0(N0)
  iCheckLinf(linf)
  iCheckK(K)
  iCheckt0(t0)
  iCheckLWa(LWalpha)
  iCheckLWb(LWbeta)
  iCheckMaxAge(maxage)

  # Maximum theoretical weight derived from L-inf and weight to length regression
  # log10 transformation to linearize it
  Winf <- 10^(LWalpha+log10(linf)*LWbeta)

  # Convert to instantaneous rates
  Fmort <- -1*log(-1*(cf-1))
  Mmort <- -1*log(-1*(cm-1))
  Zmort <- Fmort+Mmort
  S <- exp(-Zmort)

  # Compute exploitation rate
  exploitation <- (Fmort*(1-S))/Zmort

  # Find time in years to recruit to the fishery (tr - to)
  if (minlength<linf) {
    tr <- ((log(1-minlength/linf))/-K)+t0
  } else {
    tr <- ((log(1-minlength/(minlength+.1)))/-K)+t0
  }

  r <- tr-t0  # DHO NOTE ... IS THIS USED EVER???? - used in yield equation

  # Number of recruits entering the fishery at some minimum length at time (t):
  Nt <- N0*exp(-Mmort*tr)
  # Adjust Nt if less than 0 or greater than start, otherwise keep Nt as calculated
  if (Nt<0) {
    Nt <- 0
  } else if (Nt>N0) {
    Nt <- N0
  } else {
    Nt <- Nt
  }

  # Slope of the weight-length relation + 1 (for beta function below)
  Q <- LWbeta+1

  # Compute yield
  Y <- ((Fmort*Nt*exp(Zmort*r)*Winf)/K)*
    (beta(Zmort/K,Q)*stats::pbeta(exp(-K*r),Zmort/K,Q)-
       beta(Zmort/K,Q)*stats::pbeta(exp(-K*(maxage-t0)),Zmort/K,Q))

  #Uses Ibeta function from zipfR pacakge - only for testing
  #Y <- ((Fmort*Nt*exp(Zmort*r)*Winf)/K)*(Ibeta(exp(-K*r),Zmort/K,Q)-Ibeta(exp(-K*(maxage-t0)),Zmort/K,Q))

  # Adjust Y if less than 0 (to 0) or infinite (to NA) or keep as is
  if (is.na(Y)) {
    Y <- NA
  } else if (is.infinite(Y)) {
    Y <- NA
  } else if (Y<0) {
    Y <- 0
  } else {
    Y <- Y
  }

  # Number of fish harvested
  Nharv <- (Nt*Fmort)/Zmort

  # Adjust Nharv (and wt and avgl) if Nharv is greater than Nt
  # If Nharvt is good (last else) compute wt and avgl
  if (Nharv>Nt) {
    Nharv <- Nt
    wt <- Y/Nharv
    avgl <- 10^((log10(wt) - LWalpha)/LWbeta)
  } else {
    Nharv <- Nharv
    wt <- Y/Nharv
    avgl <- 10^((log10(wt) - LWalpha)/LWbeta)
  }

  # If avgl is not NA but <minlength then set
  #   to minlength, otherwise keep as is
  if (!is.na(avgl)) {
      if (avgl<minlength) {
      avgl <- minlength
    } else {
      avgl <- avgl
    }
  }

  # Number of fish that died natually
  Ndie <- (Nt*Mmort)/Zmort

  # Adjust Ndie if <0 (to 0) or more then Nt (to Nt), otherwise keep as is
  if (Ndie<0) {
    Ndie <- 0
  } else if (Ndie>Nt) {
    Ndie <- Nt
  } else {
    Ndie <- Ndie
  }

  # Create data.frame to store and return output with input parameters
  Res <- data.frame(exploitation=exploitation,
                  yield=Y,
                  Nharvest=Nharv,
                  Ndie=Ndie,
                  wt=wt,
                  avgl=avgl,
                  Nt= Nt,
                  tr=tr,
                  Fmort=Fmort,
                  Mmort=Mmort,
                  Zmort=Zmort,
                  S=S,
                  cf=cf,
                  cm=cm,
                  minlength=minlength,
                  N0=N0,
                  linf=linf,
                  K=K,
                  t0=t0,
                  LWalpha=LWalpha,
                  LWbeta=LWbeta,
                  maxage=maxage
  )

  return(Res)
}
