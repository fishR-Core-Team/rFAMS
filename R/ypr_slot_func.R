#' @title Function to simulate expected yield using the Beverton-Holt Yield Per Recruit model for single input parameters
#'
#' @description Function to estimate yield using the Beverton-Holt YPR model. This main function accepts only single values for cf, cm, and minlength. Use the wrapper ypr() function for specifying range of cf, cm, and minlength
#'
#' @param recruitmentTL A numeric representing the minimum length limit for recruiting to the fishery in mm.
#' @param lowerSL A numeric representing the length of the lower slot limit in mm.
#' @param upperSL A numeric representing the length of the upper slot limit in mm.
#' @param cf_under Single value, conditional fishing mortality under the lower slot limit.
#' @param cf_in Single value, conditional fishing mortality within the lower and upper slot limit.
#' @param cf_above Single value, conditional fishing mortality over the upper slot limit.
#' @param cm A numeric representing conditional natural mortality
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
#' \item cm A numeric representing conditional natural mortality
#' \item TotalYield is the calculated total yield
#' \item TotalHarvest is the calculated total number of harvested fish
#' \item TotalNdie is the calculated total number of fish that die of natural death
#' \item yieldUnder is the calculated yield under the slot limit
#' \item yieldIn is the calculated yied within the slot limit
#' \item yieldAbove is the calculated yield above the slot limit
#' \item exploitationUnder is the exploitation rate under the slot limit
#' \item exploitationIn is the exploitation rate within the slot limit
#' \item exploitationAbove is the exploitation rate above the slot limit
#' \item NharvestUnder is the number of harvested fish under the slot limit
#' \item NharvestIn is the number of harvested fish within the slot limit
#' \item NharvestAbove is the number of harvested fish above the slot limit
#' \item NdieUnder is the number of fish that die of natural death under the slot limit
#' \item NdieIn is the number of fish that die of natural deaths within the slot limit
#' \item NdieAbove is the number of fish that die of natural deaths above the slot limit
#' \item avglenUnder is the average length of fish harvested under the slot limit
#' \item avglenIn is the average length of fish harvested within the slot limit
#' \item avglenAbove is the average length of fish harvested above the slot limit
#' \item avgwtUnder is the average weight of fish harvested under the slot limit
#' \item avgwtIn is the average weight of fish harvested within the slot limit
#' \item avgwtAbove is the average weight of fish harvested above the slot limit
#' \item trUnder is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery)
#' \item trIn is the time for a fish to recruit to a lower length limit of the slot limit
#' \item trOver is the time for a fish to recruit to a upper length limit of the slot limit
#' \item NtUnder is the number of fish at time trUnder (time they become harvestable size under the slot limit)
#' \item NtIn is the number of fish at time trIn (time they reach the lower slot limit size)
#' \item NtAbove is the number of fish at time trAbove (time they reach the upper slot limit size)
#' \item FmortUnder is the estimated instantaneous rate of fishing mortality under the slot limit
#' \item FmortIn is the estimated instantaneous rate of fishing mortality within the slot limit
#' \item FmortAbove is the estimated instantaneous rate of fishing mortality above the slot limit
#' \item MmortUnder is the estimated  instantaneous rate of natural mortality under the slot limit
#' \item MmortIn is the estimated  instantaneous rate of natural mortality within the slot limit
#' \item MmortAbove is the estimated  instantaneous rate of natural mortality above the slot limit
#' \item ZmortUnder is the estimated  instantaneous rate of total mortality under the slot limit
#' \item ZmortIn is the estimated  instantaneous rate of total mortality within the slot limit
#' \item ZmortAbove is the estimated  instantaneous rate of total mortality above the slot limit
#' \item SUnder is the estimated total survival under the slot limit
#' \item SIn is the estimated total survival within the slot limit
#' \item SAbove is the estimated total survival above the slot limit
#' \item cfUnder A numeric representing conditional fishing mortality
#' \item cfIn A numeric representing conditional fishing mortality
#' \item cfOver A numeric representing conditional fishing mortality
#' \item recruitmentTL A numeric representing the minimum length limit for recruiting to the fishery in mm.
#' \item lowerSL A numeric representing the length of the lower slot limit in mm.
#' \item upperSL A numeric representing the length of the upper slot limit in mm.
#' \item N0 A numeric representing the initial number of new recruits entering the fishery OR a vector or list that contains named values for each \code{N0}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, \code{LWbeta}, and \code{maxage}
#' \item Linf A numeric representing the point estimate of the asymptotic mean length (L-infinity) from the von Bertalanffy growth model in mm
#' \item K A numeric representing the point estimate of the Brody growth coefficient from the von Bertalanffy growth model
#' \item t0 A numeric representing the point estimate of the x-intercept (i.e., theoretical age at a mean length of 0) from the von Bertalanffy growth model
#' \item LWalpha A numeric representing the point estimate of alpha from the length-weight regression on the log10 scale.
#' \item LWbeta A numeric representing the point estimate of beta from the length-weight regression on the log10 scale.
#' \item maxage An integer representing maximum age in the population in years
#' }
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' # Estimate yield with fixed parameters
#' Res_1 <- ypr_slot_func(recruitmentTL=200,lowerSL=250,upperSL=325,
#'                        cf_under=0.25,cf_in=0.6,cf_above=0.15,cm=0.4,
#'                        N0=100,Linf=592,K=0.2,t0=-0.3,
#'                        LWalpha=-5.528,LWbeta=3.273,maxage=15)
#' Res_1
#'
#' # Same, but with named vector in N0
#' parms <- c(N0=100,Linf=592,K=0.2,t0=-0.3,LWalpha=-5.528,LWbeta=3.273,maxage=15)
#' Res_2 <- ypr_func(recruitmentTL=200,lowerSL=250,upperSL=325,
#'                        cf_under=0.25,cf_in=0.6,cf_above=0.15,cm=0.4,
#'                        N0=parms)
#' Res_2
#'
#'
#' @rdname ypr_function
#' @export

ypr_slot_func <- function(recruitmentTL,lowerSL,upperSL,cf_under,cf_in,cf_above,cm, #cmmin,cmmax,cminc,
                     N0,Linf=NULL,K=NULL,t0=NULL,
                     LWalpha=NULL,LWbeta=NULL,maxage=NULL){
  # ---- Check inputs
  # iCheckMLH(minlength)
  # iCheckcf(cf)
  # iCheckcm(cm)
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
  iCheckMaxAge(maxage)  #Maxage is not always an integer in the slot limit code.

  #Can't use ypr_func because it calculates Nt based on natural mortality only because below length limit M the only source of mortality
  #And Nt in ypr_func is calculated only with Mmort
  #Nt is need for number entering slot but when fishing mort occurs below slot Nt must include M and F

  # Maximum theoretical weight derived from L-inf and weight to length regression
  #   log10 transformation to linearize it
  Winf <- 10^(LWalpha+log10(Linf)*LWbeta)

  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  Fmort_under <- -1*log(1-cf_under)
  Mmort_under <- -1*log(1-cm)
  Zmort_under <- Fmort_under+Mmort_under
  # Annual survival rate (S)
  S_under <- exp(-Zmort_under)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_under <- (1-S_under)*(Fmort_under/Zmort_under)

  # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  #   needed adjustment if minlength<Linf
  # and amount of time (years) to recruit to the fishery (r) ... defined in FAMS
  if (recruitmentTL<Linf) {
    tr <- ((log(1-recruitmentTL/Linf))/-K)+t0
  }else {
    tr <- ((log(1-recruitmentTL/(recruitmentTL+.1)))/-K)+t0}

  r <- tr-t0

  # Number recruiting to fishery based on time at minimum length (tr) ...
  #    FAMS equation 6:3
  Nt_under <- N0*exp(-Mmort_under*tr)
  # Adjust Nt if less than 0 or greater than start, otherwise keep Nt as calculated
  #    not clear that this is done in FAMS
  if (Nt_under<0) {
    Nt_under <- 0
  }else if (Nt_under>N0) {
    Nt_under <- N0}

  #Max age at lower slot
  tmax_lowerSL <- ((log(1-lowerSL/Linf))/-K)+t0

  # Convenience calculations for beta function below ... per FAMS definitions
  P <- Zmort_under/K
  Q <- LWbeta+1
  X <- exp(-K*r)
  Xi <- exp(-K*(tmax_lowerSL-t0))

  # FAMS equation 6:1
  Y_under <- ((Fmort_under*Nt_under*exp(Zmort_under*r)*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size
  #Number of fish harvested:
  #Calculate the number of fish between recruitment size and lower slot limit size (Nt_under)
  #Calculate the number remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_under <- (Nt_under - (Nt_under*exp(-Zmort_under* (tmax_lowerSL-tr)))) * (Fmort_under/Zmort_under)
  #Ndie also adds the number of fish lost to natural mortality before recruiting to the fishery
  Ndie_under <- (N0 - Nt_under)  + (Nt_under - (Nt_under*exp(-Zmort_under* (tmax_lowerSL-tr)))) * (Mmort_under/Zmort_under)

  # Mean weight of harvested fish ... FAMS equation 6:6
  avgwt_under <- Y_under/Nharv_under

  # Mean length of harvest fish ... from mean weight and weight-length parameters
  avglen_under <- 10^((log10(avgwt_under) - LWalpha)/LWbeta)

  #yield in slot
  #Max age at upper slot
  tmax_upperSL <- ((log(1-upperSL/Linf))/-K)+t0

  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  #Need in cf for Fmort
  Fmort_in <- -1*log(1-cf_in)
  Mmort_in <- -1*log(1-cm)
  Zmort_in <- Fmort_in+Mmort_in
  # Annual survival rate (S)
  S_in <- exp(-Zmort_in)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_in <- (1-S_in)*(Fmort_in/Zmort_in)

  Nt_in = Nt_under*exp(-Zmort_under* (tmax_lowerSL-tr)) #number reaching slot limit after all mortality under slot
  P <- Zmort_in/K
  Q <- LWbeta+1
  X <- exp(-K*(tmax_lowerSL-t0))
  Xi <- exp(-K*(tmax_upperSL-t0))

  #Yield in the slot limit
  Y_in <- ((Fmort_in*Nt_in*exp(Zmort_in*(tmax_lowerSL-t0))*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size
  #Number of fish harvested:
  #Calculate the number of fish between recruitment size and lower slot limit size (Nt_under)
  #Calculate the number remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_in <- (Nt_in - (Nt_in*exp(-Zmort_in* (tmax_upperSL-tmax_lowerSL)))) * (Fmort_in/Zmort_in)
  Ndie_in <- (Nt_in - (Nt_in*exp(-Zmort_in* (tmax_upperSL-tmax_lowerSL)))) * (Mmort_in/Zmort_in)

  # Mean weight of harvested fish ... FAMS equation 6:6
  avgwt_in <- Y_in/Nharv_in

  # Mean length of harvest fish ... from mean weight and weight-length parameters
  avglen_in <- 10^((log10(avgwt_in) - LWalpha)/LWbeta)


  #Parameters for over slot
  Fmort_above <- -1*log(1-cf_above)
  Mmort_above <- -1*log(1-cm)
  Zmort_above <- Fmort_above+Mmort_above
  # Annual survival rate (S)
  S_above <- exp(-Zmort_above)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_above <- (1-S_above)*(Fmort_above/Zmort_above)

  #Need to get number alive after harvest and natural mortality in slot
  #Nt is the number going into the slot
  #need tmax_upperSl - tmax_lowerSL (for age at recruitment to slot)
  #Number going into the upper slot - needs Zmort from within slot
  Nt_above <- Nt_in*exp(-Zmort_in* (tmax_upperSL-tmax_lowerSL))
  P <- Zmort_above/K
  Q <- LWbeta+1
  X <- exp(-K*(tmax_upperSL-t0))
  Xi <- exp(-K*(maxage-t0))

  Y_above <- ((Fmort_above*Nt_above*exp(Zmort_above*(tmax_upperSL-t0))*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size
  #Number of fish harvested:
  #Calculate the number of fish between recruitment size and lower slot limit size (Nt_under)
  #Calculate the number remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_above <- (Nt_above - (Nt_above*exp(-Zmort_above* (maxage-tmax_upperSL)))) * (Fmort_above/Zmort_above)
  Ndie_above <- (Nt_above - (Nt_above*exp(-Zmort_above* (maxage-tmax_upperSL)))) * (Mmort_above/Zmort_above)

  # Mean weight of harvested fish ... FAMS equation 6:6
  avgwt_above <- Y_above/Nharv_above

  # Mean length of harvest fish ... from mean weight and weight-length parameters
  avglen_above <- 10^((log10(avgwt_above) - LWalpha)/LWbeta)


  #Combinde dataframe for output
  data.frame(
    cm=cm,
    TotalYield = Y_under+Y_in+Y_above,
    TotalNharv = Nharv_under+Nharv_in+Nharv_above,
    TotalNdie = Ndie_under+Ndie_in+Ndie_above,
    yieldUnder=Y_under,
    yieldIn=Y_in,
    yieldAbove=Y_above,
    exploitationUnder=exploitation_under,
    exploitationIn=exploitation_in,
    exploitationAbove=exploitation_above,
    NharvestUnder=Nharv_under,
    NharvestIn=Nharv_in,
    NharvestAbove=Nharv_above,
    NdieUnder=Ndie_under,
    NdieIn=Ndie_in,
    NdieAbove=Ndie_above,
    avglenUnder=avglen_under,
    avglenIn=avglen_in,
    avglenAbove=avglen_above,
    avgwtUnder=avgwt_under,
    avgwtIn=avgwt_in,
    avgwtAbove=avgwt_above,
    trUnder=tr,
    trIn=tmax_lowerSL,
    trOver=tmax_upperSL,
    NtUnder=Nt_under,
    NtIn=Nt_in,
    NtAbove=Nt_above,
    FmortUnder=Fmort_under,
    FmortIn=Fmort_in,
    FmortAbove=Fmort_above,
    MmortUnder=Mmort_under,
    MmortIn=Mmort_in,
    MmortAbove=Mmort_above,
    ZmortUnder=Zmort_under,
    ZmortIn=Zmort_in,
    ZmortAbove=Zmort_above,
    SUnder=S_under,
    SIn=S_in,
    SAbove=S_above,
    cfUnder=cf_under,
    cfIn=cf_in,
    cfOver=cf_above,
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

}
