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
#' @param lhparms A named vector or list that contains values for each `N0`, `tmax`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See \code{\link{makeLH}} for definitions of these life history parameters. Also see details.
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
#' \item uUnder is the exploitation rate under the slot limit
#' \item uIn is the exploitation rate within the slot limit
#' \item uAbove is the exploitation rate above the slot limit
#' \item NharvestUnder is the number of harvested fish under the slot limit
#' \item NharvestIn is the number of harvested fish within the slot limit
#' \item NharvestAbove is the number of harvested fish above the slot limit
#' \item N0die is the number of fish that die of natural death before entering the fishery at a minimum length
#' \item NdieUnder is the number of fish that die of natural death between entering the fishery and the lower slot limit
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
#' \item NrUnder is the number of fish at time trUnder (time they become harvestable size under the slot limit)
#' \item NrIn is the number of fish at time trIn (time they reach the lower slot limit size)
#' \item NrAbove is the number of fish at time trAbove (time they reach the upper slot limit size)
#' \item FUnder is the estimated instantaneous rate of fishing mortality under the slot limit
#' \item FIn is the estimated instantaneous rate of fishing mortality within the slot limit
#' \item FAbove is the estimated instantaneous rate of fishing mortality above the slot limit
#' \item MUnder is the estimated  instantaneous rate of natural mortality under the slot limit
#' \item MIn is the estimated  instantaneous rate of natural mortality within the slot limit
#' \item MAbove is the estimated  instantaneous rate of natural mortality above the slot limit
#' \item ZUnder is the estimated  instantaneous rate of total mortality under the slot limit
#' \item ZIn is the estimated  instantaneous rate of total mortality within the slot limit
#' \item ZAbove is the estimated  instantaneous rate of total mortality above the slot limit
#' \item SUnder is the estimated total survival under the slot limit
#' \item SIn is the estimated total survival within the slot limit
#' \item SAbove is the estimated total survival above the slot limit
#' \item cfUnder A numeric representing conditional fishing mortality
#' \item cfIn A numeric representing conditional fishing mortality
#' \item cfOver A numeric representing conditional fishing mortality
#' \item recruitmentTL A numeric representing the minimum length limit for recruiting to the fishery in mm.
#' \item lowerSL A numeric representing the length of the lower slot limit in mm.
#' \item upperSL A numeric representing the length of the upper slot limit in mm.
#' \item N0 A numeric representing the initial number of new recruits entering the fishery OR a vector or list that contains named values for each \code{N0}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, \code{LWbeta}, and \code{tmax}
#' \item Linf A numeric representing the point estimate of the asymptotic mean length (L-infinity) from the von Bertalanffy growth model in mm
#' \item K A numeric representing the point estimate of the Brody growth coefficient from the von Bertalanffy growth model
#' \item t0 A numeric representing the point estimate of the x-intercept (i.e., theoretical age at a mean length of 0) from the von Bertalanffy growth model
#' \item LWalpha A numeric representing the point estimate of alpha from the length-weight regression on the log10 scale.
#' \item LWbeta A numeric representing the point estimate of beta from the length-weight regression on the log10 scale.
#' \item tmax An integer representing maximum age in the population in years
#' }
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' # Life history parameters to be used below
#' LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
#'
#' # Estimate yield with fixed parameters
#' Res_1 <- yprBH_slot_func(recruitmentTL=200,lowerSL=250,upperSL=325,
#'                        cf_under=0.25,cf_in=0.6,cf_above=0.15,cm=0.4,
#'                        lhparms=LH)
#' Res_1
#'
#'
#' @rdname yprBH_slot_function
#' @export

yprBH_slot_func <- function(recruitmentTL,lowerSL,upperSL,cf_under,cf_in,cf_above,cm,
                          lhparms){
  # ---- Check inputs
  # iCheckN0(N0)
  # iCheckLinf(Linf)
  # iCheckK(K)
  # iCheckt0(t0)
  # iCheckLWa(LWalpha)
  # iCheckLWb(LWbeta)
  # iCheckMaxAge(tmax)


  # Extract individual life history values
  N0 <- lhparms[["N0"]]
  tmax <- lhparms[["tmax"]]
  Linf <- lhparms[["Linf"]]
  K <- lhparms[["K"]]
  t0 <- lhparms[["t0"]]
  LWalpha <- lhparms[["LWalpha"]]
  LWbeta <- lhparms[["LWbeta"]]

  #Can't use ypr_func because it calculates Nr based on natural mortality only because below length limit M the only source of mortality
  #And Nr in ypr_func is calculated only with M
  #Nr is need for number entering slot but when fishing mort occurs below slot Nr must include M and F

  # Maximum theoretical weight derived from L-inf and weight to length regression
  #   log10 transformation to linearize it
  Winf <- 10^(LWalpha+log10(Linf)*LWbeta)


  # Yield under the slot limit####
  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  F_under <- -1*log(1-cf_under)
  M_under <- -1*log(1-cm)
  Z_under <- F_under+M_under
  # Annual survival rate (S)
  S_under <- exp(-Z_under)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_under <- (1-S_under)*(F_under/Z_under)

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
  Nr_under <- N0*exp(-M_under*tr)
  # Adjust Nr if less than 0 or greater than start, otherwise keep Nr as calculated
  #    not clear that this is done in FAMS
  if (Nr_under<0) {
    Nr_under <- 0
  }else if (Nr_under>N0) {
    Nr_under <- N0}

  #Max age at lower slot
  tmax_lowerSL <- ((log(1-lowerSL/Linf))/-K)+t0

  # Convenience calculations for beta function below ... per FAMS definitions
  P <- Z_under/K
  Q <- LWbeta+1
  X <- exp(-K*r)
  Xi <- exp(-K*(tmax_lowerSL-t0))

  # FAMS equation 6:1
  Y_under <- ((F_under*Nr_under*exp(Z_under*r)*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size

  #Calculate the number of fish between recruitment size and lower slot limit size (Nr_under)
  #Calculate the number that remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_under <- (Nr_under - (Nr_under*exp(-Z_under* (tmax_lowerSL-tr)))) * (F_under/Z_under)
  Ndie_under <- (Nr_under - (Nr_under*exp(-Z_under* (tmax_lowerSL-tr)))) * (M_under/Z_under)

  N0die <-(N0 - Nr_under) #number that die prior to recruiting to the fishery

  # Mean weight of harvested fish ... FAMS equation 6:6
  avgwt_under <- Y_under/Nharv_under

  # Mean length of harvest fish ... from mean weight and weight-length parameters
  avglen_under <- 10^((log10(avgwt_under) - LWalpha)/LWbeta)


  #yield in slot######
  #Max age at upper slot
  tmax_upperSL <- ((log(1-upperSL/Linf))/-K)+t0

  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  #Need in cf for F
  F_in <- -1*log(1-cf_in)
  M_in <- -1*log(1-cm)
  Z_in <- F_in+M_in
  # Annual survival rate (S)
  S_in <- exp(-Z_in)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_in <- (1-S_in)*(F_in/Z_in)

  Nr_in = Nr_under*exp(-Z_under* (tmax_lowerSL-tr)) #number reaching slot limit after all mortality under slot
  P <- Z_in/K
  Q <- LWbeta+1
  X <- exp(-K*(tmax_lowerSL-t0))
  Xi <- exp(-K*(tmax_upperSL-t0))

  #Yield in the slot limit
  Y_in <- ((F_in*Nr_in*exp(Z_in*(tmax_lowerSL-t0))*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size

  #Use the number of fish between lower and upper slot limit size (Nr_in)
  #Calculate the number that remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_in <- (Nr_in - (Nr_in*exp(-Z_in* (tmax_upperSL-tmax_lowerSL)))) * (F_in/Z_in)
  Ndie_in <- (Nr_in - (Nr_in*exp(-Z_in* (tmax_upperSL-tmax_lowerSL)))) * (M_in/Z_in)

  # Mean weight of harvested fish ... FAMS equation 6:6
  avgwt_in <- Y_in/Nharv_in

  # Mean length of harvest fish ... from mean weight and weight-length parameters
  avglen_in <- 10^((log10(avgwt_in) - LWalpha)/LWbeta)

  #yield over slot######

  #Parameters for over slot
  F_above <- -1*log(1-cf_above)
  M_above <- -1*log(1-cm)
  Z_above <- F_above+M_above
  # Annual survival rate (S)
  S_above <- exp(-Z_above)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_above <- (1-S_above)*(F_above/Z_above)


  Nr_above <- Nr_in*exp(-Z_in* (tmax_upperSL-tmax_lowerSL))
  P <- Z_above/K
  Q <- LWbeta+1
  X <- exp(-K*(tmax_upperSL-t0))
  Xi <- exp(-K*(tmax-t0))

  Y_above <- ((F_above*Nr_above*exp(Z_above*(tmax_upperSL-t0))*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size

  #Use the number of fish between upper slot limit and maximum age (Nr_above)
  #Calculate the number that remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_above <- (Nr_above - (Nr_above*exp(-Z_above* (tmax-tmax_upperSL)))) * (F_above/Z_above)
  Ndie_above <- (Nr_above - (Nr_above*exp(-Z_above* (tmax-tmax_upperSL)))) * (M_above/Z_above)

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
    uUnder=exploitation_under,
    uIn=exploitation_in,
    uAbove=exploitation_above,
    NharvestUnder=Nharv_under,
    NharvestIn=Nharv_in,
    NharvestAbove=Nharv_above,
    N0die=N0die,
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
    NrUnder=Nr_under,
    NrIn=Nr_in,
    NrAbove=Nr_above,
    FUnder=F_under,
    FIn=F_in,
    FAbove=F_above,
    MUnder=M_under,
    MIn=M_in,
    MAbove=M_above,
    ZUnder=Z_under,
    ZIn=Z_in,
    ZAbove=Z_above,
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
    tmax=tmax
  )

}
