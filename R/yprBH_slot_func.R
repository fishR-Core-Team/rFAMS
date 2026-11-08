#' @title Simulate expected yield below slot length limits using the Beverton-Holt Yield-per-Recruit model
#'
#' @description An INTERNAL function used by \code{\link{yprBH_SlotLL}} to estimate yield below slot (protected or inverse/harvest) length limit regulations using the Beverton-Holt Yield-per-Recruit (YPR) model with one value each of `cm` (and `lowerSL`, `upperSL`, `cfBelow`, `cfIn`, and `cfAbove`). This is the base function for \code{\link{yprBH_SlotLL}}, is NOT exported, and is NOT expected to be used directly by the user.
#'
#' @inheritParams yprBH_SlotLL
#' @param cm A SINGLE numeric representing conditional natural mortality.
#'
#' @details See details in \code{\link{yprBH_SlotLL}}.
#'
#' @return A one row data.frame with the items described in \code{\link{yprBH_SlotLL}}.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @keywords internal

yprBH_slot_func <- function(lowerSL,upperSL,cfBelow,cfIn,cfAbove,cm,lhparms,
                            recruitmentTL,loi,matchRicker){
  # ---- Extract individual life history values
  N0 <- lhparms[["N0"]]
  tmax <- lhparms[["tmax"]]
  Linf <- lhparms[["Linf"]]
  K <- lhparms[["K"]]
  t0 <- lhparms[["t0"]]
  LWalpha <- lhparms[["LWalpha"]]
  LWbeta <- lhparms[["LWbeta"]]
  # !!!!! Note that checks of all inputs were made in yprBH_SlotLL()

  # !!!!! For a protected slot (so by here recruitmentTL should be NULL), set
  #       recruitmentTL to the lowerSL of the slot limit (i.e, when fish would
  #       first be available for harvest). This is needed for the
  #       test against Linf below
  if (is.null(recruitmentTL)) recruitmentTL <- lowerSL

  #needed to account for rounding issues of sequences
  cm <- round(cm,8)

  # Maximum theoretical weight derived from L-inf and weight to length regression
  #   log10 transformation to linearize it
  Winf <- 10^(LWalpha+log10(Linf)*LWbeta)


  # Yield below the slot limit####
  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  F_Below <- -1*log(1-cfBelow)
  M_Below <- -1*log(1-cm)
  Z_Below <- F_Below+M_Below
  # Annual survival rate (S)
  S_Below <- exp(-Z_Below)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_Below <- (1-S_Below)*(F_Below/Z_Below)

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
  Nr_Below <- N0*exp(-M_Below*tr)
  # Adjust Nr if less than 0 or greater than start, otherwise keep Nr as calculated
  #    not clear that this is done in FAMS
  if (Nr_Below<0) {
    Nr_Below <- 0
  }else if (Nr_Below>N0) {
    Nr_Below <- N0}

  #Max age at lower slot
  tmax_lowerSL <- ((log(1-lowerSL/Linf))/-K)+t0

  # Convenience calculations for beta function below ... per FAMS definitions
  P <- Z_Below/K
  Q <- LWbeta+1
  X <- exp(-K*r)
  Xi <- exp(-K*(tmax_lowerSL-t0))

  # FAMS equation 6:1
  Y_Below <- ((F_Below*Nr_Below*exp(Z_Below*r)*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # ... if matchRicker then Y_Below is "corrected" to match equation 10.22 in Ricker
  if (matchRicker) Y_Below <- Y_Below*exp(M_Below*t0)

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size

  #Calculate the number of fish between recruitment size and lower slot limit size (Nr_Below)
  #Calculate the number that remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_Below <- (Nr_Below - (Nr_Below*exp(-Z_Below* (tmax_lowerSL-tr)))) * (F_Below/Z_Below)
  Ndie_Below <- (Nr_Below - (Nr_Below*exp(-Z_Below* (tmax_lowerSL-tr)))) * (M_Below/Z_Below)

  N0die <-(N0 - Nr_Below) #number that die prior to recruiting to the fishery

  #Check for division by 0 if inverse slot is used = no F below slot so no harvest below slot
  if(Nharv_Below==0){
    avgwt_Below = 0
    avglen_Below = 0
  } else{
    # Mean weight of harvested fish ... FAMS equation 6:6
    avgwt_Below <- Y_Below/Nharv_Below
    # Mean length of harvest fish ... from mean weight and weight-length parameters
    avglen_Below <- 10^((log10(avgwt_Below) - LWalpha)/LWbeta)
  }


  #yield in slot######
  #Max age at upper slot
  tmax_upperSL <- ((log(1-upperSL/Linf))/-K)+t0

  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  #Need in cf for F
  F_in <- -1*log(1-cfIn)
  M_in <- -1*log(1-cm)
  Z_in <- F_in+M_in
  # Annual survival rate (S)
  S_in <- exp(-Z_in)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_in <- (1-S_in)*(F_in/Z_in)

  Nr_in = Nr_Below*exp(-Z_Below* (tmax_lowerSL-tr)) #number reaching slot limit after all mortality Below slot
  P <- Z_in/K
  Q <- LWbeta+1
  X <- exp(-K*(tmax_lowerSL-t0))
  Xi <- exp(-K*(tmax_upperSL-t0))

  #Yield in the slot limit
  Y_in <- ((F_in*Nr_in*exp(Z_in*(tmax_lowerSL-t0))*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # ... if matchRicker then Y_in is "corrected" to match equation 10.22 in Ricker
  if (matchRicker) Y_in <- Y_in*exp(M_in*t0)

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size

  #Use the number of fish between lower and upper slot limit size (Nr_in)
  #Calculate the number that remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_in <- (Nr_in - (Nr_in*exp(-Z_in* (tmax_upperSL-tmax_lowerSL)))) * (F_in/Z_in)
  Ndie_in <- (Nr_in - (Nr_in*exp(-Z_in* (tmax_upperSL-tmax_lowerSL)))) * (M_in/Z_in)


  #Check for division by 0 if protected slot is used = no F in slot so no harvest below slot
  if(Nharv_in==0){
    avgwt_in = 0
    avglen_in = 0
  } else{
    # Mean weight of harvested fish ... FAMS equation 6:6
    avgwt_in <- Y_in/Nharv_in
    # Mean length of harvest fish ... from mean weight and weight-length parameters
    avglen_in <- 10^((log10(avgwt_in) - LWalpha)/LWbeta)
  }

  #yield Above slot######

  #Parameters for Above slot
  F_Above <- -1*log(1-cfAbove)
  M_Above <- -1*log(1-cm)
  Z_Above <- F_Above+M_Above
  # Annual survival rate (S)
  S_Above <- exp(-Z_Above)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation_Above <- (1-S_Above)*(F_Above/Z_Above)


  Nr_Above <- Nr_in*exp(-Z_in* (tmax_upperSL-tmax_lowerSL))
  P <- Z_Above/K
  Q <- LWbeta+1
  X <- exp(-K*(tmax_upperSL-t0))
  Xi <- exp(-K*(tmax-t0))

  Y_Above <- ((F_Above*Nr_Above*exp(Z_Above*(tmax_upperSL-t0))*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # ... if matchRicker then Y_in is "corrected" to match equation 10.22 in Ricker
  if (matchRicker) Y_Above <- Y_Above*exp(M_Above*t0)

  # Number of fish harvested ... FAMS equation 6:4 and 6:5 does not work for slot limit because it needs the number between
  # recruitment size and lower slot size

  #Use the number of fish between upper slot limit and maximum age (Nr_Above)
  #Calculate the number that remain then determine what proportion of lost fish are due to fishing and natural mortality
  Nharv_Above <- (Nr_Above - (Nr_Above*exp(-Z_Above* (tmax-tmax_upperSL)))) * (F_Above/Z_Above)
  Ndie_Above <- (Nr_Above - (Nr_Above*exp(-Z_Above* (tmax-tmax_upperSL)))) * (M_Above/Z_Above)

  #Check for division by 0 if inverse slot is used = no F below slot so no harvest below slot
  if(Nharv_Above==0){
    avgwt_Above = 0
    avglen_Above = 0
  } else{
    # Mean weight of harvested fish ... FAMS equation 6:6
    avgwt_Above <- Y_Above/Nharv_Above
    # Mean length of harvest fish ... from mean weight and weight-length parameters
    avglen_Above <- 10^((log10(avgwt_Above) - LWalpha)/LWbeta)
  }

  #Find out where tloi is in relation to time to lower slot and upper slot.
  #I think this might work.. needs to be tested
  if(!is.null(loi[1])){
    #Get vector of time to length's of interest
    tloi <- rep(NA,length(loi))
    Nloi <- rep(NA,length(loi))

    Nr_Below <- N0*exp(-M_Below*tr)
    for(x in 1:length(loi)){
      #Time to length of interest
      if(loi[x] > Linf){
        WARN("Specified length of interest, loi = ", loi[x]," is greater than\n",
             "Linf of ",Linf," this produces an error. Please select a length\n",
             "of interest below Linf")
        notes <- c(notes,paste0("loi=",loi[x],">Linf"))

      } else {

        tloi[x] <- ((log(1-loi[x]/Linf))/-K)+t0
        if(tloi[x] < tmax_lowerSL){ #time to reach length of interest is less than time to recruit then only M applied
          if(tloi[x] < tr){
            Nloi[x] <- N0*exp(-Z_Below*tloi[x])
          } else {
            Nloi[x] <- Nr_Below*exp(-Z_Below*(tloi[x]-tr))
          }

        } else if (tloi[x] < tmax_upperSL) { #else apply M and F
          #Nloi[x] <- Nr_in*exp(-Z_Below*tmax_lowerSL)
          #Nloi[x] <- Nloi[x]*exp(-Z_in*(tloi[x]-tmax_lowerSL))
          Nloi[x] <- Nr_in*exp(-Z_in*(tloi[x]-tmax_lowerSL))
        } else {
          # Nloi[x] <- N0*exp(-Z_Below*tmax_lowerSL)
          # Nloi[x] <- Nloi[x]*exp(-Z_in*(tmax_upperSL))
          # Nloi[x] <- Nloi[x]*exp(-Z_Above*(tloi[x]-tmax_upperSL))
          Nloi[x] <- Nr_Above*exp(-Z_Above*(tloi[x]-tmax_upperSL))
        }
      }
    }

    #assign column names
    names(Nloi) <- paste0("nAt", loi)
  }

  #Combinde dataframe for output
  tmp1 <- data.frame(
    yieldTotal = Y_Below+Y_in+Y_Above,
    yieldBelow=Y_Below,
    yieldIn=Y_in,
    yieldAbove=Y_Above,
    nharvestTotal = Nharv_Below+Nharv_in+Nharv_Above,
    ndieTotal = Ndie_Below+Ndie_in+Ndie_Above,
    nharvestBelow=Nharv_Below,
    nharvestIn=Nharv_in,
    nharvestAbove=Nharv_Above,
    n0die=N0die,
    ndieBelow=Ndie_Below,
    ndieIn=Ndie_in,
    ndieAbove=Ndie_Above,
    nrBelow=Nr_Below,
    nrIn=Nr_in,
    nrAbove=Nr_Above,
    trBelow=tr,
    trIn=tmax_lowerSL,
    trAbove=tmax_upperSL,
    avglenBelow=avglen_Below,
    avglenIn=avglen_in,
    avglenAbove=avglen_Above,
    avgwtBelow=avgwt_Below,
    avgwtIn=avgwt_in,
    avgwtAbove=avgwt_Above
  )
  tmp2 <- data.frame(
    cm=cm,
    expBelow=exploitation_Below,
    expIn=exploitation_in,
    expAbove=exploitation_Above,
    FBelow=F_Below,
    FIn=F_in,
    FAbove=F_Above,
    MBelow=M_Below,
    MIn=M_in,
    MAbove=M_Above,
    ZBelow=Z_Below,
    ZIn=Z_in,
    ZAbove=Z_Above,
    SBelow=S_Below,
    SIn=S_in,
    SAbove=S_Above,
    cfBelow=cfBelow,
    cfIn=cfIn,
    cfAbove=cfAbove,
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

  if (!is.null(loi[1])) outdf <- cbind(tmp1,t(Nloi),tmp2)
  else outdf <- cbind(tmp1,tmp2)

  outdf

}
