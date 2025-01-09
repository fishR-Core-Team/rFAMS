#' @title Simulate expected yield using the Beverton-Holt Yield-per-Recruit model for single input parameters
#'
#' @description Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model. This main function accepts only single values for conditional fishing mortalitiy (\code{cf}), conditional natural mortality (\code{cm}), and a minimum length limit for harvest (\code{minLL}).
#'
#' @param cf A single numeric representing conditional fishing mortality.
#' @param cm A single numeric representing conditional natural mortality.
#' @param minLL A single numeric representing the minimum length limit for harvest in mm.
#' @param N0 A single numeric representing the initial number of fish in the population OR a vector or list that contains named values for each \code{N0}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, \code{LWbeta}, and \code{maxage}. See examples.
#' @param Linf A single numeric representing the point estimate of the asymptotic mean length (L-infinity) from the von Bertalanffy growth model in mm. May be given in a named vector or list given to \code{N0} (see examples).
#' @param K A single numeric representing the point estimate of the Brody growth coefficient from the von Bertalanffy growth model. May be given in a named vector or list given to \code{N0} (see examples).
#' @param t0 A single numeric representing the point estimate of the x-intercept (i.e., theoretical age at a mean length of 0) from the von Bertalanffy growth model. May be given in a named vector or list given to \code{N0} (see examples).
#' @param LWalpha A single numeric representing the point estimate of alpha from the length-weight regression on the log10 scale. May be given in a named vector or list given to \code{N0} (see examples).
#' @param LWbeta A single numeric representing the point estimate of beta from the length-weight regression on the log10 scale. May be given in a named vector or list given to \code{N0} (see examples).
#' @param maxage An single whole number representing maximum age in the population in years. May be given in a named vector or list given to \code{N0} (see examples).
#' @param matchRicker A logical that indicates whether the yield function should match that in Ricker (). Defaults to \code{TRUE}. The only reason to changed to \code{FALSE} is to try to match output from FAMS. See the "YPR_FAMSvRICKER" article.
#'
#' @details Details will be filled out later
#'
#' @return A data.frame with the following calculated values:
#' \itemize{
#' \item \code{yield} is the estimated yield (in g).
#' \item \code{exploitation} is the exploitation rate.
#' \item \code{Nharvest} is the number of harvested fish.
#' \item \code{Ndie} is the number of fish that die of natural deaths.
#' \item \code{Nt} is the number of fish at time tr (time they become harvestable size).
#' \item \code{avgwt} is the average weight of fish harvested.
#' \item \code{avglen} is the average length of fish harvested.
#' \item \code{tr} is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery).
#' \item \code{Fmort} is the instantaneous rate of fishing mortality.
#' \item \code{Mmort} is the instantaneous rate of natural mortality.
#' \item \code{Zmort} is the instantaneous rate of total mortality.
#' \item \code{S} is the (total) annual rate of survival.
#' }
#'
#' For convenience the data.frame also contains the model input values (\code{minLL}, \code{cf}, \code{cm}, \code{N0}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, \code{LWbeta}, and \code{maxage}).
#'
#' The data.frame also contains a \code{notes} value which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are as follows:
#'
#' \itemize{
#' \item \code{minLL>=Linf}: The minimum length limit (minLL) being explored was greater than the given asymptotic mean length (Linf). For the purpose (only) of computing the time at recruitment to the fishery (tr) the Linf was set to minLL+0.1.
#' \item \code{tr<t0}: The age at recruitment to the fishery (tr) was less than the hypothetical time when the mean length is zero (t0). The fish can't recruit to the fishery prior to having length 0 so tr was set to t0. This also assures that the time it takes to recruit to the fishery is greater than 0.
#' \item \code{Nt<0}: The number of fish recruiting to the fishery was less than 0. There cannot be negative fish, so Nt was then set to 0.
#' \item \code{Nt>N0}: The number of fish recruiting to the fishery was more than the number of fish recruited to the populations. Fish cannot be added to the cohort, so Nt was set to N0.
#' \item \code{Y=Infinite}: The calculated yield (Y) was inifinity, which is impossible and suggests some other propblem. Yield was set to NA.
#' \item \code{Y<0}: The calculated yield (Y) was negative, which is impossible. Yield was set to 0.
#' \item \code{Nharv<0}: The calculated number of fish harvested (Nharv) was negative, which is not possible. Number harvested was set to 0.
#' \item \code{Nharv>Nt}: The calculated number of fish harvested (Nharv) was greater than the number of fish recruiting to the fishery, which is impossible. The number harvested was set to the number recruiting to the fishery.
#' \item \code{Ndie<0}: The calculated number of fish recruiting to the fishery that died naturally (Ndie) was negative, which is not possible. Number that died was set to 0.
#' \item \code{Ndie>Nt}: The calculated number of fish recruiting to the fishery that died naturally (Ndie) was greater than the number of fish recruiting to the fishery, which is impossible. The number that died was set to the number recruiting to the fishery.
#' \item \code{agvglen<minLL}: The average length of harvested fish was less than the given minimum length limit being explored, which is not possible (with only legal harvest). The average length was set to the minimum length limit.
#' }
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @seealso \code{\link{ypr_minLL_fixed}} and \code{\link{ypr_minLL_var}} for simulating yield with multiple values of \code{cf}, \code{cm}, and \code{minLL}.
#'
#' @references
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada. Was (is?) from \url{https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/1485.pdf}.
#'
#' Slipke, J.W., and M.J. Maceina. 2014. Fishery analysis and modeling simulator. v1.64. American Fisheries Society, Bethesda, MD. Was (is?) from \url{https://units.fisheries.org/fits/wp-content/uploads/sites/29/2019/06/FAMS-1.64-Manual.pdf}.
#'
#' @examples
#' # Estimate yield with fixed parameters
#' Res_1 <- ypr_func(cf=0.45,cm=0.25,
#'                   minLL=355,
#'                   N0=100,
#'                   Linf=2000,K=0.50,t0=-0.616,
#'                   LWalpha=-5.453,LWbeta=3.10,
#'                   maxage=15)
#' Res_1
#'
#' # Same, but with named vector in N0
#' parms <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
#' Res_2 <- ypr_func(cf=0.45,cm=0.25,minLL=355,N0=parms)
#' Res_2
#'
#' # Same, but with named list in N0
#' parms <- list(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
#' Res_3 <- ypr_func(cf=0.45,cm=0.25,minLL=355,N0=parms)
#' Res_3
#'
#' @rdname ypr_func
#' @export

ypr_func <- function(minLL,cf,cm,
                     N0,Linf=NULL,K=NULL,t0=NULL,
                     LWalpha=NULL,LWbeta=NULL,maxage=NULL,
                     matchRicker=TRUE){
  # ---- Check inputs
  iCheckMLH(minLL)
  iCheckcf(cf)
  iCheckcm(cm)
  iCheckN0(N0)    # initial check if vector/list
  if (length(N0)>1) {
    Linf <- N0[["Linf"]]
    K <- N0[["K"]]
    t0 <- N0[["t0"]]
    LWalpha <- N0[["LWalpha"]]
    LWbeta <- N0[["LWbeta"]]
    maxage <- N0[["maxage"]]
    N0 <- N0[["N0"]]
    iCheckN0(N0)  # second check of single value of N0
  }
  iCheckLinf(Linf)
  iCheckK(K)
  iCheckt0(t0)
  iCheckLWa(LWalpha)
  iCheckLWb(LWbeta)
  iCheckMaxAge(maxage)

  # prepare notes vector
  notes <- NULL

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
  Nt <- N0*exp(-Mmort*tr)
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
  P <- Zmort/K
  Q <- LWbeta+1
  X <- exp(-K*r)
  Xi <- exp(-K*(maxage-t0))

  # ---- Compute yield
  # Y is FAMS equation 6:1 ...
  #   see testing for internal iIbeta() to note how it matches other packages
  Y <- ((Fmort*Nt*exp(Zmort*r)*Winf)/K)*(iIbeta(X,P,Q)-iIbeta(Xi,P,Q))
  # ... if matchRicker then Y is "corrected" to match equation 10.22 in Ricker
  if (matchRicker) Y <- Y*exp(Mmort*t0)

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
  Nharv <- Nt*(Fmort/Zmort)

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
  Ndie <- Nt*(Mmort/Zmort)

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
    minLL=minLL,
    N0=N0,
    Linf=Linf,
    K=K,
    t0=t0,
    LWalpha=LWalpha,
    LWbeta=LWbeta,
    maxage=maxage,
    notes=paste(notes,collapse="; ")
  )
}
