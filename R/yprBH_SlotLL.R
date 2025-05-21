#' @title Main function to simulate expected yield using the Beverton-Holt Yield Per Recruit model for a slot limit
#'
#' @description Main wrapper function to estimate yield using the Beverton-Holt YPR model. This main function accepts a range of values for cf, cm, recruitment length, lower slot limit length, and upper slot limit length.
#'
#' @param recruitmentTL A numeric representing the minimum length limit for recruiting to the fishery in mm.
#' @param lowerSL A numeric representing the length of the lower slot limit in mm.
#' @param upperSL A numeric representing the length of the upper slot limit in mm.
#' @param cf_under Single value, conditional fishing mortality under the lower slot limit.
#' @param cf_in Single value, conditional fishing mortality within the lower and upper slot limit.
#' @param cf_above Single value, conditional fishing mortality over the upper slot limit.
#' @param cm A numeric representing conditional natural mortality
#' @param N0 A numeric representing the initial number of new recruits entering the fishery OR a vector or list that contains named values for each \code{N0}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, \code{LWbeta}, and \code{tmax}
#' @param Linf A numeric representing the point estimate of the asymptotic mean length (L-infinity) from the von Bertalanffy growth model in mm
#' @param K A numeric representing the point estimate of the Brody growth coefficient from the von Bertalanffy growth model
#' @param t0 A numeric representing the point estimate of the x-intercept (i.e., theoretical age at a mean length of 0) from the von Bertalanffy growth model
#' @param LWalpha A numeric representing the point estimate of alpha from the length-weight regression on the log10 scale.
#' @param LWbeta A numeric representing the point estimate of beta from the length-weight regression on the log10 scale.
#' @param tmax An integer representing maximum age in the population in years
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
#' #Load other required packages for organizing output and plotting
#' library(ggplot2)
#' library(dplyr)
#' library(metR)
#'
#' #Estimate yield
#'  Res_1 <- ypr_SlotLimit(recruitmentTL=200,lowerSL=250,upperSL=325,
#'                        cf_under=0.25,cf_in=0.6,cf_above=0.15,cmmin=0.3,cmmax=0.55,cminc=0.05,
#'                        N0=100,Linf=592,K=0.2,t0=-0.3,
#'                        LWalpha=-5.528,LWbeta=3.273,tmax=15)
#'
#'  Res_1
#'
#' @rdname ypr_MinTL_fixed
#' @export
ypr_SlotLimit<-function(recruitmentTL,lowerSL,upperSL,cf_under,cf_in,cf_above,cmmin,cmmax,cminc,
                        N0,Linf=NULL,K=NULL,t0=NULL,
                        LWalpha=NULL,LWbeta=NULL,tmax=NULL){

  if (missing(recruitmentTL))
    stop("Need to specify recruitmentTL")
  if (missing(lowerSL))
    stop("Need to specify lowerSL")
  if (missing(upperSL))
    stop("Need to specify upperSL")
  if (missing(cf_under))
    stop("Need to specify cf_under")
  if (missing(cf_in))
    stop("Need to specify cf_in")
  if (missing(cf_above))
    stop("Need to specify cf_above")
  if (missing(cmmin))
    stop("Need to specify cmmin")
  if (missing(cmmax))
    stop("Need to specify cmmax")
  if (missing(cminc))
    stop("Need to specify cminc")
  #iCheckMLH(minlength)
  iCheckN0(N0)
  iCheckLinf(Linf)
  iCheckK(K)
  iCheckt0(t0)
  iCheckLWa(LWalpha)
  iCheckLWb(LWbeta)
  #iChecktmax(tmax)

  if(recruitmentTL>lowerSL)
    stop("cf_under must be less than cf_in")
  if(lowerSL>upperSL)
    stop("lowerSL must be less than upperSL")
  if(cmmin>cmmax)
    stop("cmmin must be equal to or less than cmmax")


  # Setup data.frame of input values (varying cf and cm, the rest constant)
  res <- expand.grid(recruitmentTL=recruitmentTL,lowerSL=lowerSL,upperSL=upperSL,
                     cf_under=cf_under,cf_in=cf_in,cf_above=cf_above,
                     cm=seq(cmmin,cmmax,cminc),
                     N0=N0,Linf=Linf,K=K,t0=t0,
                     LWalpha=LWalpha,LWbeta=LWbeta,tmax=tmax)
  # Send each row to ypr_func() ... so calc yield et al for all cf & cm combos
  res <- purrr::pmap_df(res,ypr_slot_func)
  # Return result
  return(res)

}
