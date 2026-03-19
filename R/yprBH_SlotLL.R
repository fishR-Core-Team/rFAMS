#' @title Simulate expected yield using below slot limit regulations using the Beverton-Holt Yield-per-Recruit model
#'
#' @description Simulate yield below slot length regulations using the Beverton-Holt Yield-per-Recruit (YPR) model with (possibly) multiple values for conditional natural mortality (`cm`) and chosen values for the lower and upper lengths of the slot (i.e,. `lowerSL` and `upperSL`); conditional fishing mortality below (`cfBelow`), in (`cfIn`), and above (`cfAbove`) the slot; and length when fish recruit to the fishery (`recruitmentTL`).
#'
#' @param lowerSL A single numeric representing the length of the lower slot limit in mm. See details. Must be less than `upperSL`.
#' @param upperSL A single numeric representing the length of the upper slot limit in mm. See details. Must be less than `Linf` in `lhparms`.
#' @param cfBelow A single numeric representing conditional fishing mortality below the lower slot limit length. Must be between 0 and 1 (inclusive).
#' @param cfIn A single numeric representing conditional fishing mortality between the lower and upper slot limit lengths (i.e., "in the slot"). Must be between 0 and 1 (inclusive).
#' @param cfAbove A single numeric representing conditional fishing mortality above the upper slot limit length. Must be between 0 and 1 (inclusive).
#' @param cm A numeric vector of conditional natural mortality values. All values must be between 0 and 1 (inclusive).
#' @param lhparms A named vector or list that contains values for each `N0`, `tmax`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See \code{\link{makeLH}} for definitions of these life history parameters. Also see details.
#' @param recruitmentTL A single numeric that represents the minimum length (in mm) for recruiting to the fishery. Cannot be greater than `lowerSL`.
#' @param loi A numeric vector of lengths (in mm) of interest. Used to determine number of fish that reach these lengths. All must be less than `Linf` in `lhparms`.
#' @param matchRicker A logical that indicates whether the yield function should match that in Ricker (1975). Defaults to \code{TRUE}. The only reason to changed to \code{FALSE} is to try to match output from FAMS. See the \href{https://fishr-core-team.github.io/rFAMS/articles/YPR_FAMSvRICKER.html}{FAMS vs Ricker article}.
#' @param label An optional string to label the type of slot limit being simulated.
#'
#' @return A data.frame with the following calculated values:
#' \itemize{
#' \item `yieldTotal` is the calculated total yield
#' \item `yieldBelow` is the calculated yield below the slot limit
#' \item `yieldIn` is the calculated yield within the slot limit
#' \item `yieldAbove` is the calculated yield above the slot limit
#' \item `nharvTotal` is the calculated total number of harvested fish below, in and above slot.
#' \item `ndieTotal` is the calculated total number of fish that die of natural death
#' \item `nharvestBelow` is the number of harvested fish below the slot limit
#' \item `nharvestIn` is the number of harvested fish within the slot limit
#' \item `nharvestAbove` is the number of harvested fish above the slot limit
#' \item `n0die` is the number of fish that die of natural death before entering the fishery at a minimum length
#' \item `ndieBelow` is the number of fish that die of natural death between entering the fishery and the lower slot limit
#' \item `ndieIn` is the number of fish that die of natural deaths within the slot limit
#' \item `ndieAbove` is the number of fish that die of natural deaths above the slot limit
#' \item `nrBelow` is the number of fish at time trBelow (time they become harvestable size below the slot limit)
#' \item `nrIn` is the number of fish at time trIn (time they reach the lower slot limit size)
#' \item `nrAbove` is the number of fish at time trAbove (time they reach the upper slot limit size)
#' \item `trBelow` is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery)
#' \item `trIn` is the time for a fish to recruit to a lower length limit of the slot limit
#' \item `trAbove` is the time for a fish to recruit to a upper length limit of the slot limit
#' \item `avglenBelow` is the average length of fish harvested below the slot limit
#' \item `avglenIn` is the average length of fish harvested within the slot limit
#' \item `avglenAbove` is the average length of fish harvested above the slot limit
#' \item `avgwtBelow` is the average weight of fish harvested below the slot limit
#' \item `avgwtIn` is the average weight of fish harvested within the slot limit
#' \item `avgwtAbove` is the average weight of fish harvested above the slot limit
#' \item `nAtxxx` is the number that reach the length of interest supplied. There will be one column for each length of interest.
#' \item `cm` A numeric representing conditional natural mortality
#' \item `expBelow` is the exploitation rate below the slot limit
#' \item `expIn` is the exploitation rate within the slot limit
#' \item `expAbove` is the exploitation rate above the slot limit
#' \item `FBelow` is the estimated instantaneous rate of fishing mortality below the slot limit
#' \item `FIn` is the estimated instantaneous rate of fishing mortality within the slot limit
#' \item `FAbove` is the estimated instantaneous rate of fishing mortality above the slot limit
#' \item `MBelow` is the estimated  instantaneous rate of natural mortality below the slot limit
#' \item `MIn` is the estimated  instantaneous rate of natural mortality within the slot limit
#' \item `MAbove` is the estimated  instantaneous rate of natural mortality above the slot limit
#' \item `ZBelow` is the estimated  instantaneous rate of total mortality below the slot limit
#' \item `ZIn` is the estimated  instantaneous rate of total mortality within the slot limit
#' \item `ZAbove` is the estimated  instantaneous rate of total mortality above the slot limit
#' \item `SBelow` is the estimated total survival below the slot limit
#' \item `SIn` is the estimated total survival within the slot limit
#' \item `SAbove` is the estimated total survival above the slot limit
#' }
#'
#' For convenience the data.frame also contains the model input values (`lowerSL`, `upperSL`, `cfBelow`, `cfIn`, `cfAbove`, `cm` from input vectors; `N0`; `Linf`; `K`; `t0`; `LWalpha`; `LWbeta`; and `tmax` from `lhparms`) and, optionally, the string provided in `label`.
#'
#' @details Details will be filled out later.
#'
#' Note that the main calculations are in the internal `yprBH_slot_func` (use `rFAMS:::yprBH_slot_func` to see that source code).
#'
#' @seealso \code{\link{yprBH_MinLL}} for estimating yield with the yield-per-recruit model using a minimum length limits, or \code{\link{dpmBH_MinLL}} for estimating yield with a dynamic pool model using a minimum length limit.
#'
#' See \href{https://fishr-core-team.github.io/rFAMS/articles/YPR_SlotLL.html}{this demonstration page} for more examples of this function.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' #Load other required packages for organizing output and plotting
#' library(ggplot2)  #for plotting
#' library(dplyr)    #for select
#' library(tidyr)    #for pivot_longer
#'
#' # Life history parameters to be used below
#' LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
#' # conditional natural mortality vector
#' cm <- seq(from = 0.1, to = 0.9, by = 0.1)
#'
#' # Estimate yield based on a protected slot limit
#' Res_1 <- yprBH_SlotLL(lowerSL=250,upperSL=325,
#'                       cfBelow=0.25,cfIn=0.0,cfAbove=0.15,cm=cm,
#'                       lhparms=LH,recruitmentTL=200,
#'                       loi=c(200,250,300,325,350),label="250-325")
#'
#' Res_1
#'
#' # Plot results
#' # Total Yield vs Conditional Natural Mortality (cm)
#' ggplot(data=Res_1,mapping=aes(x=cm,y=yieldTotal)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Total Yield (g)",x="Conditional Natural Mortality (cm)") +
#'   theme_bw()
#'
#'
#' # Yield below, in, and above the slot limit vs Conditional Natural Mortality (cm)
#' # Select columns for plotting
#' plot_data <- Res_1 |>
#'   select(cm, yieldBelow, yieldIn, yieldAbove) |>
#'   pivot_longer(!cm, names_to="YieldCat",values_to="Yield")
#'
#' # Generate plot
#' ggplot(data=plot_data,mapping=aes(x=cm,y=Yield,group=YieldCat,color=YieldCat)) +
#'   geom_point() +
#'   scale_color_discrete(name="Yield",labels=c("Above SL","In SL","Below SL"))+
#'   geom_line() +
#'   labs(y="Total Yield (g)",x="Conditional Natural Mortality (cm)") +
#'   theme_bw() +
#'   theme(legend.position = "top")+
#'   guides(color=guide_legend(title="Yield"))
#'
#' @rdname yprBH_SlotLL
#' @export
yprBH_SlotLL<-function(lowerSL,upperSL,cfBelow,cfIn,cfAbove,cm,lhparms,
                       recruitmentTL=NULL,loi=NULL,matchRicker=FALSE,label=NULL){
  # ---- Check inputs
  iCheckLHparms(lhparms,"lhparms")
  iCheckCondMort(cm,"cm")
  iCheckCondMort(cfBelow,"cfBelow",onlyone=TRUE)
  iCheckCondMort(cfIn,"cfIn",onlyone=TRUE)
  iCheckCondMort(cfAbove,"cfAbove",onlyone=TRUE)
  iCheckloi(loi)
  iCheckSlotTL(lowerSL,lhparms[["Linf"]],"lowerSL")
  iCheckSlotTL(upperSL,lhparms[["Linf"]],"upperSL")
  # .... check that slot lengths are in correct order
  if (lowerSL>=upperSL) STOP("'lowerSL' must be less than 'upperSL'.")
  iCheckRecruitmentTL(recruitmentTL,lhparms[["Linf"]],lowerSL)
  iCheckSlotType(cfBelow,cfIn,cfAbove,recruitmentTL,strict=TRUE)
  iChecklabel(label)

  # Setup data.frame of input values (varying cm, the rest constant)
  res <- expand.grid(lowerSL=lowerSL,upperSL=upperSL,
                     cfBelow=cfBelow,cfIn=cfIn,cfAbove=cfAbove,
                     cm=cm)

  # Send each row to yprBH_slot_func() ... so calc yield et al for all combos
  res <- purrr::pmap_df(res,yprBH_slot_func,lhparms=lhparms,
                        loi=loi,recruitmentTL=recruitmentTL,
                        matchRicker=matchRicker)

  # Optionally create a column with label
  if (!is.null(label)) res$label <- label

  # Return result
  return(res)
}


