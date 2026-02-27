#' @title Simulate expected yield using the Beverton-Holt Yield-per-Recruit model for a range of input parameters, including minimum length limits for harvest
#'
#' @description Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model using ranges of values for conditional fishing mortality (\code{cf}), conditional natural mortality (\code{cm}), and minimum length limits for harvest (\code{minLL}).
#'
#' @param minLL A numeric vector of minimum length limits.
#' @param cf A numeric vector of conditional fishing mortality.
#' @param cm A numeric vector of conditional natural mortality.
#' @param lhparms A named vector or list that contains values for each `N0`, `tmax`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See \code{\link{makeLH}} for definitions of these life history parameters. Also see details.
#' @param loi A numeric vector for lengths of interest. Used to determine number of fish that reach desired lengths.
#' @param matchRicker A logical that indicates whether the yield function should match that in Ricker (1975). Defaults to \code{TRUE}. The only reason to changed to \code{FALSE} is to try to match output from FAMS. See the \href{https://fishr-core-team.github.io/rFAMS/articles/YPR_FAMSvRICKER.html}{FAMS vs Ricker article}.
#'
#' @details Details will be filled out later
#'
#' @return  A data.frame with the following calculated values:
#' \itemize{
#' \item \code{yield} is the estimated yield (in g).
#' \item \code{exploitation} is the exploitation rate.
#' \item \code{Nharvest} is the number of harvested fish.
#' \item \code{Ndie} is the number of fish that die of natural deaths.
#' \item \code{Nt} is the number of fish at time tr (time they become harvestable size).
#' \item \code{avgwt} is the average weight of fish harvested.
#' \item \code{avglen} is the average length of fish harvested.
#' \item \code{tr} is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery).
#' \item \code{nAtxxx} is the number that reach the length of interest supplied. There will be one column for each length of interest.
#' \item \code{F} is the instantaneous rate of fishing mortality.
#' \item \code{M} is the instantaneous rate of natural mortality.
#' \item \code{Z} is the instantaneous rate of total mortality.
#' \item \code{S} is the (total) annual rate of survival.
#' }
#'
#' For convenience the data.frame also contains the model input values (\code{minLL}, \code{cf}, and\code{cm} from input vectors; \code{N0}; \code{Linf}; \code{K}; \code{t0}; \code{LWalpha}; \code{LWbeta}; and \code{tmax}).
#'
#' The data.frame also contains a \code{notes} value which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are defined under "values" in the documentation for \code{\link{yprBH_func}}.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @seealso \code{\link{yprBH_func}} for estimating yield from single values of \code{cf}, \code{cm}, and \code{minLL} for simulating yield with multiple values of \code{cf} and \code{cm} but a fixed value for \code{minLL}.
#'
#'See \href{https://fishr-core-team.github.io/rFAMS/articles/YPR_MinLL.html}{this demonstration page} for more plotting examples
#'
#' @examples
#' # Load other required packages for organizing output and plotting
#' library(dplyr)    ## for filter
#' library(ggplot2)  ## for ggplot et al.
#' library(metR)     ## geom_contour2
#'
#' # Life history parameters to be used below
#' LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
#'
#' # Estimate yield for multiple values of minLL, cf, and cm
#' # # This is a minimal example, increments for minLL, cf, and cm would likely be smaller
#' # #   to produce finer-scaled results.
#'
#' minLL <- seq(from = 200, to = 550, by = 50)
#' cf <- seq(from = 0.1, to = 0.9, by = 0.1)
#' cm <- seq(from = 0.1, to = 0.9, by = 0.1)
#' loi <- c(400,450,500,550)
#'
#' Res_1 <- yprBH_MinLL(minLL = minLL, cf = cf, cm = cm,
#'                      lhparms=LH, loi=loi)
#'
#' # Yield curves (yield vs exploitation) by varying minimum lengths,
#' # using cm=40
#' plot_dat <- Res_1 |> filter(cm==0.40)
#'
#' ggplot(data=plot_dat,mapping=aes(y=yield,x=exploitation,
#'                                  group=minLL,color=minLL)) +
#'   geom_line(linewidth=1) +
#'   scale_color_gradient2(high="black") +
#'   xlab("Exploitation (u)")+
#'   ylab("Yield (g)")+
#'   labs(color="Min Length Limit") +
#'   theme_bw()
#'
#' # Yield isopleths for varying minLL and exploitation with cm=0.40
#' # # Using same data as previous example
#' ggplot(data=plot_dat,mapping=aes(x=exploitation,y=minLL,z=yield)) +
#'   geom_contour2(aes(label = after_stat(level))) +
#'   xlab("Exploitation (u)") +
#'   ylab("Minimum length limit (mm)") +
#'   theme_bw()
#'
#' @rdname yprBH_MinLL
#' @export

yprBH_MinLL <- function(minLL,cf,cm,lhparms,loi=NULL,matchRicker=FALSE){
  # ---- Check inputs
  iCheckLHparms(lhparms,"lhparms")
  iCheckMLH(minLL,"minLL")
  iCheckLLinf(minLL,lhparms$Linf)
  iCheckCondMort(cf,"cf")
  iCheckCondMort(cm,"cm")
  iCheckloi(loi,"loi")

  # ---- Needed to account for rounding issues of sequences
  minLL <- round(minLL,8)
  cf <- round(cf,8)
  cm <- round(cm,8)

  # ---- Compute Yield et al. for varying minLL, cf, and cm
  # Setup data.frame of input values from minLL, cf, and cm vectors
  res <- expand.grid(minLL=minLL,cf=cf,cm=cm)

  # Send each row to yprBH_func() ...
  #   i.e., calculate yield et al for all minLL, cf, and cm combos
  res <- purrr::pmap_df(res,yprBH_func,lhparms=lhparms,loi=loi,matchRicker=matchRicker)

  # ---- Return data.frame with both output values and input parameters
  res
}
