#' @title Main function to simulate expected yield using the Beverton-Holt Yield-per-Recruit model for a range of input parameters, including minimum length limits for harvest
#'
#' @description Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model using ranges of values for conditional fishing mortality (\code{cf}), conditional natural mortality (\code{cm}), and minimum length limits for harvest (\code{minLL}).
#'
#' @inheritParams yprBH_func
#' @param lengthmin A single numeric for the lower limit of minimum length limit for harvest in mm.
#' @param lengthmax A single numeric for the upper limit of minimum length limit for harvest in mm.
#' @param lengthinc A single numeric for the increment to cycle from lower to upper minimum length limit for harvest in mm.
#' @param cfmin A single numeric for minimum conditional fishing mortality.
#' @param cfmax A single numeric for maximum conditional fishing mortality.
#' @param cfinc A single numeric for increment to cycle from minimum to maximum conditional fishing mortality.
#' @param cmmin A single numeric for minimum conditional natural mortality.
#' @param cmmax A single numeric for maximum conditional natural mortality.
#' @param cminc A single numeric for increment to cycle from minimum to maximum conditional natural mortality.
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
#' \item \code{F} is the instantaneous rate of fishing mortality.
#' \item \code{M} is the instantaneous rate of natural mortality.
#' \item \code{Z} is the instantaneous rate of total mortality.
#' \item \code{S} is the (total) annual rate of survival.
#' }
#'
#' For convenience the data.frame also contains the model input values (\code{minLL} derived from \code{lengthmin}, \code{lengthmax}, and \code{lengthinc}; \code{cf} derived from \code{cfmin}, \code{cfmax}, and \code{cfinc}; \code{cm} derived from \code{cmmin}, \code{cmmax}, and \code{cminc}; \code{N0}; \code{Linf}; \code{K}; \code{t0}; \code{LWalpha}; \code{LWbeta}; and \code{tmax}).
#'
#' The data.frame also contains a \code{notes} value which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are defined under "values" in the documentation for \code{\link{yprBH_func}}.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @seealso \code{\link{yprBH_func}} for estimating yield from single values of \code{cf}, \code{cm}, and \code{minLL}, and \code{\link{yprBH_minLL_fixed}} for simulating yield with multiple values of \code{cf} and \code{cm} but a fixed value for \code{minLL}.
#'
#' @examples
#' # Life history parameters to be used below
#' LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
#'
#' # Estimate yield for multiple values of minLL, cf, and cm
#' # # This is a minimal example, lengthinc, cfinc, cminc would likely be smaller
#' # #   to produce finer-scaled results
#' Res_1 <- yprBH_minLL_var(lengthmin=200,lengthinc=50,lengthmax=550,
#'                        cfmin=0.1,cfmax=0.9,cfinc=0.1,
#'                        cmmin=0.1,cmmax=0.9,cminc=0.1,
#'                        lhparms=LH)
#'
#' # Load other required packages for organizing output and plotting
#' library(dplyr)    ## for filter
#' library(ggplot2)  ## for ggplot et al.
#' library(metR)     ## geom_text_contour
#'
#' # Custom theme for plots (to make look nice)
#' theme_FAMS <- function(...) {
#'   theme_bw() +
#'   theme(
#'     panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#'     axis.text=element_text(size=14,color="black"),
#'     axis.title=element_text(size=16,color="black"),
#'     axis.title.y=element_text(angle=90),
#'     axis.line=element_line(color="black"),
#'     panel.border=element_blank()
#'   )
#' }
#'
#' # Yield curve (yield vs exploitation)
#' # Extract results for cm=0.40 and minimum length limit=400
#' plot_dat <- Res_1 |> dplyr::filter(cm==0.40,minLL==400)
#'
#' ggplot(data=plot_dat,mapping=aes(x=u,y=yield)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Yield (g)",x="Exploitation (u)") +
#'   theme_FAMS()
#'
#' # Yield curves by varying minimum lengths, using cm=40
#' plot_dat <- Res_1 |> filter(cm==0.40)
#'
#' ggplot(data=plot_dat,mapping=aes(y=yield,x=u,
#'                                  group=minLL,color=minLL)) +
#'   geom_line(linewidth=1) +
#'   scale_color_gradient2(high="black") +
#'   labs(y="Yield (g)",x="Exploitation (u)",color="Min Length Limit") +
#'   theme_FAMS()
#'
#' # Yield isopleths for varying minLL and exploitation with cm=0.40
#' # # Using same data as previous example
#' ggplot(data=plot_dat,mapping=aes(x=u,y=minLL,z=yield)) +
#'   geom_contour(color="black") +
#'   metR::geom_text_contour(stroke=0.15) +
#'   xlab("Exploitation (u)") +
#'   ylab("Minimum length limit (mm)") +
#'   theme_FAMS()
#'
#' # Same as previous but using number harvested isopleths
#' ggplot(data=plot_dat,mapping=aes(x=u,y=minLL,z=Nharvest)) +
#'   geom_contour(color="black")+
#'   metR::geom_text_contour(stroke = 0.15)+
#'   xlab("Exploitation (u)")+
#'   ylab("Minimum length limit (mm)")+
#'   theme_FAMS()
#'
#'
#' @rdname yprBH_minLL_var
#' @export

yprBH_minLL_var <- function(lengthmin,lengthmax,lengthinc,
                          cfmin,cfmax,cfinc,
                          cmmin,cmmax,cminc,
                          lhparms,matchRicker=FALSE){
  # ---- Check inputs
  iCheckMLH(lengthmin,"minimum")
  iCheckMLH(lengthmax,"maximum")
  minLL <- iCheckMLHinc(lengthinc,lengthmin,lengthmax)
  iCheckLLinf(lengthmin,lhparms$Linf)
  iCheckLLinf(lengthmax,lhparms$Linf)
  iCheckcf(cfmin,"minimum")
  iCheckcf(cfmax,"maximum")
  cf <- iCheckcfminc(cfinc,cfmin,cfmax)
  iCheckcm(cmmin,"minimum")
  iCheckcm(cmmax,"maximum")
  cm <- iCheckcfminc(cminc,cmmin,cmmax)


  # ---- Compute Yield et al. for varying minLL, cf, and cm
  # Setup data.frame of input values ... minLL, cf, and cm sequences were
  #   created in checks above
  res <- expand.grid(minLL=minLL,cf=cf,cm=cm)

  # Send each row to yprBH_func() ...
  #   i.e., calculate yield et al for all minLL, cf, and cm combos
  res <- purrr::pmap_df(res,yprBH_func,matchRicker=matchRicker,lhparms=lhparms)


  # ---- Return data.frame with both output values and input parameters
  res
}
