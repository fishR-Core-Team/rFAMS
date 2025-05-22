#' @title Simulate expected yield using the Beverton-Holt Yield-per-Recruit model for a range of input parameters
#'
#' @description Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model using a range of values for conditional fishing (\code{cf}) and natural (\code{cm}) mortality and a single fixed minimum length limit for harvest (\code{minLL}).
#'
#' @inheritParams ypr_func
#' @param minLL The minimum length limit for harvest in mm
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
#' \item \code{Fmort} is the instantaneous rate of fishing mortality.
#' \item \code{Mmort} is the instantaneous rate of natural mortality.
#' \item \code{Zmort} is the instantaneous rate of total mortality.
#' \item \code{S} is the (total) annual rate of survival.
#' }
#'
#' For convenience the data.frame also contains the model input values (\code{minLL}; \code{cf} derived from \code{cfmin}, \code{cfmax}, and \code{cfinc}; \code{cm} derived from \code{cmmin}, \code{cmmax}, and \code{cminc}; \code{N0}; \code{Linf}; \code{K}; \code{t0}; \code{LWalpha}; \code{LWbeta}; and \code{maxage}).
#'
#' The data.frame also contains a \code{notes} value which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are defined under "values" in the documentation for \code{\link{ypr_func}}.
#'
#' @seealso \code{\link{ypr_func}} for estimating yield from single values of \code{cf}, \code{cm}, and \code{minLL}, and \code{\link{ypr_minLL_var}} for simulating yield with multiple values of \code{cf}, \code{cm}, and \code{minLL}.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' # Life history parameters to be used below
#' parms <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,
#'            LWalpha=-5.453,LWbeta=3.10,maxage=15)
#'
#' # Estimate yield for multiple values of minLL, cf, and cm
#' # # This is a minimal example, lengthinc, cfinc, cminc would likely be smaller
#' # #   to produce finer-scaled results
#' Res_1 <- ypr_minLL_fixed(minLL=200,
#'                          cfmin=0.1,cfmax=0.9,cfinc=0.1,
#'                          cmmin=0.1,cmmax=0.9,cminc=0.1,
#'                          N0=parms)
#'
#' # Load other required packages for organizing output and plotting
#' library(dplyr)    ## for filter %>%
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
#' # Extract results for cm=0.40
#' plot_dat <- Res_1 %>% dplyr::filter(cm==0.40)
#'
#' ggplot(data=plot_dat,mapping=aes(x=exploitation,y=yield)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Yield (g)",x="Exploitation") +
#'   theme_FAMS()
#'

#' @rdname ypr_minLL_fixed
#' @export
ypr_minLL_fixed<-function(minLL,cfmin,cfmax,cfinc,cmmin,cmmax,cminc,
                          N0,Linf,K,t0,LWalpha,LWbeta,maxage,
                          matchRicker=TRUE){

  # ---- Check inputs
  iCheckMLH(minLL)
  iCheckcf(cfmin,"minimum")
  iCheckcf(cfmax,"maximum")
  cf <- iCheckcfminc(cfinc,cfmin,cfmax)
  iCheckcm(cmmin,"minimum")
  iCheckcf(cmmax,"maximum")
  cm <- iCheckcfminc(cminc,cmmin,cmmax)
  iCheckN0(N0)
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

  # ---- Compute Yield et al. for varying minLL, cf, and cm
  # Setup data.frame of input values ... cf, and cm sequences were
  #   created in checks above
  res <- expand.grid(minLL=minLL,cf=cf,cm=cm,
                     N0=N0,Linf=Linf,K=K,t0=t0,
                     LWalpha=LWalpha,LWbeta=LWbeta,maxage=maxage)

  # Send each row to ypr_func() ...
  #   i.e., calculate yield et al for all cf, and cm combos
  res <- purrr::pmap_df(res,ypr_func,matchRicker=matchRicker)

  # ---- Return data.frame with both output values and input parameters
  res
}
