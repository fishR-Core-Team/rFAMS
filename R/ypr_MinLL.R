#' @title Simulate expected yield using the Beverton-Holt Yield-per-Recruit model for a range of input parameters
#'
#' @description Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model with user-supplied inputs for minimum length limits for harvest (\code{minLL}), conditional fishing mortality (\code{cf}), and conditional natural mortality (\code{cm}). Any of these three inputs may be varied, alone or with the other inputs, to estimate yield under varying scenarios of minimum length limits, conditional fishing mortality, and conditional natural mortality.
#'
#' @details Best practice is to enter the life history parameters into a list or vector using \code{\link{makeLH}} (rather than directly) as `makeLH` performs several sanity checks on the values entered (e.g., ensures Linf>0); i.e.,
#'
#' ```R
#' # Best practice for entering life history parameter values
#' LH <- makeLH(N0=100,maxage=15,Linf=600,K=0.30,t0=-0.6,
#'              LWalpha=-5.453,LWbeta=3.10)
#'
#' # Works but no checks on the values
#' LH <- list(N0=100,maxage=15,Linf=600,K=0.30,t0=-0.6,
#'            LWalpha=-5.453,LWbeta=3.10)
#' ```
#'
#' @inheritParams ypr_func
#' @param minLL A single numeric for the minimum length limit for harvest in mm. If \code{minLLmax} and \code{minLLinc} are given then this will be the minimum minimum length limit for a sequence from \code{minLL} to \code{minLLmax} in increments of \code{minLLinc}.
#' @param cf A single numeric for conditional fishing mortality. If \code{cfmax} and \code{cfinc} are given then this will be the minimum conditional fishing mortality for a sequence from \code{cf} to \code{cfmax} in increments of \code{cfinc}.
#' @param cm A single numeric for conditional natural mortality. If \code{cmmax} and \code{cminc} are given then this will be the minimum conditional natural mortality for a sequence from \code{cm} to \code{cmmax} in increments of \code{cminc}.
#' @param minLLmax A single numeric for the upper limit of the sequence of minimum length limit for harvest in mm. See \code{minLL} above.
#' @param minLLinc A single numeric for the increment of the sequence of minimum length limit for harvest in mm. See \code{minLL} above.
#' @param lhparms A named vector or list that contains values for each \code{N0}, \code{maxage}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, and \code{LWbeta}. See \code{\link{makeLH}} for definitions of these life history parameters. Also see details.
#' @param cfmax A single numeric for the upper limit of the sequence of maximum conditional fishing mortality. See \code{cf} above.
#' @param cfinc A single numeric for increment to cycle from minimum to maximum conditional fishing mortality. See \code{cf} above.
#' @param cmmax A single numeric for the upper limit of the sequence of maximum conditional natural mortality. See \code{cf} above.
#' @param cminc A single numeric for the increment of the sequence of conditional natural mortality. See \code{cf} above.
#'
#' @return  A data.frame with the following calculated values:
#'
#' * `yield` is the estimated yield (in g).
#' * `exploitation` is the exploitation rate.
#' * `Nharvest` is the number of harvested fish.
#' * `Ndie` is the number of fish that die of natural deaths.
#' * `Nt` is the number of fish at time tr (time they become harvestable size).
#' * `avgwt` is the average weight of fish harvested.
#' * `avglen` is the average length of fish harvested.
#' * `tr` is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery).
#' * `Fmort` is the instantaneous rate of fishing mortality.
#' * `Mmort` is the instantaneous rate of natural mortality.
#' * `Zmort` is the instantaneous rate of total mortality.
#' * `S` is the (total) annual rate of survival.
#'
#' For convenience the data.frame also contains the model input values: `minLL` (may be derived from `minLL`, `minLLmin`, and `minLLinc`); `cf` (may be derived from `cf`, `cfmax`, and `cfinc`); `cm` (may be derived from `cm`, `cmmax`, and `cminc`); `N0`; `Linf`; `K`; `t0`; `LWalpha`; `LWbeta`; and `maxage`).
#'
#' The data.frame also contains a `notes` value which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are defined under "values" in the documentation for \code{\link{ypr_func}}.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @seealso \code{\link{ypr_func}} for the details of estimating yield from single values of all model input and life history parameters.
#'
#' @examples
#' # Life history parameters to be used below
#' LH <- makeLH(N0=100,maxage=15,Linf=600,K=0.30,t0=-0.6,LWalpha=-5.453,LWbeta=3.10)
#'
#' # --- Simple Examples -------------------------------------------------------
#' # Estimate yield for single values of minLL, cf, and cm
#' res <- ypr_minLL(minLL=200,cf=0.3,cm=0.5,lhparms=LH)
#' res
#'
#' # Estimate yield for multiple values of cf and cm ... minLL constant
#' # # This is a minimal ex, inc values should be smaller for finer-scaled results
#' res <- ypr_minLL(minLL=200,cf=0.3,cm=0.5,lhparms=LH,
#'                  cfmax=0.7,cfinc=0.2,
#'                  cmmax=0.9,cminc=0.2)
#' res
#'
#' # Estimate yield for multiple values of minLL, cf, and cm
#' # # This is a minimal ex, inc values should be smaller for finer-scaled results
#' res <- ypr_minLL(minLL=200,cf=0.3,cm=0.5,lhparms=LH,
#'                  minLLmax=500,minLLinc=100,
#'                  cfmax=0.7,cfinc=0.2,
#'                  cmmax=0.9,cminc=0.2)
#' res
#'
#' # --- More Realist Example (with Plots) -------------------------------------
#' # More realistic example to show possible plots
#' res <- ypr_minLL(minLL=200,cf=0.1,cm=0.1,lhparms=LH,
#'                  minLLmax=500,minLLinc=10,
#'                  cfmax=0.9,cfinc=0.1,
#'                  cmmax=0.9,cminc=0.1)
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
#' # Extract results for cm=0.40 and minimum length limit=400
#' plot_dat <- res %>% dplyr::filter(cm==0.40,minLL==400)
#'
#' ggplot(data=plot_dat,mapping=aes(x=exploitation,y=yield)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Yield (g)",x="Exploitation") +
#'   theme_FAMS()
#'
#' # Yield curves by varying minimum lengths, using cm=40
#' plot_dat <- res %>% filter(cm==0.40)
#'
#' ggplot(data=plot_dat,mapping=aes(y=yield,x=exploitation,
#'                                  group=minLL,color=minLL)) +
#'   geom_line(linewidth=1) +
#'   scale_color_gradient2(high="black") +
#'   labs(y="Yield (g)",x="Exploitation",color="Min Length Limit") +
#'   theme_FAMS()
#'
#' # Yield isopleths for varying minLL and exploitation with cm=0.40
#' # # Using same data as previous example
#' ggplot(data=plot_dat,mapping=aes(x=exploitation,y=minLL,z=yield)) +
#'   geom_contour_filled(alpha=0.5) +
#'   geom_contour(color="black") +
#'   metR::geom_text_contour(stroke=0.15) +
#'   xlab("Exploitation") +
#'   ylab("Minimum length limit (mm)") +
#'   theme_FAMS() +
#'   theme(legend.position="none")
#'
#' # Same as previous but using number harvested isopleths
#' ggplot(data=plot_dat,mapping=aes(x=exploitation,y=minLL,z=Nharvest)) +
#'   geom_contour_filled(alpha=0.5) +
#'   geom_contour(color="black")+
#'   metR::geom_text_contour(stroke = 0.15)+
#'   xlab("Exploitation")+
#'   ylab("Minimum length limit (mm)")+
#'   theme_FAMS() +
#'   theme(legend.position="none")
#'
#' @rdname ypr_minLL
#' @export

ypr_minLL <- function(minLL,cf,cm,lhparms,
                      minLLmax=NULL,minLLinc=NULL,
                      cfmax=NULL,cfinc=NULL,
                      cmmax=NULL,cminc=NULL,
                      matchRicker=TRUE){
  # ---- Check inputs
  # Modeling inputs first ... each of minLL, cf, and cm should be a single value
  #   or a sequence when this is done, if arguments are declared properly
  iCheckMLH(minLL)
  iCheckMort(cf,typeTFM="fishing",typeIC="conditional")
  iCheckMort(cm,typeTFM="natural",typeIC="conditional")
  if (!is.null(minLLmax)) {
    if (is.null(minLLinc)) STOP("if 'minLLmax' is given then 'minLLinc' must also be given.")
    iCheckMLH(minLLmax,minmax="maximum",check_missing=FALSE)
    minLL <- iCheckMLHinc(minLLinc,minLL,minLLmax,check_missing=FALSE)
  }
  if (!is.null(cfmax)) {
    if (is.null(cfinc)) STOP("if 'cfmax' is given then 'cfinc' must also be given.")
    iCheckMort(cfmax,typeTFM="fishing",typeIC="conditional",
              minmax="maximum",check_missing=FALSE)
    cf <- iCheckMortinc(cfinc,cf,cfmax,typeTFM="fishing",typeIC="conditional",
                        check_missing=FALSE)
  }
  if (!is.null(cmmax)) {
    if (is.null(cminc)) STOP("if 'cmmax' is given then 'cminc' must also be given.")
    iCheckMort(cmmax,typeTFM="natural",typeIC="conditional",
              minmax="maximum",check_missing=FALSE)
    cm <- iCheckMortinc(cminc,cm,cmmax,typeTFM="fishing",typeIC="conditional",
                        check_missing=FALSE)
  }

  # Life history parameters next
  ## Check lhparms vector/list first
  iCheckLHparms(lhparms)
  ## Then extract individual parms and check them
  N0 <- lhparms[["N0"]]
  Linf <- lhparms[["Linf"]]
  K <- lhparms[["K"]]
  t0 <- lhparms[["t0"]]
  LWalpha <- lhparms[["LWalpha"]]
  LWbeta <- lhparms[["LWbeta"]]
  maxage <- lhparms[["maxage"]]

  # ---- Compute Yield et al. for varying minLL, cf, and cm
  # Setup data.frame of input values
  res <- tidyr::expand_grid(minLL=minLL,cf=cf,cm=cm) |> as.data.frame()

  # Send each row to ypr_func()
  res <- purrr::pmap_df(res,ypr_func,lhparms=lhparms,matchRicker=matchRicker)

  # ---- Return data.frame with both output values and input parameters
  res
}
