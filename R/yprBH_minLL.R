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
#' Best practice is to use \code{\link{makeMort}} to create the data.frame for `morts` because it (i) allows the user to enter mortality values in a variety of formats with the other values being calculated, (ii) allows for creating multiple mortality values of a type in a variety of ways, and (iii) performs sanity checks on the entered values (e.g., M>0).; i.e.,
#'
#' ```R
#' # Best practice for creating mortality data.frame (an example)
#' MORTS <- makeMort("cfcm",cf=0.3,cm=0.5)
#'
#' # Works but no checks on the values (and is a pain)
#' MORTS <- data.frame(cf=0.3,cm=0.5,F=03566749,M=0.6931472,Z=1.049822,
#'                     S=0.35,A=0.65,u=02208362)
#' ```
#'
#' @param minLL A single numeric for the minimum length limit for harvest in mm. If `minLLmax` and `minLLinc` are given then this will be the minimum minimum length limit for a sequence from `minLL` to `minLLmax` in increments of `minLLinc`.
#' @param morts A data.frame that contains values for each `cf`, `cm`, `F`, `M`, `Z`, `S`, `A`, and `u`. See \code{\link{makeMort}} for definitions of these parameters. Also see details.
#' @param lhparms A named vector or list that contains values for each `N0`, `maxage`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See \code{\link{makeLH}} for definitions of these life history parameters. Also see details.
#' @param minLLmax A single numeric for the upper limit of the sequence of minimum length limit for harvest in mm. See `minLL` above.
#' @param minLLinc A single numeric for the increment of the sequence of minimum length limit for harvest in mm. See `minLL` above.
#' @param matchRicker A logical that indicates whether the yield function should match that in Ricker (1975). Defaults to `TRUE`. The only reason to changed to `FALSE` is to try to match output from FAMS. See the "YPR_FAMSvRICKER" article.
#'
#' @return A data.frame with the following calculated values:
#'
#' * `yield` is the estimated yield (in g).
#' * `Nharvest` is the number of harvested fish.
#' * `Ndie` is the number of fish that die of natural deaths.
#' * `Nt` is the number of fish at time tr (time they become harvestable size).
#' * `avgwt` is the average weight of fish harvested.
#' * `avglen` is the average length of fish harvested.
#' * `tr` is the time for a fish to recruit to a minimum length limit (i.e., time to enter fishery).
#'
#' For convenience the data.frame also contains the input values of `minLL`, `F`, and `M`, as well as the following values calculated directly from `F` and `M`:
#'
#' * `Z` is the total instantaneous mortality rate (=F+M).
#' * `cf` is the conditional fishing mortality rate (=1-exp(-F)).
#' * `cm` is the conditional natural mortality rate (=1-exp(-M)).
#' * `u` is the exploitation rate (=A(F/Z)).
#' * `A` is the annual total mortality rate (=1-exp(-Z)).
#' * `S` is the annual total survival rate (=exp(-Z))
#'
#' Also for convenience, the input values for the life history parameters are included in the data.frame (i.e., `N0`, `maxage`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`).
#'
#' Finally, the data.frame also contains a `notes` column which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are:
#'
#' * `minLL>=Linf`: The minimum length limit (minLL) being explored was greater than the given asymptotic mean length (Linf). For the purpose (only) of computing the time at recruitment to the fishery (tr) the Linf was set to minLL+0.1.
#' * `tr<t0`: The age at recruitment to the fishery (tr) was less than the hypothetical time when the mean length is zero (t0). The fish can't recruit to the fishery prior to having length 0 so tr was set to t0. This also assures that the time it takes to recruit to the fishery is greater than 0.
#' * `Nt<0`: The number of fish recruiting to the fishery was less than 0. There cannot be negative fish, so Nt was then set to 0.
#' * `Nt>N0`: The number of fish recruiting to the fishery was more than the number of fish recruited to the populations. Fish cannot be added to the cohort, so Nt was set to N0.
#' * `Y=Infinite`: The calculated yield (Y) was inifinity, which is impossible and suggests some other propblem. Yield was set to NA.
#' * `Y<0`: The calculated yield (Y) was negative, which is impossible. Yield was set to 0.
#' * `Nharv<0`: The calculated number of fish harvested (Nharv) was negative, which is not possible. Number harvested was set to 0.
#' * `Nharv>Nt`: The calculated number of fish harvested (Nharv) was greater than the number of fish recruiting to the fishery, which is impossible. The number harvested was set to the number recruiting to the fishery.
#' * `Ndie<0`: The calculated number of fish recruiting to the fishery that died naturally (Ndie) was negative, which is not possible. Number that died was set to 0.
#' * `Ndie>Nt`: The calculated number of fish recruiting to the fishery that died naturally (Ndie) was greater than the number of fish recruiting to the fishery, which is impossible. The number that died was set to the number recruiting to the fishery.
#' * `agvglen<minLL`: The average length of harvested fish was less than the given minimum length limit being explored, which is not possible (with only legal harvest). The average length was set to the minimum length limit.
#'
#' Finally, for convenience the data.frame also contains the model input values (or values derived from those values as given in `morts` and `lhparms`): `minLL`, `cf`, `cm`, `F`, `M`, `Z`, `A`, `S`, `u`, `N0`, `maxage`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @seealso \code{\link{yprBH_minLL_1}}
#'
#' @references
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada. Was (is?) from \url{https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/1485.pdf}.
#'
#' Slipke, J.W., and M.J. Maceina. 2014. Fishery analysis and modeling simulator. v1.64. American Fisheries Society, Bethesda, MD. Was (is?) from \url{https://units.fisheries.org/fits/wp-content/uploads/sites/29/2019/06/FAMS-1.64-Manual.pdf}.
#'
#' @examples
#' # Life history parameters to be used below
#' LH <- makeLH(N0=100,maxage=15,Linf=600,K=0.30,t0=-0.6,LWalpha=-5.453,LWbeta=3.10)
#'
#' # --- Simple Examples -------------------------------------------------------
#' # Estimate yield for single values of minLL, cf, and cm
#' MORTS <- makeMort(cf=0.3,cm=0.5)
#' res <- yprBH_minLL(minLL=200,morts=MORTS,lhparms=LH)
#' res
#'
#' # Estimate yield for multiple values of cf and cm ... minLL constant
#' # # This is a minimal ex, inc values should be smaller for finer-scaled results
#' MORTS <- makeMort(cf=0.3,cfmax=0.7,cfinc=0.2,cm=0.5,cmmax=0.9,cminc=0.2)
#' res <- yprBH_minLL(minLL=200,morts=MORTS,lhparms=LH)
#' res
#'
#' # Estimate yield for multiple values of minLL, cf, and cm
#' # # This is a minimal ex, inc values should be smaller for finer-scaled results
#' MORTS <- makeMort(cf=0.3,cfmax=0.7,cfinc=0.2,cm=0.5,cmmax=0.9,cminc=0.2)
#' res <- yprBH_minLL(minLL=200,minLLmax=500,minLLinc=100,morts=MORTS,lhparms=LH)
#' res
#'
#' # Estimate yield for multiple values of minLL, cf, and cm ... all not sequence
#' MORTS <- makeMort(cf=c(0.3,0.7),cm=c(0.5,0.9))
#' res <- yprBH_minLL(minLL=c(200,250,400),morts=MORTS,lhparms=LH)
#' res
#'
#' # --- More Realist Example (with Plots) -------------------------------------
#' # More realistic example to show possible plots
#' MORTS <- makeMort(cf=0.1,cfmax=0.9,cfinc=0.1,cm=0.1,cmmax=0.9,cminc=0.1)
#' res <- yprBH_minLL(minLL=200,minLLmax=500,minLLinc=10,morts=MORTS,lhparms=LH)
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
#' ggplot(data=plot_dat,mapping=aes(x=u,y=yield)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Yield (g)",x="Exploitation") +
#'   theme_FAMS()
#'
#' # Yield curves by varying minimum lengths, using cm=40
#' plot_dat <- res %>% filter(cm==0.40)
#'
#' ggplot(data=plot_dat,mapping=aes(y=yield,x=u,group=minLL,color=minLL)) +
#'   geom_line(linewidth=1) +
#'   scale_color_gradient2(high="black") +
#'   labs(y="Yield (g)",x="Exploitation",color="Min Length Limit") +
#'   theme_FAMS()
#'
#' # Yield isopleths for varying minLL and exploitation with cm=0.40
#' # # Using same data as previous example
#' ggplot(data=plot_dat,mapping=aes(x=u,y=minLL,z=yield)) +
#'   geom_contour_filled(alpha=0.5) +
#'   geom_contour(color="black") +
#'   metR::geom_text_contour(stroke=0.15) +
#'   xlab("Exploitation") +
#'   ylab("Minimum length limit (mm)") +
#'   theme_FAMS() +
#'   theme(legend.position="none")
#'
#' # Same as previous but using number harvested isopleths
#' ggplot(data=plot_dat,mapping=aes(x=u,y=minLL,z=Nharvest)) +
#'   geom_contour_filled(alpha=0.5) +
#'   geom_contour(color="black")+
#'   metR::geom_text_contour(stroke = 0.15)+
#'   xlab("Exploitation")+
#'   ylab("Minimum length limit (mm)")+
#'   theme_FAMS() +
#'   theme(legend.position="none")
#'
#' @rdname yprBH_minLL
#' @export

yprBH_minLL <- function(minLL,morts,lhparms,
                        minLLmax=NULL,minLLinc=NULL,matchRicker=TRUE){
  #===== Internal checking and calculation function
  iGetMinLL <- function(minLL,minLLmax,minLLinc) {
    if (length(minLL)>1) {
      # cf vector provided by the user ... just do some checks
      if (!is.numeric(minLL)) STOP("'minLL' must be numeric.")
      if (any(minLL<0)) STOP("All values in 'minLL' must be >=0")
    } else {
      # cf needs to be created as a sequence ... after some checks
      iCheckMLH(minLL)
      if (!is.null(minLLmax)) {
        if (is.null(minLLinc))
          STOP("if 'minLLmax' is given then 'minLLinc' must also be given.")
        iCheckMLH(minLLmax,minmax="maximum",check_missing=FALSE)
        minLL <- iCheckMLHinc(minLLinc,minLL,minLLmax,check_missing=FALSE)
      }
    }
    minLL
  }

  # ---- Check inputs
  # morts and lhparms were checked in makeMort and makeLH
  minLL <- iGetMinLL(minLL,minLLmax,minLLinc)

  # ---- Compute Yield et al. for varying minLL and morts
  # Setup data.frame of input values
  minLLtmp <- data.frame(minLL=rep(minLL,each=nrow(morts)))
  mortstmp <- morts[rep(seq_len(nrow(morts)),times=length(minLL)),]
  rownames(mortstmp) <- seq_len(nrow(mortstmp))
  inputs <- do.call(cbind,list(minLLtmp,mortstmp))

  # Send each row to yprBH_minLL_1
  res <- purrr::pmap_df(inputs,yprBH_minLL_1,lhparms=lhparms,matchRicker=matchRicker)

  # ---- Return data.frame with both output values and input parameters
  res
}
