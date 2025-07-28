#' @title Main function to simulate expected yield using the Dynamic Pool model for a range of input parameters, including minimum length limits for harvest
#'
#' @description Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model using ranges of values for conditional fishing mortality (\code{cf}), conditional natural mortality (\code{cm}), and minimum length limits for harvest (\code{minLL}).
#'
#' @inheritParams dpmBH_func
#' @param simyears A single numeric for the lower limit of minimum length limit for harvest in mm.
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
#' lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,
#'             LWalpha=-5.2147,LWbeta=3.153)
#' simyears <- 50
#' minLL <- 400
#' rec <- fixedRec(Nrec = 100, simyears = simyears)
#' cm <- rep(0.18,(lhparms$tmax+1))
#' cf <- c(rep(0,3), rep(0.33,(lhparms$tmax+1) - 3))
#'
#' out<-dpmBH(simyears = simyears, minLL = minLL, cf = cf, cm = cm,
#'       rec = rec, lhparms = lhparms, matchRicker=FALSE)
#'
#' @rdname dpmBH
#' @export

dpmBH <- function(simyears,minLL,cf,cm,rec,lhparms,matchRicker=FALSE){

  # ---- Check inputs
  # iCheckMLH(lengthmin,"minimum")
  # iCheckMLH(lengthmax,"maximum")
  # minLL <- iCheckMLHinc(lengthinc,lengthmin,lengthmax)
  # iCheckcf(cfmin,"minimum")
  # iCheckcf(cfmax,"maximum")
  # cf <- iCheckcfminc(cfinc,cfmin,cfmax)
  # iCheckcm(cmmin,"minimum")
  # iCheckcf(cmmax,"maximum")
  # cm <- iCheckcfminc(cminc,cmmin,cmmax)
  # iCheckN0(N0)    # initial check if vector/list
  # if (length(N0)>1) {
  #   Linf <- N0[["Linf"]]
  #   K <- N0[["K"]]
  #   t0 <- N0[["t0"]]
  #   LWalpha <- N0[["LWalpha"]]
  #   LWbeta <- N0[["LWbeta"]]
  #   tmax <- N0[["tmax"]]
  #   N0 <- N0[["N0"]]
  #   iCheckN0(N0)  # second check of single value of N0
  # }
  # iCheckLinf(Linf)
  # iCheckK(K)
  # iCheckt0(t0)
  # iCheckLWa(LWalpha)
  # iCheckLWb(LWbeta)
  # iChecktmax(tmax)

  res<-dpmBH_func(minLL = minLL, cf = cf, cm= cm, rec = rec[1], lhparms = lhparms,matchRicker=FALSE)
  yearsum<-data.frame(year= seq(1:nrow(res)), yc = rep(1,length(seq(1:nrow(res)))))
  res<-cbind(yearsum,res)

  for(x in 2:simyears){
    out<-dpmBH_func(minLL = minLL, cf = cf, cm= cm, rec = rec[x], lhparms = lhparms,matchRicker=FALSE)
    yearsum<-data.frame(year= x:(nrow(out)+x-1), yc = rep(x,length(x:(nrow(out)+x-1))))
    out<-cbind(yearsum,out)

    res<-rbind(res,out)

  }

  res2<-subset(res,res$year<=simyears)


  # ---- Return data.frame with both output values and input parameters
  res
}
