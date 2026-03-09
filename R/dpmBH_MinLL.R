#' @title Simulate expected yield under minimum length regulations using the Dynamic Pool model for a range of input parameters
#'
#' @description Simulate yield under minimum length regulations using the Dynamic Pool (DPM) model with (possibly) multiple values for conditional fishing mortality (`cf`) and conditional natural mortality (`cm`).
#'
#' @param minLL A single numeric representing the minimum length limit for harvest in mm.
#' @param cf A matrix of conditional fishing mortality where each row represents a year and each column represents an age (age-0 through maximum age; i.e., `tmax` in `lhparms`). All values must be between 0 and 1 (inclusive).
#' @param cm A matrix of conditional natural mortality where each row represents a year and each column represents an age (age-0 through maximum age; i.e., `tmax` in `lhparms`). All values must be between 0 and 1 (inclusive).
#' @param rec A numeric vector with length `simyears` that specifies the number of recruits each year. This vector is best generated using the \code{\link{genRecruits}}. All values must be greater than 0.
#' @param lhparms A named vector or list that contains values for each `N0`, `tmax`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See \code{\link{makeLH}} for definitions of these life history parameters. Also see details.
#' @param simyears A single numeric for the number of years to simulate. Value must be a whole number greater than 1.
#' @param species A single character to specify the species used in the simulation. This will define the length for `stock`, `quality`, `preferred`, `memorable`, and `trophy` lengths from the FSA package. See the \code{\link[FSA]{PSDlit}} documentation.
#' @param group A single character to specify the sub-group name for `species` which may be required when defining the `stock`, `quality`, `preferred`, `memorable`, and `trophy` length categories from the FSA package. See the \code{\link[FSA]{PSDlit}} documentation.
#' @param matchRicker A logical that indicates whether the yield function should match that in Ricker (1975). Defaults to `FALSE`. See the \href{https://fishr-core-team.github.io/rFAMS/articles/YPR_FAMSvRICKER.html}{FAMS vs Ricker article}.
#'
#' @return  A list with two data.frame object. The first list item named `sumbyAge` contains a data.frame with the following calculated values in a summary by age:
#'
#' \itemize{
#' \item `year` is the year number for the simulation
#' \item `yc`is the year class number for the simulation
#' \item `age` is the age of fish from the year class
#' \item `length` is the length-at-age at the beginning of the year based on parameters supplied for the von Bertlanffy growth model.
#' \item `weight` is the total weight at the beginning of the year for length-at-age based on the parameters supplied for the weight-length model.
#' \item `N_start` is the number of fish alive at the start of the year for the given age and year class.
#' \item `exploitation` is the exploitation rate at age based on the supplied conditional fishing mortality rate.
#' \item `expect_nat_death` is the expectation of natural death based on the supplied conditional natural mortality rate.
#' \item `cf` is the supplied conditional fishing mortality rate.
#' \item `cm` is the supplied conditional natural mortality rate.
#' \item `F` is the instantaneous rate of fishing mortality.
#' \item `M` is the instantaneous rate of natural mortality.
#' \item `Z` is the instantaneous rate of total mortality.
#' \item `S` is the (total) annual rate of survival.
#' \item `biomass` is the total biomass of fish at age and year.
#' \item `N_harvest` is the total number of fish harvested at age and year.
#' \item `N_die` is the total number of fish that die at age and year.
#' \item `yield` is the estimated yield (in g).
#' \item `minLL` is the minimum length limit specified in the simulation.
#' }
#'
#' For convenience the data.frame also contains the model input values (`N0`, `Linf`, `K`, `t0`, `LWalpha`, `LWbeta`, and `tmax`).
#'
#' The second list item named `sumbyYear` contains a data.frame with the following calculated values in a summary by year:
#'
#' \itemize{
#' \item `year` is the year number for the simulation
#' \item `Age_1plus` is the total number of fish age-1 plus per year.
#' \item `Yield_Age_1plus` is the total year of age-1 plus fish per year.
#' \item `Total_biomass` is the total biomass of age-1 plus fish per year.
#' \item `N_harvest_Age_1plus` is the number of age-1 plus fish that are harvested per year.
#' \item `N_die_Age_1plus` is the number of age-1 plus fish that die per year.
#' \item `substock` is the number of substock sized fish at age and year at the beginning of the year.
#' \item `stock` is the number of stock sized fish at age and year at the beginning of the year.
#' \item `quality` is the number of quality sized fish at age and year at the beginning of the year.
#' \item `preferred` is the number of preferred sized fish at age and year at the beginning of the year.
#' \item `memorable` is the number of memorable sized fish at age and year at the beginning of the year.
#' \item `trophy` is the number of trophy sized fish at age and year at the beginning of the year.
#' \item `PSD` is the number of quality sized fish divided by the number of stock sized multiplied by 100.
#' \item `PSD_P` is the number of preferred sized fish divided by the number of stock sized multiplied by 100.
#' \item `PSD_M` is the number of memorable sized fish divided by the number of stock sized multiplied by 100.
#' \item `PSD_T` is the number of trophy sized fish divided by the number of stock sized multiplied by 100.
#' }
#'
#' PSD-X are calculated based on the number of fish in each category (`stock`, `quality`, `preferred`, `memorable`, and `trophy`) at the beginning of the year. That is, the length-at-age during the start of the year is used to assign PSD-X categories at age. For example, if Quality size is 300mm, an age-1 fish at 275mm at the start of the year would not be counted as a quality-sized fish, but an age-2 fish at 325mm at the start of the year would be counted as a quality-sized fish.
#'
#' @details Details will be filled out later.
#'
#' Note that the main calculations are in the internal `dpmBH_func` (use `rFAMS:::dpmBH_func` to see that source code).
#'
#' @seealso \code{\link{yprBH_MinLL}} for estimating yield with a yield-per-recruit model using a minimum length limit and \code{\link{yprBH_SlotLL}} for estimating yield with the yield-per-recruit model and a slot limit.
#'
#' See \href{https://fishr-core-team.github.io/rFAMS/articles/dpmBH.html}{this demonstration page} for more examples of this function.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' #load required library
#' library(dplyr)
#' library(ggplot2)
#'
#' # Example of simulating yield with the dynamic pool model,
#'
#' lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,
#'             LWalpha=-5.2147,LWbeta=3.153)
#' simyears <- 50
#' minLL <- 400
#' rec <- genRecruits(method = "fixed", nR = 100, simyears = simyears)
#' cm <- matrix(rep(c(rep(0,1), rep(0.18,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
#' cf <- matrix(rep(c(rep(0,1), rep(0.33,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
#'
#' out<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf,
#'                  cm = cm, rec = rec, lhparms = lhparms,
#'                  matchRicker=FALSE,species="Striped Bass",group="landlocked")
#'
#' #Use summary by year data frame to plot yield vs year
#' ggplot(data=out[[2]],mapping=aes(x=year,y=Yield_age_1plus)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Total yield (g)",x="Year") +
#'   theme_bw()
#'
#' #Plot date using summary by age
#' #filter for year class = 1
#' plotdat<- out[[1]] |> filter(yc==1)
#' #Plot yield vs age
#' ggplot(data=plotdat,mapping=aes(x=age,y=yield)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Total yield (g)",x="Age") +
#'   theme_bw()
#'
#' #Recruitment based on a normal distribution
#' rec <- genRecruits(method = "normal", simyears = simyears,
#'                    meanR = 1000, sdR = 500, minR = 100, maxR =2500)
#' cm <- matrix(rep(c(rep(0,1), rep(0.18,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
#' cf <- matrix(rep(c(rep(0,1), rep(0.33,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
#'
#' out_2<-dpmBH_MinLL(minLL = minLL, cf = cf, cm = cm,
#'                    rec = rec, lhparms = lhparms,simyears = simyears,
#'                    species="Striped Bass",group="landlocked",matchRicker=FALSE)
#'
#' #Use summary by year data frame to plot yield vs year
#' ggplot(data=out_2[[2]],mapping=aes(x=year,y=PSD)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="PSD",x="Year") +
#'   theme_bw()
#'
#' #Plot date using summary by age
#' #Plot yield vs age for each year class
#' ggplot(data=out_2[[1]],mapping=aes(x=age,y=yield,group=yc,color=yc)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Total yield (g)",x="Age") +
#'   theme_bw()
#'
#' @rdname dpmBH_MinLL
#' @export

dpmBH_MinLL <- function(minLL,cf,cm,rec,lhparms,simyears,
                        species=NULL,group=NULL,matchRicker=FALSE){

  # ---- Check inputs
  iCheckMLH(minLL,lhparms$Linf)
  iCheckspecies(species)
  iChecksimyears(simyears)
  iCheckCondMort2(cf,simyears,lhparms$tmax,"cf")
  iCheckCondMort2(cm,simyears,lhparms$tmax,"cm")
  iCheckrec(rec)

  #needed to account for rounding issues of sequences
  cf <- round(cf,8)
  cm <- round(cm,8)

  res<-dpmBH_func(minLL = minLL, cf = cf[1,], cm= cm[1,], rec = rec[1], lhparms = lhparms,matchRicker=FALSE)
  yearsum<-data.frame(year= seq(1:nrow(res)), yc = rep(1,length(seq(1:nrow(res)))))
  res<-cbind(yearsum,res)

  for(x in 2:simyears){
    out<-dpmBH_func(minLL = minLL, cf = cf[x,], cm= cm[x,], rec = rec[x], lhparms = lhparms,matchRicker=FALSE)
    yearsum<-data.frame(year= x:(nrow(out)+x-1), yc = rep(x,length(x:(nrow(out)+x-1))))
    out<-cbind(yearsum,out)

    res<-rbind(res,out)

  }

  res<-subset(res,res$year<=simyears)

  res <- list(sumbyAge=res,sumbyYear=isum_by_year(res,species=species,group=group))
  # ---- Return data.frame with both output values and input parameters.
  # ---- Contains a summary by age and summary by year
  return(res)
}
