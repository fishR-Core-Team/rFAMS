#' @title Main function to simulate expected yield using the Dynamic Pool model for a range of input parameters, including minimum length limits for harvest
#'
#' @description Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model using ranges of values for conditional fishing mortality (\code{cf}), conditional natural mortality (\code{cm}), and minimum length limits for harvest (\code{minLL}).
#'
#' @param simyears A single numeric for the lower limit of minimum length limit for harvest in mm.
#' @param minLL A single numeric representing the minimum length limit for harvest in mm.
#' @param cf A matrix of conditional fishing mortality where each row represents a year and each column represents age. Ages are age-0 through maximum age.
#' @param cm A matrix of conditional natural mortality where each row represents a year and each column represents age. Ages are age-0 through maximum age.
#' @param rec A A numeric vector of length `simyears` to specify recruitment each year. The vector can be geneated using the `genRecruits()` function.
#' @param lhparms A named vector or list that contains values for each `N0`, `tmax`, `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See \code{\link{makeLH}} for definitions of these life history parameters. Also see details.
#' @param matchRicker A logical that indicates whether the yield function should match that in Ricker (). Defaults to \code{TRUE}. The only reason to changed to \code{FALSE} is to try to match output from FAMS. See the "YPR_FAMSvRICKER" article.
#' @param species is a single character to specify the species used in the simulation and will define the length for `stock`, `quality`, `preferred`, `memorable`, and `trophy`. Length categories are obtained from the FSA package, see the \code{\link[FSA]{PSDlit}} documentation.
#' @param group is a single character to specify the sub-group name of a species used in the simulation and will define the length for `stock`, `quality`, `preferred`, `memorable`, and `trophy`. Length categories are obtained from the FSA package, see the \code{\link[FSA]{PSDlit}} documentation.
#'
#' @details Details
#'
#' @return  A list with two data.frame object. The first list item contains a data.frame with the following calculated values in a summary by age:
#' \itemize{
#' \item \code{year} is the year number for the simulation
#' \item \code{yc}is the year class number for the simulation
#' \item \code{age} is the age of fish from the year class
#' \item \code{length} is the length at age at the beginning of the year based on parameters supplied for the von Bertlanffy growth model.
#' \item \code{weight} is the total weight at the beginning of the year for length at age based on the parameters supplied for the weight-length model.
#' \item \code{N_start} is the number of fish alive at the start of the year for the given age and year class.
#' \item \code{explitation} is the exploitation rate at age based on the supplied conditional fishing mortality rate.
#' \item \code{expect_nat_death} is the expectation of natural death based on the supplied conditional natural mortality rate.
#' \item \code{cf} is the supplied conditional fishing mortality rate.
#' \item \code{cm} is the supplied conditional natural mortality rate.
#' \item \code{F} is the instantaneous rate of fishing mortality.
#' \item \code{M} is the instantaneous rate of natural mortality.
#' \item \code{Z} is the instantaneous rate of total mortality.
#' \item \code{S} is the (total) annual rate of survival.
#' \item \code{biomass} is the total biomass of fish at age and year
#' \item \code{N_harvest} is the total number of fish harvested at age and year
#' \item \code{N_die} is the total number of fish that die at age and year
#' \item \code{yield} is the estimated yield (in g).
#' \item \code{minLL} is the minimum length limit specified in the simulation
#' \item \code{N0} is the number of initial
#' }
#'
#' For convenience the data.frame also contains the model input values (\code{minLL}, \code{N0}, \code{N0}, \code{Linf}, \code{K}, \code{t0}, \code{LWalpha}, \code{LWbeta}, and \code{tmax}).
#'
#' The data.frame also contains a \code{notes} value which may contain abbreviations for "issues" that occurred when computing the results and were adjusted for. The possible abbreviates are as follows:
#'
#' \itemize{
#' \item \code{minLL>=Linf}: The minimum length limit (minLL) being explored was greater than the given asymptotic mean length (Linf). For the purpose (only) of computing the time at recruitment to the fishery (tr) the Linf was set to minLL+0.1.
#' \item \code{tr<t0}: The age at recruitment to the fishery (tr) was less than the hypothetical time when the mean length is zero (t0). The fish can't recruit to the fishery prior to having length 0 so tr was set to t0. This also assures that the time it takes to recruit to the fishery is greater than 0.
#' \item \code{Y=Infinite}: The calculated yield (Y) was inifinity, which is impossible and suggests some other propblem. Yield was set to NA.
#' \item \code{Y<0}: The calculated yield (Y) was negative, which is impossible. Yield was set to 0.
#' \item \code{Nharv<0}: The calculated number of fish harvested (Nharv) was negative, which is not possible. Number harvested was set to 0.
#' \item \code{Ndie<0}: The calculated number of fish recruiting to the fishery that died naturally (Ndie) was negative, which is not possible. Number that died was set to 0.
#' \item \code{agvglen<minLL}: The average length of harvested fish was less than the given minimum length limit being explored, which is not possible (with only legal harvest). The average length was set to the minimum length limit.
#' }
#'
#' The second list item contains a data.frame with the following calculated values in a summary by year:
#' \itemize{
#' \item \code{year} is the year number for the simulation
#' \item \code{substock} is the number of substock sized fish at age and year at the beginning of the year.
#' \item \code{stock} is the number of stock sized fish at age and year at the beginning of the year.
#' \item \code{quality} is the number of quality sized fish at age and year at the beginning of the year.
#' \item \code{preferred} is the number of preferred sized fish at age and year at the beginning of the year.
#' \item \code{memorable} is the number of memorable sized fish at age and year at the beginning of the year.
#' \item \code{trophy} is the number of trophy sized fish at age and year at the beginning of the year.
#' \item \code{PSD} is the number of quality sized fish divided by the number of stock sized multiplied by 100.
#' \item \code{PSD_P} is the number of preferred sized fish divided by the number of stock sized multiplied by 100.
#' \item \code{PSD_M} is the number of memorable sized fish divided by the number of stock sized multiplied by 100.
#' \item \code{PSD_T} is the number of trophy sized fish divided by the number of stock sized multiplied by 100.
#' \item \code{Age_1plus} is the total number of fish age-1 plus per year.
#' \item \code{Yield_Age_1plus} is the total year of age-1 plus fish per year.
#' \item \code{Total_biomass} is the total biomass of age-1 plus fish per year.
#' \item \code{N_harvest_Age_1plus} is the number of age-1 plus fish that are harvested per year.
#' \item \code{N_die_Age_1plus} is the nubmer of age-1 plus fish that die per year.
#' }
#'
#' PSD-X are calculated based on the number of fish in each category (`stock`, `quality`, `preferred`, `memorable`, and `trophy`) at the beginning of the year. That is, the length at age during the start of the year is used to assign PSD-X categories at age. For example, if Quality size is 300mm, an age-1 fish at 275mm at the start of the year would not be counted as a quality sized fish, but an age-2 fish at 325mm at the start of the year would be counted as a quality sized fish.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @seealso \code{\link{yprBH_func}} for estimating yield from single values of \code{cf}, \code{cm}, and \code{minLL}, and \code{\link{yprBH_minLL_fixed}} for simulating yield with multiple values of \code{cf} and \code{cm} but a fixed value for \code{minLL}.
#'
#' @examples
#' #load required library
#' library(dplyr)
#' library(ggplot2)
#'
#'# Setting a custom theme for plots (to make look nice)
#' # Optional for plotting
#' theme_FAMS <- function(...) {
#'   theme_bw() +
#'     theme(
#'       panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#'       axis.text=element_text(size=14,color="black"),
#'       axis.title=element_text(size=16,color="black"),
#'       axis.title.y=element_text(angle=90),
#'       axis.line=element_line(color="black"),
#'       panel.border=element_blank()
#'     )
#' }
#'
#' # Example of simulating yield with the dynamic pool model,
#'
#' lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,
#'             LWalpha=-5.2147,LWbeta=3.153)
#' simyears <- 50
#' minLL <- 400
#' rec <- genRecruits(method = "fixed", Nrec = 100, simyears = simyears)
#' cm <- matrix(rep(c(rep(0,1), rep(0.18,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
#' cf <- matrix(rep(c(rep(0,1), rep(0.33,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
#'
#' out<-dpmBH(simyears = simyears, minLL = minLL, cf = cf, cm = cm, rec = rec, lhparms = lhparms,
#'            matchRicker=FALSE,species="Striped Bass",group="landlocked")
#'
#' #Use summary by year data frame to plot PSD vs year
#' ggplot(data=out[[2]],mapping=aes(x=year,y=PSD)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="PSD",x="Year") +
#'   theme_FAMS()
#'
#' #Use summary by year data frame to plot yield vs year
#' ggplot(data=out[[2]],mapping=aes(x=year,y=Yield_age_1plus)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Total yield (g)",x="Year") +
#'   theme_FAMS()
#'
#' #Plot date using summary by age
#' #filter for year class = 1
#' plotdat<- out[[1]] |> filter(yc==1)
#' #Plot yield vs age
#' ggplot(data=plotdat,mapping=aes(x=age,y=yield)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Total yield (g)",x="Age") +
#'   theme_FAMS()
#'
#' #Plot Number harvested vs age
#' ggplot(data=plotdat,mapping=aes(x=age,y=N_harvest)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Number harvested",x="Age") +
#'   theme_FAMS()
#'
#'
#' #Recruitment based on a normal distribution
#' rec <- genRecruits(method = "normal", simyears = simyears,
#'                    meanR = 1000, sdR = 500, MinR = 100, MaxR =2500)
#' cm <- matrix(rep(c(rep(0,1), rep(0.18,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
#' cf <- matrix(rep(c(rep(0,1), rep(0.33,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
#'
#' out_2<-dpmBH(simyears = simyears, minLL = minLL, cf = cf, cm = cm, rec = rec, lhparms = lhparms,
#'              matchRicker=FALSE,species="Striped Bass",group="landlocked")
#'
#' #Use summary by year data frame to plot PSD vs year
#' ggplot(data=out_2[[2]],mapping=aes(x=year,y=PSD)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="PSD",x="Year") +
#'   theme_FAMS()
#'
#' #Use summary by year data frame to plot yield vs year
#' ggplot(data=out_2[[2]],mapping=aes(x=year,y=Yield_age_1plus)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Total yield (g)",x="Year") +
#'   theme_FAMS()
#'
#' #Plot date using summary by age
#' #Plot yield vs age for each year class
#' ggplot(data=out_2[[1]],mapping=aes(x=age,y=yield,group=yc,color=yc)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Total yield (g)",x="Age") +
#'   theme_FAMS()
#'
#' #Plot Number harvested vs age
#' ggplot(data=out_2[[1]],mapping=aes(x=age,y=N_harvest,group=yc,color=yc)) +
#'   geom_point() +
#'   geom_line() +
#'   labs(y="Number harvested",x="Age") +
#'   theme_FAMS()
#'
#'
#' @rdname dpmBH
#' @export

dpmBH <- function(simyears,minLL,cf,cm,rec,lhparms,matchRicker=FALSE,species=NULL, group=NULL){

  # ---- Check inputs
  iCheckMLH(minLL)
  iCheckLLinf(minLL,lhparms$Linf)
  iCheckspecies(species)
  iChecksimyears(simyears)
  iCheckcfcm_dpm(cf)
  iCheckcfcm_dpm(cm)
  iCheckrec(rec)


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

  res <- list(res,isum_by_year(res,species=species,group=group))
  # ---- Return data.frame with both output values and input parameters.
  # ---- Contains a summary by age and summary by year
  return(res)
}
