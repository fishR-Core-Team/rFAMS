#' @title Generate a vector of recruitment abundance for the dynamic pool model.
#'
#' @description These function is used to generate recruitment abundances across multiple years using different random function.
#'
#' @param method A single string to call the method of generating a vector of recruits. `fixed` generate recruitment based on a fixed value for each year of simyears, `uniform` generates recruitment based on random values from a unifrom distribution for each year of simyears, `normal` generates recruitment based on random values from a unifrom distribution for each year of simyears, `StrYC_Nth` generates recruitment based on a strong year class every Nth year, and `StrYC_randInt` generates recruitment based on a strong year classes at random intervals.
#' @param simyears A single numeric that sets the number of years to simulate recruitment
#' @param Nrec A single numeric that sets the fixed number of recruitment
#' @param MinR A single numeric that sets the minimum recruitment abundance during simulations.
#' @param MaxR A single numeric that sets the maximum recruitment abundance during simulations.
#' @param meanR A single numeric that sets the mean recruitment abundance.
#' @param sdR A single numeric that sets the standard deviation of recruitment abundance
#' @param Nthyr A single numeric that sets the Nth year that a strong year class will occur
#' @param sizeStr A single numeric that sets the multiplier for the strong year class relative to meanR
#' @param avgFreq A single numeric that sets the average frequency of a strong year class.
#'
#' @details This function is used internally and not generally used interactively
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#'
#' @returns A vector that contains the given recruitment options that can be used directly in the dynamic pool model (e.g., \code{\link{dpmBH}}).
#'
#' @examples
#' # To be filled out later
#'
#' @rdname genRecruits
#' @export
#'

genRecruits <- function(method=c("fixed", "uniform", "normal", "StrYC_Nth", "StrYC_randInt"), simyears = 50,
                        Nrec = NULL, MinR = NULL, MaxR = NULL, meanR = NULL, sdR = NULL,
                        Nthyr = NULL, sizeStr = NULL, avgFreq = NULL){
  iChecksimyears(simyears)

  if (method == "fixed"){
    iCheckNrec(Nrec)
    iCheckNrec(Nrec)

    rec <- rep(x = Nrec, times = simyears)

  }else if (method == "uniform"){
    iCheckMinR(MinR)
    iCheckMaxR(MaxR)

    rec <- round(stats::runif(n = simyears, min = MinR, max = MaxR))

  }else if (method == "normal"){
    iCheckMinRNorm(MinR)
    iCheckMaxRNorm(MaxR)
    iCheckmeanR(meanR)
    iChecksdR(sdR)

    rec <- round(stats::rnorm(n = simyears, mean = meanR, sd = sdR))
    rec[rec < MinR] <- MinR
    rec[rec > MaxR] <- MaxR

  }else if (method == "StrYC_Nth"){
    iCheckmeanRNth(meanR)
    iCheckNthyr(Nthyr)
    iChecksizeStr(sizeStr)

    rec <- rep(c(rep(meanR, (Nthyr - 1)), (meanR * sizeStr)), (simyears/Nthyr))

  }else if (method == "StrYC_randInt"){
    iCheckmeanRrandInt(meanR)
    iCheckavgFreq(avgFreq)
    iChecksizeStrRrandInt(sizeStr)

    rec <- stats::rbinom(simyears,1,(1/avgFreq)) + 1
    rec[rec == 1] <- meanR
    rec[rec == 2] <- meanR * sizeStr

  }else {
    STOP("Please select a valid recruitment method.")
  }
 return(rec)
}

#'
#' #Fixed
#' fixedRec <- function(simyears = 50, Nrec = 1000){
#'   rec <- rep(x = Nrec, times = simyears)
#'   rec
#' }
#'
#'
#' #' @rdname recruit_functions
#' #' @export
#' #Random - Uniform
#' unifRec <- function(simyears = 50, MinR = 10, MaxR = 1000){
#'   rec <- round(stats::runif(n = simyears, min = MinR, max = MaxR))
#'   rec
#' }
#'
#' #' @rdname recruit_functions
#' #' @export
#' #Random - Normal
#' rnormRec <- function(simyears = 50, meanR = 1000, sdR = 500, MinR = 100, MaxR = 2500){
#'   rec <- round(stats::rnorm(n = simyears, mean = meanR, sd = sdR))
#'   rec[rec < MinR] <- MinR
#'   rec[rec > MaxR] <- MaxR
#'
#'   rec
#' }
#'
#' #' @rdname recruit_functions
#' #' @export
#' #Strong year-class every Nth year
#' StrYC_Nth <- function(simyears = 50, meanR = 1000, Nthyr = 5, sizeStr = 2){
#'   rec <- rep(c(rep(meanR, (Nthyr - 1)), (meanR * sizeStr)), (simyears/Nthyr))
#'   rec
#' }
#'
#' #' @rdname recruit_functions
#' #' @export
#' #Strong year-class at random intervals
#' StrYC_randInt <- function(simyears = 50, meanR = 1000, avgFreq = 5, sizeStr = 2){
#'   rec <- stats::rbinom(simyears,1,(1/avgFreq)) + 1
#'   rec[rec == 1] <- meanR
#'   rec[rec == 2] <- meanR * sizeStr
#'   rec
#' }
