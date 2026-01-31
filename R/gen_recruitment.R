#' @title Generate a vector of number of recruits for the dynamic pool model.
#'
#' @description This function is used to generate number of recruits across multiple years using different random functions.
#'
#' @param method A single string to call the method of generating a vector of recruits. `fixed` generates recruitment based on a fixed value for each year of simyears, `uniform` generates recruitment based on random values from a uniform distribution for each year of simyears, `normal` generates recruitment based on random values from a normal distribution for each year of simyears, `StrYC_Nth` generates recruitment based on a strong year class every Nth year, and `StrYC_randInt` generates recruitment based on a strong year classes at random intervals.
#' @param simyears A single numeric that sets the number of years to simulate recruitment
#' @param nR A single numeric that sets the fixed number of recruitment. Used when `method="fixed"`.
#' @param minR A single numeric that sets the minimum number of recruits during simulations. Used when `method="uniform"` or `method="normal"`.
#' @param maxR A single numeric that sets the maximum number of recruits during simulations. Used when `method="uniform"` or `method="normal"`.
#' @param meanR A single numeric that sets the mean number of recruits. Used when `method="normal"`, `method="StrYC_Nth"`, or `method="StrYC_randInt"`.
#' @param sdR A single numeric that sets the standard deviation of number of recruits. Used when `method="normal"`.
#' @param nStr A single numeric that sets the Nth year that a strong year class will occur. Used when `method="StrYC_Nth"`.
#' @param sizeStr A single numeric that sets the multiplier for the strong year class relative to meanR. Used when `method="StrYC_Nth"`, or `method="StrYC_randInt"`.
#' @param avgFreq A single numeric that sets the average frequency of a strong year class. Used when `method="StrYC_randInt"`.
#'
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#'
#' @returns A vector that contains the number of recruits for each simulation that can be used directly in the dynamic pool model (e.g., \code{\link{dpmBH}}).
#'
#' @examples
#' # Genearte recruits based on a fixed number
#' genRecruits("fixed",nR=50)
#'
#' # Generate recruits from a uniform distribution bound by 25 and 75
#' genRecruits("uniform",minR=25,maxR=75)
#'
#' # Generate recruits based on a normal distribution with a mean of 50,
#' # standard deviation of 10, and trucated to be between 25 and 75
#' genRecruits("normal",minR=25,maxR=75,meanR=50,sdR=10)
#'
#' # Geneate recruits based on a fixed mean recruit number of 50 and a
#' # strong year class every 5 years with recruits 2 times the mean recruits
#' genRecruits("StrYC_Nth",meanR=50,sizeStr=2,nStr=5)
#'
#' # Generate recruits based on a fixed mean recruit number of 50 and a
#' # strong year class at random intervals of size 2 times the mean recruits
#' # with the random interval averaging every 5 years.
#' genRecruits("StrYC_randInt",meanR=50,sizeStr=2,avgFreq=5)
#'
#' @rdname genRecruits
#' @export
#'

genRecruits <- function(method=c("fixed", "uniform", "normal", "StrYC_Nth", "StrYC_randInt"), simyears = 50,
                        nR = NULL, minR = NULL, maxR = NULL, meanR = NULL, sdR = NULL,
                        nStr = NULL, sizeStr = NULL, avgFreq = NULL){

  meths <- c("fixed", "uniform", "normal", "StrYC_Nth", "StrYC_randInt")
  if (is.null(method)) STOP("'method' must be one of ",paste(meths, collapse=", "))

  iChecksimyears(simyears)

  if (method == "fixed"){
    iChecknR(nR)

    rec <- rep(x = nR, times = simyears)

  }else if (method == "uniform"){
    iCheckminR(minR)
    iCheckmaxR(maxR)

    rec <- round(stats::runif(n = simyears, min = minR, max = maxR))

  }else if (method == "normal"){
    iCheckminRNorm(minR)
    iCheckmaxRNorm(maxR)
    iCheckmeanR(meanR)
    iChecksdR(sdR)

    rec <- round(stats::rnorm(n = simyears, mean = meanR, sd = sdR))
    rec[rec < minR] <- minR
    rec[rec > maxR] <- maxR

  }else if (method == "StrYC_Nth"){
    iCheckmeanRNth(meanR)
    iChecknStr(nStr)
    iChecksizeStr(sizeStr)

    rec <- rep(c(rep(meanR, (nStr - 1)), (meanR * sizeStr)), (simyears/nStr))

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

