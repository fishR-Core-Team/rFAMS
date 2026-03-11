#' @title Generate a vector of number of recruits for the dynamic pool model.
#'
#' @description This function is used to generate number of recruits across multiple years using different random functions.
#'
#' @param simyears A single numeric that sets the number of years to simulate recruitment
#' @param method A single string to call the method of generating a vector of recruits. `fixed` generates recruitment based on a fixed value for each year of simyears, `uniform` generates recruitment based on random values from a uniform distribution for each year of simyears, `normal` generates recruitment based on random values from a normal distribution for each year of simyears, `StrYC_Nth` generates recruitment based on a strong year class every Nth year, and `StrYC_randInt` generates recruitment based on a strong year classes at random intervals.
#' @param nR A single numeric that sets the fixed number of recruitment. Used when `method="fixed"` or `method="StrYC_Nth"`.
#' @param minR A single numeric that sets the minimum number of recruits during simulations. Used when `method="uniform"` or `method="normal"`.
#' @param maxR A single numeric that sets the maximum number of recruits during simulations. Used when `method="uniform"` or `method="normal"`.
#' @param meanR A single numeric that sets the mean number of recruits. Used when `method="normal"` or `method="StrYC_randInt"`.
#' @param sdR A single numeric that sets the standard deviation of number of recruits. Used when `method="normal"`.
#' @param nStr A single numeric that sets the Nth year that a strong year class will occur. Used when `method="StrYC_Nth"`.
#' @param sizeStr A single numeric that sets the multiplier for the strong year class relative to meanR. Used when `method="StrYC_Nth"`, or `method="StrYC_randInt"`.
#' @param avgFreq A single numeric that sets the average frequency of a strong year class. Used when `method="StrYC_randInt"`.
#' @param x Object saved from `genRecruits`.
#' @param \dots Optional arguments for `print`.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @returns A vector that contains the number of recruits for each simulation that can be used directly in the dynamic pool model (e.g., \code{\link{dpmBH_MinLL}}).
#'
#' @examples
#' # Generate recruits for 20 years based on a fixed number
#' rec <- genRecruits(simyears=20,method="fixed",nR=50)
#' rec
#'
#' # Generate recruits for 20 years from a uniform distribution bound
#' # by 25 and 75
#' rec <- genRecruits(simyears=20,method="uniform",minR=25,maxR=75)
#' rec
#'
#' # Generate recruits for 20 years based on a normal distribution with a mean
#' # of 50, standard deviation of 10, and trucated to be between 25 and 75
#' rec <- genRecruits(simyears=20,method="normal",minR=25,maxR=75,meanR=50,sdR=10)
#' rec
#'
#' # Geneate recruits for 20 years based on a fixed number of recruits at 50 and
#' # a strong year class every 5 years with recruits 2 times the mean recruits
#' rec <- genRecruits(simyears=20,method="StrYC_Nth",nR=50,sizeStr=2,nStr=5)
#' rec
#'
#' # Generate recruits for 20 years based on a fixed number of recruits at 50
#' # and a strong year class at random intervals of size 2 times the mean
#' # recruitswith the random interval averaging every 5 years.
#' rec <- genRecruits(simyears=20,method="StrYC_randInt",nR=50,sizeStr=2,avgFreq=5)
#' rec
#'
#' @rdname genRecruits
#' @export
genRecruits <- function(simyears,
                        method=c("fixed","uniform","normal","StrYC_Nth","StrYC_randInt"),
                        nR=NULL,minR=NULL,maxR=NULL,meanR=NULL,sdR=NULL,
                        nStr=NULL,sizeStr=NULL,avgFreq=NULL) {
  # ----- Overall checks
  method <- match.arg(method)
  iChecksimyears(simyears)

  if (method == "fixed"){
    iCheckGRarg(nR,method,isWhole="warn")
    tmp <- iHndlGRExtraArgs(c(minR=minR,maxR=maxR,meanR=meanR,sdR=sdR,
                              nStr=nStr,sizeStr=sizeStr,avgFreq=avgFreq))
    if (!is.null(tmp))
      WARN("Only use 'nr' when 'method' is '",method,"'. These values were ignored: ",tmp)

    rec <- rep(x = nR, times = simyears)

  } else if (method == "uniform"){
    iCheckGRarg(minR,method,isWhole="warn")
    iCheckGRarg(maxR,method,isWhole="warn")
    tmp <- iHndlGRExtraArgs(c(nR=nR,meanR=meanR,sdR=sdR,
                              nStr=nStr,sizeStr=sizeStr,avgFreq=avgFreq))
    if (!is.null(tmp))
      WARN("Only use 'minR' and 'maxR' when 'method' is '",method,
           "'. These values were ignored: ",tmp)

    rec <- round(stats::runif(n = simyears, min = minR, max = maxR))

  } else if (method == "normal"){
    iCheckGRarg(minR,method,isWhole="warn")
    iCheckGRarg(maxR,method,isWhole="warn")
    iCheckGRarg(meanR,method,isWhole=NULL)
    if (meanR<minR) STOP("'meanR' cannot be less than 'minR'.")
    if (meanR>maxR) STOP("'meanR' cannot be greater than 'maxR'.")
    iCheckGRarg(sdR,method,isWhole=NULL)
    tmp <- iHndlGRExtraArgs(c(nR=nR,nStr=nStr,sizeStr=sizeStr,avgFreq=avgFreq))
    if (!is.null(tmp))
      WARN("Only use 'minnr', 'maxR', 'meanR', and 'sdR' when 'method' is '",
           method,"'. These values were ignored: ",tmp)

    rec <- round(stats::rnorm(n = simyears, mean = meanR, sd = sdR))
    rec[rec < minR] <- minR
    rec[rec > maxR] <- maxR

  } else if (method == "StrYC_Nth"){
    iCheckGRarg(nR,method,isWhole="warn")
    iCheckGRarg(nStr,method,isWhole="error")
    if (nStr>simyears) STOP("'nStr' (=",nStr,") is greater than 'simyears' (=",
                            simyears,"); the results may not be what you expect.")
    iCheckGRarg(sizeStr,method,isWhole=NULL)
    tmp <- iHndlGRExtraArgs(c(minR=minR,maxR=maxR,meanR=meanR,sdR=sdR,avgFreq=avgFreq))
    if (!is.null(tmp))
      WARN("Only use 'nR', 'nStr', and 'sizeStr' when 'method' is '",method,
           "'. These values were ignored: ",tmp)

    rec <- rep(nR,times=simyears)                     # fill vector w/ nR for simyears
    rec[nStr*(1:floor(simyears/nStr))] <- nR*sizeStr  # replace every nStr w/ nR*sizeStr

  } else if (method == "StrYC_randInt"){
    iCheckGRarg(nR,method,isWhole="warn")
    iCheckGRarg(sizeStr,method,isWhole=NULL)
    iCheckGRarg(avgFreq,method,isWhole=NULL)
    if (avgFreq>simyears) WARN("'avgFreq' (=",avgFreq,") is greater than 'simyears' (=",
                               simyears,"); the results may not be what you expect.")
    tmp <- iHndlGRExtraArgs(c(minR=minR,maxR=maxR,meanR=meanR,sdR=sdR,nStr=nStr))
    if (!is.null(tmp))
      WARN("Only use 'nR', 'avgFreq', and 'sizeStr' when 'method' is '",method,"
           '. These values were ignored: ",tmp)

    rec <- stats::rbinom(simyears,1,(1/avgFreq)) + 1
    rec[rec == 1] <- nR
    rec[rec == 2] <- nR * sizeStr

  }
  class(rec) <- c("GENREC",class(rec))
  return(rec)
}

#' @rdname genRecruits
#' @export
print.GENREC <- function(x,...) {
  print(unclass(x),...)
  return(invisible(x))
}

# ===== Internal functions (only used by genRecruits)
#' Handle possibly too many arguments for a given method=
#' @noRd
iHndlGRExtraArgs <- function(x) {
  if (!is.null(x)) paste0(paste0("'",names(x),"'"),collapse=", ")
  else NULL
}

#' Handles checking any of the recruitment-related arguments in `dpmBH_MinLL`
#' @param x Any argument from `dpmBH_MinLL`
#' @param meth Method given in `dpmBH_MinLL`
#' @param isWhole A character that is `NULL` if should not check that x is a whole number or is `"warn"` or `"error"` indicating how it should respond if it is not a whole number.
#' @noRd
iCheckGRarg <- function(x,meth,isWhole) {
  nm <- iHndlArgName(deparse(substitute(x)))
  if (!is.null(x)) {
    iErrMore1(x,nm)
    iErrNotNumeric(x,nm)
    iErrLT(x,0,nm)
    if (!is.null(isWhole)) {
      if (!is.wholenumber(x)) {
        msg <- paste0("The value in ",nm," is not a whole number.")
        if (isWhole=="warn") WARN(msg)
        else STOP(msg)
      }
    }
  } else STOP("Must provide ",nm," when 'method=\"",meth,"\"'.")
}
