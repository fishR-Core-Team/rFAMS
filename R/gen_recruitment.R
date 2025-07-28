#' @title Generate a vector of recruitment abundance for the dynamic pool model.
#'
#' @description These function is used to generate recruitment abundances across multiple years using different random function.
#'
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
#' @returns A named list that contains the given reproduction options that can be used directly in the yield-per-recruit calculation functions (e.g., \code{\link{yprBH_SlotLL}}).
#'
#' @examples
#' # This example creates a list where fish mature at age-4. Leading 0's are used in
#' # the vectors for percF and percFSpawn because they do not contribute to eggs produced
#'
#' SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
#'                  percF=c(0,0,0,rep(0.50,27)),
#'                  percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)))
#'
#' @name recruit_functions

#' @rdname recruit_functions
#' @export
#Generate initial recruits
#Fixed
fixedRec <- function(simyears = 50, Nrec = 1000){
  rec <- rep(x = Nrec, times = simyears)
  rec
}


#' @rdname recruit_functions
#' @export
#Random - Uniform
unifRec <- function(simyears = 50, MinR = 10, MaxR = 1000){
  rec <- round(stats::runif(n = simyears, min = MinR, max = MaxR))
  rec
}

#' @rdname recruit_functions
#' @export
#Random - Normal
rnormRec <- function(simyears = 50, meanR = 1000, sdR = 500, MinR = 100, MaxR = 2500){
  rec <- round(stats::rnorm(n = simyears, mean = meanR, sd = sdR))
  rec[rec < MinR] <- MinR
  rec[rec > MaxR] <- MaxR

  rec
}

#' @rdname recruit_functions
#' @export
#Strong year-class every Nth year
StrYC_Nth <- function(simyears = 50, meanR = 1000, Nthyr = 5, sizeStr = 2){
  rec <- rep(c(rep(meanR, (Nthyr - 1)), (meanR * sizeStr)), (simyears/Nthyr))
  rec
}

#' @rdname recruit_functions
#' @export
#Strong year-class at random intervals
StrYC_randInt <- function(simyears = 50, meanR = 1000, avgFreq = 5, sizeStr = 2){
  rec <- stats::rbinom(simyears,1,(1/avgFreq)) + 1
  rec[rec == 1] <- meanR
  rec[rec == 2] <- meanR * sizeStr
  rec
}
