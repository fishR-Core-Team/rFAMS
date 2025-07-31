#' @title Function to calculate static spawning potential ratio
#'
#' @description Calculates the static spawning potential ratio with the yield-per-recruit model
#'
#' @param Linf A single numeric that represents the point estimate of asymptotic mean length from the von Bertalanffy growth model OR an `nls` object created from fitting the von Bertalanffy equation to length-at-age data.
#' @param K A single numeric that represents the point estimate of the Brody growth coefficient from the von Bertalanffy growth model.
#' @param t0 A single numeric that represents the point estimate of the x-intercept (i.e., theoretical age at a mean length of 0) from the von Bertalanffy growth model.
#' @param FLR A single character to indicate the fecundity-length relationship; either "linear" or "log10".
#' @param FLRint A single numeric that represents the intercept of the fecundity-length relationship.
#' @param FLRslope A single numeric that represents the slope of the fecundity-length relationship.
#' @param MatAge A single integer that represents the age at maturity.
#' @param percF A numeric vector representing the percentage of females at each age, starting at age-1.
#' @param percFSpawn A numeric vector representing the percentage of spawning females at each age, starting at age-1.
#' @param M A single numeric that represents natural mortality.
#' @param F A single numeric that represents fishing mortality.
#' @param MUnder A single numeric that represents natural mortality under the slot limit.
#' @param MIn A single numeric that represents natural mortality within the slot limit.
#' @param MAbove A single numeric that represents natural mortality above the slot limit.
#' @param FUnder A single numeric that represents fishing mortality under the slot limit.
#' @param FIn A single numeric that represents fishing mortality within the slot limit.
#' @param FAbove A single numeric that represents fishing mortality above the slot limit.
#' @param tmax A single numeric that represents the maximum age of fish.
#' @param minLL A single numeric that represents the minimum length limit for harvestable size
#' @param slot A boolean indicating if a slot limit is used (TRUE) or if a minmium length limit is used (FALSE)
#' @param recruitmentTL A single numeric that represents the total length (mm) fish are recruited to the fishery (i.e., will be harvested)
#' @param lowerSL A single numeric that represents the total length (mm) of the lower limit of the slot limit
#' @param upperSL A single numeric that represents the total length (mm) of the upper limits of the slot limit
#'
#' @details This function is generally not used independently. It is called when requesting the spawning potential ratio while calculating yield-per-recruit.
#'
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' # Estimate spawning potential ratio for fixed parameters with a single minimum length limit
#' spr_1 <- static_spr(Linf = 1349.5, K = 0.111, t0 = 0.065, FLR = "linear", FLRint = -1057029,
#'                     FLRslope = 2777.08, MatAge = 4,
#'                     percF = c(0,0,0,rep(0.50,27)),
#'                     percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
#'                     M = 0.198, F = 0.105, tmax = 30, slot = FALSE, minLL = 250)
#' spr_1
#'
#'#' # Estimate spawning potential ratio for fixed parameters with a slot limit
#' spr_2 <- static_spr(Linf = 1349.5, K = 0.111, t0 = 0.065, FLR = "linear", FLRint = -1057029,
#'                     FLRslope = 2777.08, MatAge = 4,
#'                     percF = c(0,0,0,rep(0.50,27)),
#'                     percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
#'                     MUnder = 0.35667, MIn = 0.35667, MAbove = 0.35667,
#'                     FUnder = 0.28768, FIn = 0.91629, FAbove = 0.16252,
#'                     tmax = 30, slot = TRUE, recruitmentTL = 200, lowerSL = 300, upperSL = 400)
#' spr_2
#' @rdname static_spr
#' @export

static_spr <- function(Linf, K, t0, FLR, FLRint, FLRslope,MatAge, percF, percFSpawn, M = NULL, F = NULL,
                       MUnder = NULL, MIn = NULL, MAbove = NULL, FUnder = NULL, FIn = NULL, FAbove = NULL,
                       tmax, slot=FALSE, minLL = NULL, recruitmentTL = NULL, lowerSL = NULL, upperSL = NULL){

 #generate length at age and fecundity at age
 ages <- 1:tmax
 l_at_age <- Linf * (1 - exp(-K * (ages - t0)))
 if(FLR == "linear"){
  fec_at_age <- FLRint + l_at_age * FLRslope
 }else {
  fec_at_age <- exp(FLRint + log(l_at_age) * FLRslope)
 }
 fec_at_age[1:MatAge] <- 0

#Build fishing and natural mortality vector based on length at age.
#If called with minimum length limit, build vector with F starting at age where expected mean length at start of year is > minimum length limit.
#If called with slot limit, build vector of F below, within, or above slot based on expected mean length at age at start of year.
if(slot == FALSE){
   F_vect<-c()
   for(x in 1:length(l_at_age)){
     if(l_at_age[x] < minLL ){
       F_vect <- c(F_vect,0)
     }else{
       F_vect <- c(F_vect,F)
     }
   }
   M_vect<-rep(M,length(F_vect))
}else{
  F_vect<-c()
  for(x in 1:length(l_at_age)){
    if(l_at_age[x] < recruitmentTL ){
      F_vect <- c(F_vect,0)
    }else if(l_at_age[x] < lowerSL){
      F_vect <- c(F_vect,FUnder)
    }else if(l_at_age[x] < upperSL){
      F_vect <- c(F_vect,FIn)
    }else{
      F_vect <- c(F_vect,FAbove)
    }
  }
  M_vect<-rep(MUnder,length(F_vect))
}

 #calculate vector of survival with and withing fishing mortality
 S_exp <- exp(-(F_vect + M_vect))
 S_no_exp<-exp(-(M_vect))

 #Calculate potential recruitment fecundity with and without fishing mortality
 P <- sum(sapply(1:length(fec_at_age), function(i) {
   Ei <- fec_at_age[i] * percF[i] * percFSpawn[i]
   survivals <- S_exp[1:(i-1)]  # Get survival from age 0 to i-1
   prod_survival <- prod(survivals)
   Ei * prod_survival
 }))

 Punfished <- sum(sapply(1:length(fec_at_age), function(i) {
   Ei <- fec_at_age[i] * percF[i] * percFSpawn[i]
   survivals <- S_no_exp[1:(i-1)] # Get survival from age 0 to i-1
   prod_survival <- prod(survivals)
   Ei * prod_survival
 }))

 spr<-P/Punfished

 #return spr
 spr
}
