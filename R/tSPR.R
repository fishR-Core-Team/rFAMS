#' @title Function to calculate transitional spawning potential ratio
#'
#' @description Calculates the unweighted and weighted transitional spawning potential ratio with the dynamic pool model
#'
#' @param dproutput A dataframe containing outupt from the dynamic pool model
#' @param SPRdat A named list that contains values for each `FLR`, `FLRint`, `FLRslope`, `MatAge`, `percF`, and `percFSpawn`. See \code{\link{makeSPR}} for definitions of these parameters. Also see details.
#'
#' @details This function is generally not used independently. It is called when requesting the spawning potential ratio while calculating yield-per-recruit.
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' # Estimate spawning potential ratio for fixed parameters with a single minimum length limit
#' #spr_1 <- static_spr(Linf = 1349.5, K = 0.111, t0 = 0.065, FLR = "linear", FLRint = -1057029,
#' #                    FLRslope = 2777.08, MatAge = 4,
#' #                    percF = c(0,0,0,rep(0.50,27)),
#' #                    percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
#' #                    M = 0.198, F = 0.105, tmax = 30, slot = FALSE, minLL = 250)
#' #spr_1
#'
#'#' # Estimate spawning potential ratio for fixed parameters with a slot limit
#' #spr_2 <- static_spr(Linf = 1349.5, K = 0.111, t0 = 0.065, FLR = "linear", FLRint = -1057029,
#' #                     FLRslope = 2777.08, MatAge = 4,
#' #                     percF = c(0,0,0,rep(0.50,27)),
#' #                     percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
#' #                     MUnder = 0.35667, MIn = 0.35667, MAbove = 0.35667,
#' #                     FUnder = 0.28768, FIn = 0.91629, FAbove = 0.16252,
#' #                     tmax = 30, slot = TRUE, recruitmentTL = 200, lowerSL = 300, upperSL = 400)
#' # spr_2
#' @rdname t_spr
#' @export

t_spr <- function(dproutput = NULL, SPRdat = NULL){

  year <- dproutput$yc
  Age <- dproutput$Age
  Linf <- dproutput$Linf
  K <- dproutput$K
  t0 <- dproutput$t0


  unw_spr<-c(0) #To start with first year of age-0
  w_spr <- c(0) #To start with first year of age-0
 #Cycle through each year
 for(x in 2:max(dproutput$year)){

   #subset for one year at a time
   yeardf<-dproutput |>
     dplyr::filter(year == x & Age > 0) |>
     dplyr::arrange(Age) |>
     dplyr::mutate(l_at_age = Linf * (1 - exp(-K * (Age - t0)))) |>
     dplyr::mutate(dplyr::across(c(l_at_age), ~ if_else(. < 0, 0, .)))

   #  yeardf$FLRint<--1057029
   # yeardf$FLRslope <- 2777.08

   if(SPRdat$FLR == "linear"){
     yeardf$fec_at_age <- SPRdat$FLRint + yeardf$l_at_age * SPRdat$FLRslope
   }else {
     yeardf$fec_at_age <- exp(SPRdat$FLRint + log(yeardf$l_at_age) * SPRdat$FLRslope)
   }
   yeardf <- yeardf |>
     dplyr::mutate(fec_at_age=ifelse(Age<SPRdat$MatAge,0,fec_at_age))

  #Build fishing and natural mortality vector based on length at age.
  #If called with minimum length limit, build vector with F starting at age where expected mean length at start of year is > minimum length limit.
  #If called with slot limit, build vector of F below, within, or above slot based on expected mean length at age at start of year.
    F_vect <- yeardf$F
    M_vect <- yeardf$M
    S_exp <- yeardf$S

   #calculate vector of survival with and withing fishing mortality
   #S_exp <- exp(-(F_vect + M_vect))
   S_no_exp<-exp(-(M_vect))

   #abundance <- sum(yeardf$N_start) - yeardf$N_start[1]
   #N_recruit <- sum(yeardf$N_start[5:30])

      #Calculate potential recruitment fecundity with and without fishing mortality
   P <- sum(sapply(1:length(yeardf$fec_at_age), function(i) {
     Ei <- yeardf$fec_at_age[i] * SPRdat$percF[i] * SPRdat$percFSpawn[i] #*yeardf$N_start[i]
     survivals <- S_exp[1:(i-1)]  # Get survival from age 1 to i-1
     prod_survival <- prod(survivals)
     Ei * prod_survival
   }))

   Punfished <- sum(sapply(1:length(yeardf$fec_at_age), function(i) {
     Ei <- yeardf$fec_at_age[i] * SPRdat$percF[i] * SPRdat$percFSpawn[i] #* yeardf$N_start[i]
     survivals <- S_no_exp[1:(i-1)] # Get survival from age 1 to i-1
     prod_survival <- prod(survivals)
     Ei * prod_survival
   }))


   Pwgth <- sum(sapply(1:length(yeardf$fec_at_age), function(i) {
     Ei <- yeardf$fec_at_age[i] * SPRdat$percF[i] * SPRdat$percFSpawn[i]
     survivals <- S_exp[1:(i-1)]  # Get survival from age 1 to i-1
     prod_survival <- prod(survivals)
     Ei * prod_survival * dproutput$Nt[i]
   }))

   Punfishedwgth <- sum(sapply(1:length(yeardf$fec_at_age), function(i) {
     Ei <- yeardf$fec_at_age[i] * SPRdat$percF[i] * SPRdat$percFSpawn[i]
     survivals <- S_no_exp[1:(i-1)] # Get survival from age 1 to i-1
     prod_survival <- prod(survivals)
     Ei * prod_survival * dproutput$Nt[i]
   }))


   if(Punfished <= 0 | is.na(Punfished) | is.nan(Punfished)){
     unw_spr <- c(unw_spr,0)
     w_spr <- c(w_spr,0)
   }else{
     unw_spr<-c(unw_spr,P/Punfished)
     w_spr<-c(w_spr,Pwgth/Punfishedwgth)
   }



   #unw_spr
 }
  #return spr
  tspr <- data.frame(unw_spr= unw_spr, w_spr = w_spr)
  tspr
}
