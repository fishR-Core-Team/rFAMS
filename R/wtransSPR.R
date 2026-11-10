#' @title Function to calculate weighted transitional spawning potential ratio
#'
#' @description An INTERNAL function used by \code{\link{yprBH_MinLL}} and \code{\link{yprBH_SlotLL}} to calculate weighted transitional spawning potential ratio with the dynamic pool model
#'
#' @param df A data frame containing results from the dynamic pool model by age.
#' @param SPRdat A named list that contains values for each `FLR`, `FLRint`, `FLRslope`, and `MatAge`. See \code{\link{makeSPR}} for definitions of these parameters.
#'
#' @details This function is generally not used independently. It is called when requesting the spawning potential ratio while calculating yield-per-recruit.
#'
#'
#' @return A vector
#' \itemize{
#' \item `wtSPR` is a vector containing weighted transitional spawning potential ratio for each year of the simulation
#' }
#'
#'
#' @author Derek Ogle, \email{jason.doll@fmarion.edu}
#' @author Jason C. Doll, \email{DerekOgle51@gmail.com}
#'
#'
#' @keywords internal
#' @export

wtrans_spr <- function(df,SPRdat) {

  df2 <- df |>
    dplyr::filter(age != 0) |>
    dplyr::mutate(
      Fec_a = iMakeFecundity(FLR=SPRdat$FLR, FLRint=SPRdat$FLRint, FLRslope=SPRdat$FLRslope, t=age,L_t=length, MatAge=SPRdat$MatAge),
      percF_a = SPRdat$percF[age],
      percFSpawn_a = SPRdat$percFSpawn[age],
    ) |>
    dplyr::group_by(yc) |>
    dplyr::arrange(age, .by_group = TRUE) |>
    dplyr::mutate(S_noexp = cumprod(exp(-M)),
           S_exp = cumprod(exp(-Z)),
           Eggs_noexp = nstart * percF_a * percFSpawn_a * Fec_a * S_noexp,
           Eggs_exp = nstart * percF_a * percFSpawn_a * Fec_a * S_exp) |>
    dplyr::group_by(year) |>
    dplyr::summarize(totaleggs_noexp = sum(Eggs_noexp),
           totaleggs_exp = sum(Eggs_exp),
           wtSPR = totaleggs_exp / totaleggs_noexp,
           wtSPR = dplyr::coalesce(wtSPR,0))


  return(df2$wtSPR)

}
