#' @title Function to calculate static spawning potential ratio
#'
#' @description An INTERNAL function used by \code{\link{yprBH_MinLL}} and \code{\link{yprBH_SlotLL}} to calculate static spawning potential ratio with the yield-per-recruit model
#'
#' @param tmax A single numeric that represents the maximum age of fish.
#' @param ageInterval A single numeric that represents the interval of ages to be used (e.g., 0.1 means that ages will be modeled by tenths of a year).
#' @param Linf A single numeric that represents the point estimate of asymptotic mean length from the von Bertalanffy growth model OR an `nls` object created from fitting the von Bertalanffy equation to length-at-age data.
#' @param K A single numeric that represents the point estimate of the Brody growth coefficient from the von Bertalanffy growth model.
#' @param t0 A single numeric that represents the point estimate of the x-intercept (i.e., theoretical age at a mean length of 0) from the von Bertalanffy growth model.
#' @param FLR A single character to indicate the fecundity-length relationship; either "linear" or "log10".
#' @param FLRint A single numeric that represents the intercept of the fecundity-length relationship.
#' @param FLRslope A single numeric that represents the slope of the fecundity-length relationship.
#' @param MatAge A single integer that represents the age at maturity.
#' @param percF A numeric vector representing the percentage of females at each age, starting at age-1.
#' @param percFSpawn A numeric vector representing the percentage of spawning females at each age, starting at age-1.
#' @param L A numeric vector of lengths where mortality may change. Will assume first value is 0 if not provided. See details and examples.
#' @param cm A numeric that represents natural mortalities for the length intervals in \code{L}. See details and examples.
#' @param cf A numeric that represents fishing mortalities for the length intervals in \code{L}. See details and examples.
#'
#' @details This function is generally not used independently. It is called when requesting the spawning potential ratio while calculating yield-per-recruit.
#'
#'
#' @return A list of six elements
#' \itemize{
#' \item `Regs` is a list containing management regulations:
#'    \itemize{
#'    \item `L` = either minimum length limit or slot limit lengths.
#'    \item `M` = natural mortality.
#'    \item `F` = fishing mortality.
#'    }
#' \item `intcalcs` is a data frame containing values used in calculating SPR.
#'     \itemize{
#'       \item `t` is the time step in years based on the `ageInterval` used.
#'       \item `L_t` is the Length at time t.
#'       \item `Fec_t` is the fecundity at time t.
#'       \item `percF_t` is the percentage of females as specified in the input parameters.
#'       \item `percFSpawn_t` is the percentage of spawning females as specified in the input parameters.
#'       \item `E_t` = `Fec_t` x `percF_t` x `percFSpawn_t`
#'       \item `F_t` is the instantaneous fishing mortality at time `t`
#'       \item `M_t` is the instantaneous natural mortality at time `t`
#'       \item `Z_t` is the total instantaneous mortality at time `t`
#'       \item `S_noexp_t` is total annual survival with no exploitation at time `t`
#'       \item `S_exp_t` is the total annual survival with exploitation at time `t`
#'       \item `S_noexp_t1` is the cumulative product of total annual survival with no exploitation up to time `t`
#'       \item `S_exp_t1` is the cumulative product of total annual survival with exploitation up to time `t`
#'       \item `P_noexp_t` is the spawning potential with no exploitation at time `t`
#'       \item `P_exp_t` is the spawning potential with exploitation at time `t`
#'     }
#' \item `P_unfished` is the total spawning potential with NO exploitation.
#' \item `P_fished` is the total spawning potential with exploitation.
#' \item `SPR` is the spawning potential ratio based on specified regulations.
#' \item `Num_Eggs` is the total number of eggs expected based on specified regulations
#' }
#'
#' Note that  P_fished, P_unfished and SPR are set to 0 when they calculation yields a number less than 0.0001.
#'
#' @author Derek Ogle, \email{jason.doll@fmarion.edu}
#' @author Jason C. Doll, \email{DerekOgle51@gmail.com}
#'
#'
#' @examples
#' #===== Spawning potential ratio for fixed parameters with a single minimum length limit
#' #----- FAMS inputs
#' cf <- 0.1
#' cm <- 0.18
#' percF <- c(0,0,0,rep(0.50,27))
#' percFSpawn <- c(0,0,0,0.24,0.24,0.53,rep(1.00,24))
#' L <- 280    # MLL
#'
#' # Adjust ageInterval to test effect of fine-tuning ages on estimates
#' spr_1a <- static_spr(tmax = 30, ageInterval=0.1,
#'                      Linf = 1349.5, K = 0.111, t0 = 0.065,
#'                      FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
#'                      MatAge = 4, percF = percF, percFSpawn = percFSpawn,
#'                      L = L, cm = cm, cf = cf)
#'
#'
#' #===== spawning potential ratio for fixed parameters with a slot limit
#' L <- c(200,300,400)  ## Recruitment L, then lower & upper L for harvest slot
#' cf <- c(0,0.25,0.60,0.15)
#' cm <- rep(0.30,length(cf))
#'
#' spr_2a <- static_spr(tmax = 30, ageInterval=0.1,
#'                      Linf = 1349.5, K = 0.111, t0 = 0.065,
#'                      FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
#'                      MatAge = 4, percF = c(0,0,0,rep(0.50,27)),
#'                      percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
#'                      L = L, cm= cm, cf = cf)
#'
#' @keywords internal
#' @export

static_spr <- function(tmax,ageInterval,
                       Linf, K, t0,
                       FLR, FLRint, FLRslope,
                       MatAge, percF, percFSpawn,
                       L, cm, cf) {
  #----- Convert cf & cm to F and M
  F_ <- c(0,-log(1-cf))
  M <- rep(-log(1-cm),length(F_))

  tr <- ((log(1-L/Linf))/-K)+t0

  df <- data.frame(t=seq(1,tmax,ageInterval)) |>
    dplyr::mutate(
      L_t = Linf * (1 - exp(-K * (t - t0))),
      Fec_t = iMakeFecundity(FLR, FLRint, FLRslope, t,L_t, MatAge),
      percF_t = c(rep(percF[-length(percF)], each = ageInterval),
                  percF[length(percF)]),
      percFSpawn_t = c(rep(percFSpawn[-length(percFSpawn)], each = ageInterval),
                       percFSpawn[length(percFSpawn)]),
      E_t = Fec_t * percF_t * percFSpawn_t,
      F_t = iMakeFM_t(L_t, L, F_),
      F_t = dplyr::if_else(t < tr, 0, F_t),
      M_t = iMakeFM_t(L_t, L, M),
      Z_t = F_t + M_t,
      S_noexp_t = exp(-M_t * ageInterval),
      S_exp_t = exp(-Z_t * ageInterval),
      S_noexp_t1 = cumprod(S_noexp_t),
      S_exp_t1 = cumprod(S_exp_t),
      P_noexp_t = E_t * S_noexp_t1,
      P_exp_t = E_t * S_exp_t1)
  P_unfished <- sum(df$P_noexp_t)
  P_fished <- sum(df$P_exp_t)
  SPR <- P_fished/P_unfished
  if (P_fished < 0.001) {P_fished <- 0; SPR <- 0}
  if (P_unfished < 0.001) {P_unfished <- 0; SPR <- 0}
  if (SPR < 0.0001) SPR <- 0
  Num_Eggs <- sum(df$Fec_t)
  return(list(Regs=list(L=L,M=M,F=F_),intcalcs=df,
              P_fished=P_fished,P_unfished=P_unfished,SPR=SPR,Num_Eggs=Num_Eggs))
}
