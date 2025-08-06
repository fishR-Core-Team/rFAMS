#' @title Function to calculate static spawning potential ratio
#'
#' @description Calculates the static spawning potential ratio with the yield-per-recruit model
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
#' @param M A numeric that represents natural mortalities for the length intervals in \code{L}. See details and examples.
#' @param F A numeric that represents fishing mortalities for the length intervals in \code{L}. See details and examples.
#'
#' @details This function is generally not used independently. It is called when requesting the spawning potential ratio while calculating yield-per-recruit.
#'
#' @examples
#' #===== Spawning potential ratio for fixed parameters with a single minimum length limit
#' #----- FAMS inputs
#' cf <- 0.1
#' cm <- 0.18
#' percF <- c(0,0,0,rep(0.50,27))
#' percFSpawn <- c(0,0,0,0.24,0.24,0.53,rep(1.00,24))
#' L <- 280    # MLL
#' #----- Convert cf & cm to F and M
#' F <- c(0,-log(1-cf))
#' M <- rep(-log(1-cm),length(F))
#'
#' #----- Run DHO's version of the SPR estimating functions with FAMS inputs
#' #      Adjust ageInterval to test effect of fine-tuning ages on estimates
#' spr_1a <- static_spr_alt(tmax = 30, ageInterval=0.1,
#'                          Linf = 1349.5, K = 0.111, t0 = 0.065,
#'                          FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
#'                          MatAge = 4, percF = percF, percFSpawn = percFSpawn,
#'                          L = L, M = M, F = F)
#' spr_1a
#' # Examine what mortalities look like by length intervals
#' plot_FM(spr_1a)
#'
#'
#' #===== pawning potential ratio for fixed parameters with a slot limit
#' L <- c(200,300,400)  ## Recruitment L, then lower & upper L for harvest slot
#'                      ## ... F outside slot is hooking mortality???
#' F <- c(0,0.28768,0.91629,0.16252)
#' M <- rep(0.35667,length(F))
#'
#' spr_2a <- static_spr_alt(tmax = 30, ageInterval=0.1,
#'                          Linf = 1349.5, K = 0.111, t0 = 0.065,
#'                          FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
#'                          MatAge = 4, percF = c(0,0,0,rep(0.50,27)),
#'                          percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
#'                          L = L, M = M, F = F)
#' spr_2a
#' plot_FM(spr_2a)
#'
#' @rdname static_spr
#' @export

static_spr_alt <- function(tmax,ageInterval,
                           Linf, K, t0,
                           FLR, FLRint, FLRslope,
                           MatAge, percF, percFSpawn,
                           L, M, F) {
  df <- data.frame(t=seq(1,tmax,ageInterval)) |>
    dplyr::mutate(
      L_t=Linf*(1-exp(-K*(t-t0))),
      Fec_t=iMakeFecundity(FLR,FLRint,FLRslope,t,L_t,MatAge),
      ## Make adjustment here so last value is the last age (not that age + intervals)
      percF_t=c(rep(percF[-length(percF)],each=ageInterval),percF[length(percF)]),
      percFSpawn_t=c(rep(percFSpawn[-length(percFSpawn)],each=ageInterval),
                     percFSpawn[length(percFSpawn)]),
      E_t=Fec_t*percF_t*percFSpawn_t,
      F_t=iMakeFM_t(L_t,L,F),
      M_t=iMakeFM_t(L_t,L,M),
      Z_t=F_t+M_t,
      ## Make adjustment here as mortality is only over ageInterval, not annually
      S_noexp_t=exp(-M_t*ageInterval),
      S_exp_t=exp(-Z_t*ageInterval),
      S_noexp_t1=cumprod(S_noexp_t),
      S_exp_t1=cumprod(S_exp_t),
      P_noexp_t=E_t*S_noexp_t1,
      P_exp_t=E_t*S_exp_t1)
  P_fished <- sum(df$P_exp_t)
  P_unfished <- sum(df$P_noexp_t)
  SPR <- P_fished/P_unfished
  return(list(Regs=list(L=L,M=M,F=F),intcalcs=df,
              P_fished=P_fished,P_unfished=P_unfished,SPR=SPR))
}

#===== Make fecundity estimates for each age corresponding to the length intervals
#      in L_t from provided fecundity-length relationship and age-at-maturity
iMakeFecundity <- function(FLR,FLRint,FLRslope,t,L_t,MatAge) {
  # Adjust depending on if FLRint & FLRslope is from linear or exponential model
  if (FLR == "linear") Fec_t <- FLRint + L_t * FLRslope
  else Fec_t <- exp(FLRint + log(L_t) * FLRslope)
  # Set fecundities to 0 for ages < age-at-maturity
  Fec_t[t<MatAge] <- 0
  # Return fecundity vector
  Fec_t
}

#===== Make mortality vectors for age corresponding to the length intervals in L_t
#      based on user-provided mortalities in mort by each interval defined by L
iMakeFM_t <- function(L_t,L,mort) {
  # Check if first length break is 0, if not add it to L
  if (L[1]!=0) L <- c(0,L)
  # Populate mortality vector with mortality for last interval
  mortv <- rep(mort[length(mort)],length(L_t))
  # Change mortality vector values base on length interval
  for (i in 2:length(L)) mortv[L_t<L[i] & L_t>=L[i-1]] <- mort[i-1]
  # return mortality vector
  mortv
}

#===== Helper to visualize F and M by length interval (helps visuals if MLL,
#      harvest slot, protected slot, or other)
plot_FM <- function(x) {
  # Isolate results from x
  ## Length regulations
  L <- x$Regs$L
  ### Check if first length break is 0, if not add it to L
  if (L[1]!=0) L <- c(0,L)
  ### Make length labels (also the levels)
  L_lbls <- paste(L,c(L[-1],Inf),sep="-")
  ## Mortalities
  M <- x$Regs$M
  F <- x$Regs$F

  # Put in a data.frame
  tmp <- data.frame(L=L_lbls,F=F,M=M) |>
    tidyr::pivot_longer(cols=c("F","M"),names_to="type",values_to="mortality") |>
    dplyr::mutate(L=factor(L,levels=L_lbls))

  # Make the plot
  ggplot2::ggplot(data=tmp,mapping=ggplot2::aes(x=L,y=mortality,fill=type)) +
    ggplot2::geom_bar(stat="identity",color="black") +
    ggplot2::scale_y_continuous(name="Z",expand=ggplot2::expansion(mult=c(0,0.02))) +
    ggplot2::scale_x_discrete(name="Length Interval") +
    ggplot2::scale_fill_manual(values=c("F"="gray75","M"="gray30")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid=ggplot2::element_blank())
}
