#' @title Make a list or vector of life history parameters for yield-per-recruit analyses.
#'
#' @description Efficiently construct either a vector or list that contains the seven life history parameters required for Beverton-Holt yield-per-recruit analyses. The parameters can be given by the user through function arguments. Alternativvely, the von Bertalanffy parameters (`Linf`, `K`, and `t0`) may be extracted from an `nls` object created from fitting the von Bertalanffy equation to length-at-age data (object created outside this function). Similarly the log10-transformed weight-length model coefficients may be extracted from an `lm` object created from fitting the model to transformed weight-length data (object created outside this function). All parameter values are checked for sanity (e.g., Linf>0).
#'
#' @param N0 A single numeric that represents the number of fish in the population at the hypothetical age of \code{t0}.
#' @param tmax A single whole number that represents maximum age in the population in years.
#' @param Linf A single numeric that represents the point estimate of asymptotic mean length from the von Bertalanffy growth model OR an `nls` object created from fitting the von Bertalanffy equation to length-at-age data.
#' @param K A single numeric that represents the point estimate of the Brody growth coefficient from the von Bertalanffy growth model.
#' @param t0 A single numeric that represents the point estimate of the x-intercept (i.e., theoretical age at a mean length of 0) from the von Bertalanffy growth model.
#' @param LWalpha A single numeric that represents the point estimate of alpha from the length-weight regression on the log10 scale OR an `lm` object created from fitting the model to log10-transformed weight-length data.
#' @param LWbeta A single numeric that represents the point estimate of beta from the length-weight regression on the log10 scale.
#' @param restype A character that indicates the type of output (list or vector) returned by the function.
#'
#' @details Use of this function for putting life history parameters into a list or vector is recommended as (i) values for `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta` can be extracted from objects from appropriate model fitting and (ii) checks for impossible or improbable values for each parameter are performed; i.e.,
#'
#' @author Derek Ogle
#'
#' ```R
#' # Best practice for entering life history parameter values
#' LH <- makeLH(N0=100,tmax=15,Linf=600,K=0.30,t0=-0.6,
#'              LWalpha=-5.453,LWbeta=3.10)
#'
#' # Works but no checks on the values
#' LH <- list(N0=100,tmax=15,Linf=600,K=0.30,t0=-0.6,
#'            LWalpha=-5.453,LWbeta=3.10)
#' ```
#'
#' If a list is returned then values will be displayed with the number of decimals provided by the user. If a vector is returned then the number of decimals displayed will be the same for each value and will match the value supplied by the user with the most decimals. Thus, a list is preferred as it will be easier to match what was given to what was expected to be given.
#'
#' @returns A named list or vector (depending on `restype`) that contains the given (or extracted) life history parameters values that can be used directly in the yield-per-recruit calculation functions (e.g., \code{\link{yprBH_SlotLimit}}).
#'
#' @examples
#' library(FSA)
#' library(FSAdata)
#'
#' # ----- Simple examples with explicity arguments for each -------------------
#' makeLH(N0=100,tmax=15,Linf=500,K=0.3,t0=-0.5,LWalpha=-5.613,LWbeta=3.1)
#' makeLH(N0=100,tmax=15,Linf=500,K=0.3,t0=-0.5,LWalpha=-5.613,LWbeta=3.1,
#'        restype="vector")
#'
#' # ----- Example of extracting values from model fits ------------------------
#' # N0 and tmax provided as arguments ... Linf, K, and t0 extracted from nls
#' #   output and LWalpha and LWbeta extracted from lm output. Note that nls
#' #   and lm output here are just examples of the function, they should be
#' #   calculated for the same species from the same waterbody, etc.
#'
#' ## get some LVB results (as an example)
#' data(SpotVA1,package="FSA")
#' SpotVA1 <- SpotVA1 |>
#'   dplyr::mutate(tl=tl*25.4)
#' vb1 <- FSA::vbFuns()
#' fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,
#'             start=FSA::vbStarts(tl~age,data=SpotVA1))
#'
#' ## get some LW results (as an example)
#' data(BluegillLM,package="FSAdata")
#' BluegillLM <- BluegillLM |>
#'   dplyr::mutate(logW=log10(wght),
#'                 logL=log10(tl))
#' fit2 <- lm(logW~logL,data=BluegillLM)
#'
#' makeLH(N0=100,tmax=15,Linf=fit1,LWalpha=fit2)
#'
#' @rdname makeLH
#' @export

makeLH <- function(N0,tmax,Linf,K,t0,LWalpha,LWbeta,restype=c("list","vector")) {
  restype <- match.arg(restype)
  iCheckN0(N0)
  iCheckMaxAge(tmax)

  ## if nls object in Linf then extract coefficients and put in separate values
  if (!missing(Linf)) {
    if (isa(Linf,"lm")) STOP("'Linf' given object from 'lm()', did you mean\n",
                             "  to give it an 'nls' object?")
    if (isa(Linf,"nls")) {
      tmp <- stats::coef(Linf)
      if (length(names(tmp))!=3)
        STOP("Number of paramaters in 'nls' object is not 3; 'nls' object must\n",
             "  be from fitting a von Bertalanffy model.")
      if (!all(names(tmp) %in% c("Linf","K","t0")))
        STOP("Names of parameters in 'nls' object are not 'Linf', 'K', and 't0';\n",
             "'nls' object must be from fitting a von Bertalanffy model.")
      Linf <- tmp[["Linf"]]
      if (missing(K)) K <- tmp[["K"]]
      if (missing(t0)) t0 <- tmp[["t0"]]
    }
  }
  iCheckLinf(Linf)
  iCheckK(K)
  iCheckt0(t0)

  ## if LWalpha is an lm object then extract coeffs and put in separate values
  if (!missing(LWalpha)) {
    if (isa(LWalpha,"nls")) STOP("'LWalpha' given object from 'nls()', did you\n",
                                 "  mean to give it an 'lm' object?")
    if (isa(LWalpha,"lm")) {
      tmp <- stats::coef(LWalpha)
      if (length(names(tmp))!=2)
        STOP("Number of paramaters in 'lm' object is not 2; 'lm' object must\n",
             "  be from fitting a log10 weight-length linear regression.")
      LWalpha <- tmp[["(Intercept)"]]
      if (missing(LWbeta)) LWbeta <- tmp[[2]]
    }
  }
  iCheckLWa(LWalpha)
  iCheckLWb(LWbeta)

  ## Return vector or list
  if (restype=="list") res <- list(N0,tmax,Linf,K,t0,LWalpha,LWbeta)
  else res <- c(N0,tmax,Linf,K,t0,LWalpha,LWbeta)
  names(res) <- c("N0","tmax","Linf","K","t0","LWalpha","LWbeta")
  res
}
