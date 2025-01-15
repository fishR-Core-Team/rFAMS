#' @title Create data frame of mortality values for yield-per-recruit (YPR) simulations
#'
#' @description XXX
#'
#' @details The user may enter the mortality values in one of three ways, which must be declared with `input` as follows.
#'
#' * With `input="cfcm"` the user must use `cf` (and optionally `cfmax` and `cfinc`) and `cm` (and optionally `cmmax` and `cminc`) to enter conditional fishing mortality (cf) and natural mortality (cm) values.
#' * With `input="FM"` the user must use `F` (and optionally `Fmax` and `Finc`) and `M` (and optionally `Mmax` and `Minc`) to enter instantaneous fishing mortality (F) and natural mortality (M) values.
#' * With `input="MZ"` the user must use `M` (and optionally `Mmax` and `Minc`) and `Z` (and optionally `Zmax` and `Zinc`) to enter instantaneous natural mortality (Z) and total mortality (M) values.
#'
#' In all instances the values may be entered in a variety of ways, as described below for `cf`,
#'
#' * If `cf` is a single value and `cfmax` and `cfinc` are not used, then a single constant conditional fishing mortality value will be used.
#' * If `cf`, `cfmax`, and `cfinc` are all single values, then a sequence of conditional fishing mortality values will be created from `cf` to `cfmax` in steps of `cfinc`.
#' * If `cf` is a vector of values and `cfmax` and `cfinc` are not used, then that vector of conditional fishing mortality values will be used.
#'
#' @param input A string that contains an abbreviation for how the mortality values will be given by the user. See details.
#' @param cf A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for conditional fishing mortality. May not be used depending on `input`. See details and examples.
#' @param cfmax A single numeric that represents the maximum values for a sequence of conditional fishing mortality values. May not be used depending on `input`. See details and examples.
#' @param cfinc A single numeric that represents the increment (i.e., step) for a sequence of conditional fishing mortality values. May not be used depending on `input`. See details and examples.
#' @param cm A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for conditional natural mortality. May not be used depending on `input`. See details and examples.
#' @param cmmax A single numeric that represents the maximum values for a sequence of conditional natural mortality values. May not be used depending on `input`. See details and examples.
#' @param cminc A single numeric that represents the increment (i.e., step) for a sequence of conditional natural mortality values. May not be used depending on `input`. See details and examples.
#' @param F A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for instantaneous fishing mortality. May not be used depending on `input`. See details and examples.
#' @param Fmax A single numeric that represents the maximum values for a sequence of instantaneous fishing mortality values. May not be used depending on `input`. See details and examples.
#' @param Finc A single numeric that represents the increment (i.e., step) for a sequence of instantaneous fishing mortality values. May not be used depending on `input`. See details and examples.
#' @param M A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for instantaneous natural mortality. May not be used depending on `input`. See details and examples.
#' @param Mmax A single numeric that represents the maximum values for a sequence of instantaneous natural mortality values. May not be used depending on `input`. See details and examples.
#' @param Minc A single numeric that represents the increment (i.e., step) for a sequence of instantaneous natural mortality values. May not be used depending on `input`. See details and examples.
#' @param Z A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for instantaneous total mortality. May not be used depending on `input`. See details and examples.
#' @param Zmax A single numeric that represents the maximum values for a sequence of instantaneous total mortality values. May not be used depending on `input`. See details and examples.
#' @param Zinc A single numeric that represents the increment (i.e., step) for a sequence of instantaneous total mortality values. May not be used depending on `input`. See details and examples.
#'
#' @returns A data.frame that contains the following items
#'
#' * `cf`: Conditional fishing mortality rate
#' * `cm`: Conditional natural mortality rate
#' * `F`: Instantaneous fishing mortality rate
#' * `M`: Instantaneous naural mortality rate
#' * `Z`: Instantaneous total mortality rate
#' * `S`: Annual survival rate
#' * `A`: Annual mortality rate
#' * `u`: Annual exploitation rate
#'
#' @examples
#' # Demo of various ways to provide the input values
#' ## Both cf and cm constant
#' makeMort("cfcm",cf=0.1,cm=0.2)
#'
#' ## cf varies as a sequence, cm constant
#' makeMort("cfcm",cf=0.1,cfmax=0.5,cfinc=0.1,cm=0.2)
#'
#' ## both cf and cm vary as a sequence
#' makeMort("cfcm",cf=0.1,cfmax=0.3,cfinc=0.1,cm=0.2,cmmax=0.5,cminc=0.1)
#'
#' ## cf varies by user-provided values, cm constant
#' makeMort("cfcm",cf=c(0.1,0.2,0.3),cm=0.2)
#'
#' ## both cf and cm vary by user-provided values
#' makeMort("cfcm",cf=c(0.1,0.2,0.3),cm=c(0.2,0.8,0.3))
#'
#' ## Similar examples for F & M values
#' makeMort("FM",F=0.1,M=0.5)
#' makeMort("FM",F=0.1,Fmax=0.5,Finc=0.1,M=0.5)
#' makeMort("FM",F=0.25,Fmax=1,Finc=0.25,M=0.5,Mmax=2,Minc=0.5)
#' makeMort("FM",F=c(0.25,0.4),M=0.5,Mmax=2,Minc=0.25)
#'
#' ## Similar examples for F & M values
#' makeMort("MZ",M=0.5,Z=1)
#' makeMort("MZ",M=0.5,Z=0.5,Zmax=2,Zinc=0.25)
#' makeMort("MZ",M=0.5,Mmax=2,Minc=0.25,Z=0.5,Zmax=2,Zinc=0.5)
#'
#' @rdname makeMort
#' @export

makeMort <- function(input=c("cfcm","FM","MZ","cmcf","MF","ZM"),
                     cf=NULL,cfmax=NULL,cfinc=NULL,
                     cm=NULL,cmmax=NULL,cminc=NULL,
                     F=NULL,Fmax=NULL,Finc=NULL,
                     M=NULL,Mmax=NULL,Minc=NULL,
                     Z=NULL,Zmax=NULL,Zinc=NULL) {

  ## Internal functions
  iGetcfcm <- function(cf,cfmax,cfinc,cm,cmmax,cminc) {
    if (length(cf)>1) {
      # cf vector provided by the user ... just do some checks
      if (!is.numeric(cf)) STOP("'cf' must be numeric.")
      if (any(cf<0)) STOP("All values in 'cf' must be >=0")
      if (any(cf>1)) STOP("All values in 'cf' must be <=1")
    } else {
      # cf needs to be created as a sequence ... after some checks
      iCheckMort(cf,typeTFM="fishing",typeIC="conditional")
      if (!is.null(cfmax)) {
        if (is.null(cfinc))
          STOP("if 'cfmax' is given then 'cfinc' must also be given.")
        iCheckMort(cfmax,typeTFM="fishing",typeIC="conditional",
                   minmax="maximum",check_missing=FALSE)
        cf <- iCheckMortinc(cfinc,cf,cfmax,typeTFM="fishing",typeIC="conditional",
                            check_missing=FALSE)
      }
    }

    if (length(cm)>1) {
      # cm vector provided by the user ... just do some checks
      if (!is.numeric(cm)) STOP("'cm' must be numeric.")
      if (any(cm<0)) STOP("All values in 'cm' must be >=0")
      if (any(cm>1)) STOP("All values in 'cm' must be <=1")
    } else {
      # cm needs to be created as a sequence ... after some checks
      iCheckMort(cm,typeTFM="natural",typeIC="conditional")
      if (!is.null(cmmax)) {
        if (is.null(cminc))
          STOP("if 'cmmax' is given then 'cminc' must also be given.")
        iCheckMort(cmmax,typeTFM="natural",typeIC="conditional",
                   minmax="maximum",check_missing=FALSE)
        cm <- iCheckMortinc(cminc,cm,cmmax,typeTFM="natural",typeIC="conditional",
                            check_missing=FALSE)
      }
    }

    # Create all combos of cf & cm, and add F, M, and Z ... return item
    tidyr::expand_grid(cf,cm) |>
      dplyr::mutate(F=-log(1-cf),
                    M=-log(1-cm),
                    Z=F+M)
  }

  iGetFM <- function(F,Fmax,Finc,M,Mmax,Minc) {
    if (length(F)>1) {
      # F vector provided by the user ... just do some checks
      if (!is.numeric(F)) STOP("'F' must be numeric.")
      if (any(F<0)) STOP("All values in 'F' must be >=0")
    } else {
      # F needs to be created as a sequence ... after some checks
      iCheckMort(F,typeTFM="fishing",typeIC="instantaneous")
      if (!is.null(Fmax)) {
        if (is.null(Finc)) STOP("if 'Fmax' is given then 'Finc' must also be given.")
        iCheckMort(Fmax,typeTFM="fishing",typeIC="instantaneous",
                   minmax="maximum",check_missing=FALSE)
        F <- iCheckMortinc(Finc,F,Fmax,typeTFM="fishing",typeIC="instantaneous",
                           check_missing=FALSE)
      }
    }

    if (length(M)>1) {
      # M vector provided by the user ... just do some checks
      if (!is.numeric(M)) STOP("'M' must be numeric.")
      if (any(M<0)) STOP("All values in 'M' must be >=0")
    } else {
      # M needs to be created as a sequence ... after some checks
      iCheckMort(M,typeTFM="natural",typeIC="instantaneous")
      if (!is.null(Mmax)) {
        if (is.null(Minc)) STOP("if 'Mmax' is given then 'Minc' must also be given.")
        iCheckMort(Mmax,typeTFM="natural",typeIC="instantaneous",
                   minmax="maximum",check_missing=FALSE)
        M <- iCheckMortinc(Minc,M,Mmax,typeTFM="natural",typeIC="instantaneous",
                           check_missing=FALSE)
      }
    }

    # Create all combos of F & M, and add cf, cm, and Z ... return item
    tidyr::expand_grid(F,M) |>
      dplyr::mutate(cf=1-exp(-F),
                    cm=1-exp(-M),
                    Z=F+M)
  }

  iGetMZ <- function(M,Mmax,Minc,Z,Zmax,Zinc) {
    if (length(M)>1) {
      # M vector provided by the user ... just do some checks
      if (!is.numeric(M)) STOP("'M' must be numeric.")
      if (any(M<0)) STOP("All values in 'M' must be >=0")
    } else {
      # M needs to be created as a sequence ... after some checks
      iCheckMort(M,typeTFM="natural",typeIC="instantaneous")
      if (!is.null(Mmax)) {
        if (is.null(Minc)) STOP("if 'Mmax' is given then 'Minc' must also be given.")
        iCheckMort(Mmax,typeTFM="natural",typeIC="instantaneous",
                   minmax="maximum",check_missing=FALSE)
        M <- iCheckMortinc(Minc,M,Mmax,typeTFM="natural",typeIC="instantaneous",
                           check_missing=FALSE)
      }
    }

    if (length(Z)>1) {
      # Z vector provided by the user ... just do some checks
      if (!is.numeric(Z)) STOP("'M' must be numeric.")
      if (any(Z<0)) STOP("All values in 'Z' must be >=0")
    } else {
      # M needs to be created as a sequence ... after some checks
      iCheckMort(Z,typeTFM="total",typeIC="instantaneous")
      if (!is.null(Zmax)) {
        if (is.null(Zinc)) STOP("if 'Zmax' is given then 'Zinc' must also be given.")
        iCheckMort(Zmax,typeTFM="total",typeIC="instantaneous",
                   minmax="maximum",check_missing=FALSE)
        Z <- iCheckMortinc(Zinc,Z,Zmax,typeTFM="total",typeIC="instantaneous",
                           check_missing=FALSE)
      }
    }

    # Create all combos of Z & M; add cf, cm, and F; delete F is neg ... return item
    tidyr::expand_grid(Z,M) |>
      dplyr::mutate(F=Z-M,
                    cf=1-exp(-F),
                    cm=1-exp(-M)) |>
      dplyr::filter(F>=0)
  }

  ## Main function
  #----- Deal with visible bindings issue on check
  S <- A <- NULL
  #- Handle input arg ... match, deal w/ reversals of acronyms
  input <- match.arg(input)
  if (input=="cmcf") input <- "cfcm"
  if (input=="MF") input <- "FM"
  if (input=="ZM") input <- "MZ"
  #- Send warnings if arguments other than those asked for are used
  if (input %in% c("cfcm","MF")) {
    if (any(!is.null(Z),!is.null(Zmax),!is.null(Zinc)))
      STOP("'Z', 'Zmax', and 'Zinc' not used with 'input' of '",input,"'.")
  }
  if (input=="cfcm") {
    if (any(!is.null(F),!is.null(Fmax),!is.null(Finc)))
      STOP("'F', 'Fmax', and 'Finc' not used with 'input' of '",input,"'",
                   ifelse(is.null(cf),"\n  Did you mean to use 'cf' instead?","."))
    if (any(!is.null(M),!is.null(Mmax),!is.null(Minc)))
      STOP("'M', 'Mmax', and 'Minc' not used with 'input' of '",input,"'",
                   ifelse(is.null(cm),"\n  Did you mean to use 'cm' instead?","."))
  }
  if (input=="FM") {
    if (any(!is.null(cf),!is.null(cfmax),!is.null(cfinc)))
      STOP("'cf', 'cfmax', and 'cfinc' not used with 'input' of '",input,"'",
                   ifelse(is.null(F),"\n  Did you mean to use 'F' instead?","."))
    if (any(!is.null(cm),!is.null(cmmax),!is.null(cminc)))
      STOP("'cm', 'cmmax', and 'cminc' not used with 'input' of '",input,"'",
                   ifelse(is.null(M),"\n  Did you mean to use 'M' instead?","."))
  }
  if (input=="MZ") {
    if (any(!is.null(cf),!is.null(cfmax),!is.null(cfinc)))
      STOP("'cf', 'cfmax', and 'cfinc' not used with 'input' of '",input,"'.")
    if (any(!is.null(F),!is.null(Fmax),!is.null(Finc)))
      STOP("'F', 'Fmax', and 'Finc' not used with 'input' of '",input,"'",
                   ifelse(is.null(M),"\n  Did you mean to use 'M' instead?",
                          ifelse(is.null(Z),"\n  Did you mean to use 'Z' instead?",".")))
    if (any(!is.null(cm),!is.null(cmmax),!is.null(cminc)))
      STOP("'cm', 'cmmax', and 'cminc' not used with 'input' of '",input,"'",
                   ifelse(is.null(M),"\n  Did you mean to use 'M' instead?","."))
  }

  #- Send to internals depending on input choice
  switch(input,
         cfcm = iGetcfcm(cf,cfmax,cfinc,cm,cmmax,cminc),
         FM = iGetFM(F,Fmax,Finc,M,Mmax,Minc),
         MZ = iGetMZ(M,Mmax,Minc,Z,Zmax,Zinc)) |>
    dplyr::arrange(cf,cm) |>
    dplyr::select(cf,cm,F,M,Z) |>
    dplyr::mutate(S=exp(-Z),
                  A=1-S,
                  u=A*F/Z) |>
    as.data.frame()
}
