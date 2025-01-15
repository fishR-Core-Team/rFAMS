#' @title Create data,frame of mortality values for yield-per-recruit (YPR) simulations
#'
#' @description Create a data.frame of instantaneous fishing (F) and natural (M) mortality rates from user inputs to be used in yield-per-recruit (YPR) simulations in other functions.
#'
#' @details The user may enter the mortality values in one of three ways.
#'
#' * User may enter values for `F` (and optionally `Fmax` and `Finc`) and `M` (and optionally `Mmax` and `Minc`) to enter instantaneous fishing mortality (F) and natural mortality (M) values.
#' * User may enter values for `cf` (and optionally `cfmax` and `cfinc`) and `cm` (and optionally `cmmax` and `cminc`) to enter conditional fishing mortality (cf) and natural mortality (cm) values.
#' * User may enter values of `Z` (and optionally `Zmax` and `Zinc`) along with `F` (and optionally `Fmax` and `Finc`) or `M` (and optionally `Mmax` and `Minc`) to enter instantaneous natural mortality (Z) and one of its components (F or M) values.
#'
#' In all instances the values may be entered in a variety of ways, as described below for `F`,
#'
#' * If `F` is a single value and `Fmax` and `Finc` are not used, then a single constant instantaneous fishing mortality value will be used.
#' * If `F`, `Fmax`, and `Finc` are all single values, then a sequence of instantaneous fishing mortality values will be created from `F` to `Fmax` in steps of `Finc`.
#' * If `F` is a vector of values and `Fmax` and `Finc` are not used, then that vector of instantaneous fishing mortality values will be used.
#'
#' The number of combinations of `F` and `M` may be less than expected when using `Z` as combinations that result in F<0 or M<0 are not included.
#'
#' @param F A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for instantaneous fishing mortality. May not be used depending on `input`. See details and examples.
#' @param Fmax A single numeric that represents the maximum values for a sequence of instantaneous fishing mortality values. May not be used depending on `input`. See details and examples.
#' @param Finc A single numeric that represents the increment (i.e., step) for a sequence of instantaneous fishing mortality values. May not be used depending on `input`. See details and examples.
#' @param M A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for instantaneous natural mortality. May not be used depending on `input`. See details and examples.
#' @param Mmax A single numeric that represents the maximum values for a sequence of instantaneous natural mortality values. May not be used depending on `input`. See details and examples.
#' @param Minc A single numeric that represents the increment (i.e., step) for a sequence of instantaneous natural mortality values. May not be used depending on `input`. See details and examples.
#' @param Z A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for instantaneous total mortality. May not be used depending on `input`. See details and examples.
#' @param Zmax A single numeric that represents the maximum values for a sequence of instantaneous total mortality values. May not be used depending on `input`. See details and examples.
#' @param Zinc A single numeric that represents the increment (i.e., step) for a sequence of instantaneous total mortality values. May not be used depending on `input`. See details and examples.
#' @param cf A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for conditional fishing mortality. May not be used depending on `input`. See details and examples.
#' @param cfmax A single numeric that represents the maximum values for a sequence of conditional fishing mortality values. May not be used depending on `input`. See details and examples.
#' @param cfinc A single numeric that represents the increment (i.e., step) for a sequence of conditional fishing mortality values. May not be used depending on `input`. See details and examples.
#' @param cm A single numeric that represents a constant value, a single numeric that represents the minimum value in a sequence, or a vector of values for conditional natural mortality. May not be used depending on `input`. See details and examples.
#' @param cmmax A single numeric that represents the maximum values for a sequence of conditional natural mortality values. May not be used depending on `input`. See details and examples.
#' @param cminc A single numeric that represents the increment (i.e., step) for a sequence of conditional natural mortality values. May not be used depending on `input`. See details and examples.
#'
#' @returns A data.frame that contains columns of `F` (instantaneous fishing mortality rate) and `M`: (instantaneous natural mortality rate) that were derived from the arguments as described in the details.
#'
#' @examples
#' # Demo of various ways to provide the input values
#' ## Both cf and cm constant
#' makeMort(cf=0.1,cm=0.2)
#'
#' ## cf varies as a sequence, cm constant
#' makeMort(cf=0.1,cfmax=0.5,cfinc=0.1,cm=0.2)
#'
#' ## both cf and cm vary as a sequence
#' makeMort(cf=0.1,cfmax=0.3,cfinc=0.1,cm=0.2,cmmax=0.5,cminc=0.1)
#'
#' ## cf varies by user-provided values, cm constant
#' makeMort(cf=c(0.1,0.2,0.3),cm=0.2)
#'
#' ## both cf and cm vary by user-provided values
#' makeMort(cf=c(0.1,0.2,0.3),cm=c(0.2,0.8,0.3))
#'
#' ## Similar examples for F & M values
#' makeMort(F=0.1,M=0.5)
#' makeMort(F=0.1,Fmax=0.5,Finc=0.1,M=0.5)
#' makeMort(F=0.25,Fmax=1,Finc=0.25,M=0.5,Mmax=2,Minc=0.5)
#' makeMort(F=c(0.25,0.4),M=0.5,Mmax=2,Minc=0.25)
#'
#' ## Similar examples for M & Z values
#' makeMort(M=0.5,Z=1)
#' makeMort(M=0.5,Z=0.5,Zmax=2,Zinc=0.25)
#' makeMort(M=0.5,Mmax=2,Minc=0.25,Z=0.5,Zmax=2,Zinc=0.5)
#'
#' @rdname makeMort
#' @export

makeMort <- function(F=NULL,Fmax=NULL,Finc=NULL,
                     M=NULL,Mmax=NULL,Minc=NULL,
                     Z=NULL,Zmax=NULL,Zinc=NULL,
                     cf=NULL,cfmax=NULL,cfinc=NULL,
                     cm=NULL,cmmax=NULL,cminc=NULL) {

  #===== Internal checking functions
  iCheckInputCFCM <- function(F,Fmax,Finc,M,Mmax,Minc,Z,Zmax,Zinc,
                              cf,cfmax,cfinc,cm,cmmax,cminc) {
    if (!is.null(cf)) {
      if (is.null(cm)) {
        if (any(!is.null(Z),!is.null(Zmax),!is.null(Zinc)))
          STOP("'Z', 'Zmax', and 'Zinc' not used with 'cf', only 'cm' can be.")
        if (any(!is.null(F),!is.null(Fmax),!is.null(Finc)))
          STOP("'F', 'Fmax', and 'Finc' not used with 'cf', only 'cm' can be")
        if (any(!is.null(M),!is.null(Mmax),!is.null(Minc)))
          STOP("'M', 'Mmax', and 'Minc' not used with 'cf'\n",
               "  Did you mean to use 'cm' instead?")
        STOP("Must include 'cm' with 'cf'.")
      } else {
        if (any(!is.null(Z),!is.null(Zmax),!is.null(Zinc)))
          WARN("'Z', 'Zmax', and 'Zinc' not used with 'cf' and 'cm';\n",
               "  only values in 'cf' and 'cm' were used.")
        if (any(!is.null(F),!is.null(Fmax),!is.null(Finc)))
          WARN("'F', 'Fmax', and 'Finc' not used with 'cf' and 'cm';\n",
               "  only values in 'cf' and 'cm' were used.")
        if (any(!is.null(M),!is.null(Mmax),!is.null(Minc)))
          WARN("'M', 'Mmax', and 'Minc' not used with 'cf' and 'cm';\n",
               "  only values in 'cf' and 'cm' were used.")
      }
    } else {
      if (!is.null(cm)) {
        if (any(!is.null(Z),!is.null(Zmax),!is.null(Zinc)))
          STOP("'Z', 'Zmax', and 'Zinc' not used with 'cm', only 'cf' can be.")
        if (any(!is.null(M),!is.null(Mmax),!is.null(Minc)))
          STOP("'M', 'Mmax', and 'Minc' not used with 'cm', only 'cf' can be")
        if (any(!is.null(F),!is.null(Fmax),!is.null(Finc)))
          STOP("'F', 'Fmax', and 'Finc' not used with 'cm'\n",
               "  Did you mean to use 'cf' instead?")
        STOP("Must include 'cf' with 'cm'.")
      }
    }
    # If here without STOPping then return input value
    "cfcm"
  }

  iCheckInputFMZ <- function(F,Fmax,Finc,M,Mmax,Minc,Z,Zmax,Zinc,
                             cf,cfmax,cfinc,cm,cmmax,cminc) {
    if (all(!is.null(F),!is.null(M),!is.null(Z)))
      STOP("Only two of 'F', 'M', and 'Z' can be given (not all three).")
    if (any(!is.null(cf),!is.null(cfmax),!is.null(cfinc)))
      STOP("'cf', 'cfmax', and 'cfinc' not used with 'Z', 'F', or 'M'",
           ifelse(!is.null(F),"\n  Did you mean to use 'F' instead?","."))
    if (any(!is.null(cm),!is.null(cmmax),!is.null(cminc)))
      STOP("'cm', 'cmmax', and 'cminc' not used with 'Z', 'F', or 'M'",
           ifelse(!is.null(F),"\n  Did you mean to use 'M' instead?","."))
    if (!is.null(F) & is.null(M) & is.null(Z))
      STOP("Must include 'M' or 'Z' with 'F'.")
    if (!is.null(M) & is.null(F) & is.null(Z))
      STOP("Must include 'F' or 'Z' with 'M'.")
    if (!is.null(Z) & is.null(F) & is.null(M))
      STOP("Must include 'F' or 'M' with 'Z'.")
    if (!is.null(F)) {
      if (!is.null(M)) input <- "FM"
      else input <- "FZ"
    } else input <- "MZ"
    input
  }

  #===== Internal checking and calculation functions
  iGetCF <- function(cf,cfmax,cfinc) {
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
    cf
  }

  iGetCM <- function(cm,cmmax,cminc) {
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
    cm
  }

  iGetF <- function(F,Fmax,Finc) {
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
    F
  }

  iGetM <- function(M,Mmax,Minc) {
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
    M
  }

  iGetZ <- function(Z,Zmax,Zinc) {
    if (length(Z)>1) {
      # Z vector provided by the user ... just do some checks
      if (!is.numeric(Z)) STOP("'Z' must be numeric.")
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
    Z
  }

  #===== Internal calculation functions
  iHndlCFCM <- function(cf,cfmax,cfinc,cm,cmmax,cminc) {
    # Get vector of values if all checks for values are passed
    cf <- iGetCF(cf,cfmax,cfinc)
    cm <- iGetCM(cm,cmmax,cminc)

    # Create all combos of F & M from cf & cm ... return item
    F <- -log(1-cf)
    M <- -log(1-cm)
    tidyr::expand_grid(F,M)
  }

  iHndlFM <- function(F,Fmax,Finc,M,Mmax,Minc) {
    # Get vector of values if all checks for values are passed
    F <- iGetF(F,Fmax,Finc)
    M <- iGetM(M,Mmax,Minc)
    # Create all combos of F & M ... return item
    tidyr::expand_grid(F,M)
  }

  iHndlMZ <- function(M,Mmax,Minc,Z,Zmax,Zinc) {
    # Get vector of values if all checks for values are passed
    M <- iGetM(M,Mmax,Minc)
    Z <- iGetZ(Z,Zmax,Zinc)
    # Create all combos of F & M from M & Z, only return where F>=0 ... return item
    tidyr::expand_grid(M,Z) |>
      dplyr::mutate(F=Z-M) |>
      dplyr::filter(F>=0) |>
      dplyr::select(F,M) |>
      dplyr::arrange(F,M)
  }

  iHndlFZ <- function(F,Fmax,Finc,Z,Zmax,Zinc) {
    # Get vector of values if all checks for values are passed
    F <- iGetF(F,Fmax,Finc)
    Z <- iGetZ(Z,Zmax,Zinc)
    # Create all combos of F & M from F & Z, only return where M>=0 ... return item
    tidyr::expand_grid(F,Z) |>
      dplyr::mutate(M=Z-F) |>
      dplyr::filter(M>=0) |>
      dplyr::select(F,M) |>
      dplyr::arrange(F,M)
  }


  ## Main function
  #----- Determine which values are given
  if (is.null(cf) & is.null(cm) & is.null(F) & is.null(M) & is.null(Z))
    STOP("Values must be given to 'cf' and 'cm', or any two of 'F', 'M', and 'Z'")

  if (!all(is.null(cf),is.null(cm)))
    input <- iCheckInputCFCM(F,Fmax,Finc,M,Mmax,Minc,Z,Zmax,Zinc,
                             cf,cfmax,cfinc,cm,cmmax,cminc)
  if (!all(is.null(F),is.null(M),is.null(Z)))
    input <- iCheckInputFMZ(F,Fmax,Finc,M,Mmax,Minc,Z,Zmax,Zinc,
                            cf,cfmax,cfinc,cm,cmmax,cminc)

  #----- Send to internals depending on input choice
  switch(input,
         cfcm = iHndlCFCM(cf,cfmax,cfinc,cm,cmmax,cminc),
         FM = iHndlFM(F,Fmax,Finc,M,Mmax,Minc),
         MZ = iHndlMZ(M,Mmax,Minc,Z,Zmax,Zinc),
         FZ = iHndlFZ(F,Fmax,Finc,Z,Zmax,Zinc)) |>
  as.data.frame()
}
