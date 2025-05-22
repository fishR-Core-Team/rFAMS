#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in rFAMS.
#'
#' @rdname rFAMS-internals
#' @keywords internal
#' @aliases STOP WARN .onAttach is.wholenumber iIbeta iErrMore1 iErrNotNumeric iErrLT iErrGt iCheckMLH iCheckMLHinc iCheckcf iCheckcm iCheckcfminc iCheckN0 iCheckLinf iCheckK iCheckt0 iCheckLWb iCheckLWa iCheckMaxAge

# -- Sends a start-up message to the console when the package is loaded.
.onAttach <- function(libname, pkgname) {
  vers <- read.dcf(system.file("DESCRIPTION",
                               package=pkgname,lib.loc=libname),
                   fields="Version")
  packageStartupMessage("Welcome to rFAMS, the R version of FAMS! We are currently in development.")
}


# -- Helper Functions
# same as stop() and warning() but with call.=FALSE as default
STOP <- function(...,call.=FALSE,domain=NULL) stop(...,call.=call.,domain=domain)
WARN <- function(...,call.=FALSE,immediate.=FALSE,noBreaks.=FALSE,domain=NULL) {
  warning(...,call.=call.,immediate.=immediate.,noBreaks.=noBreaks.,domain=domain)
}

# Checks if a value is a whole number
is.wholenumber <- function(x,tol=.Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# Incomplete beta function ... see tests for comparison to other packages
iIbeta <- function(x,a,b) {
  if (any(x<0)) STOP("'x' in incomplete beta function must be >=0.")
  if (any(x>1)) STOP("'x' in incomplete beta function must be <=1.")
  if (any(a<0)) STOP("'a' in incomplete beta function must be >=0.")
  if (any(b<0)) STOP("'b' in incomplete beta function must be >=0.")
  beta(a,b)*stats::pbeta(x,a,b)
}

# -- General Error Checks --
# Error if more than one item
iErrMore1 <- function(x,nm) if(length(x)>1) STOP("Only use one value in ",nm,".")

# Error if not numeric
iErrNotNumeric <- function(x,nm) if (!is.numeric(x)) STOP(nm," must be a number.")

# Error if less than value
iErrLT <- function(x,value,nm) if (x<value) STOP(nm," must be >=",value,".")

# Error if greater than value
iErrGT <- function(x,value,nm) if (x>value) STOP(nm," must be <=",value,".")


# -- Specific Checks --
# Check minimum length limit for harvest
iCheckMLH <- function(x,type="") {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a minimum length (mm) limit for harvest in ",nm,".")
  if (is.null(x)) STOP("Need to specify a minimum length (mm) limit for harvest in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<100) WARN("A minimum length limit of harvest of ",x," mm seems too small,\n",
                  "please check value in ",nm,".")
  if (x>1600) WARN("A minimum length limit of harvest of ",x," mm seems too large,\n",
                   "  please check value in ",nm,".")
}

# Check min length at harvest increments (min/max should be checked prior),
#   return sequence if everything looks good
iCheckMLHinc <- function(xinc,xmin,xmax) {
  ## checks of increment
  nm <- paste0("'",deparse(substitute(xinc)),"'")
  if (missing(xinc))
    STOP("Need to specify an increment for minimum length (mm) limit for harvest in ",nm,".")
  if (is.null(xinc))
    STOP("Need to specify an increment for minimum length (mm) limit for harvest in ",nm,".")
  iErrMore1(xinc,nm)
  iErrNotNumeric(xinc,nm)
  iErrLT(xinc,0,nm)
  ## Check min vs max
  nm1 <- paste0("'",deparse(substitute(xmin)),"'")
  nm2 <- paste0("'",deparse(substitute(xmax)),"'")
  if(xmin>xmax) STOP(nm1," must be equal to or less than ",nm2,".")
  res <- seq(xmin,xmax,xinc)
  if (length(res)>100)
    WARN("Choices of ",nm1,", ",nm2,", and ",nm," resulted in ",length(res),
         " values./n","  Depending on other choices the simulation may be slow.")
  ## Return sequence
  res
}


# Check conditional fishing mortality value
iCheckcf <- function(x,type=NULL) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
  if (missing(x)) STOP("Need to specify a",type,
                       " conditional fishing mortality in ",nm,".")
  if (is.null(x)) STOP("Need to specify a ",type,
                       " conditional fishing mortality in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
}

# Check conditional natural mortality value
iCheckcm <- function(x,type=NULL) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
  if (missing(x)) STOP("Need to specify a",type,
                       " conditional natural mortality in ",nm,".")
  if (is.null(x)) STOP("Need to specify a",type,
                       " conditional natural mortality in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
}

# Check conditional mortality increments (min/max should be checked prior),
#   return sequence if everything looks good
iCheckcfminc <- function(xinc,xmin,xmax) {
  ## checks of increment
  nm <- paste0("'",deparse(substitute(xinc)),"'")
  if (missing(xinc))
    STOP("Need to specify an increment for conditional natural mortality in ",nm,".")
  if (is.null(xinc))
    STOP("Need to specify an increment for conditional natural mortality in ",nm,".")
  iErrMore1(xinc,nm)
  iErrNotNumeric(xinc,nm)
  iErrLT(xinc,0,nm)
  iErrGT(xinc,1,nm)
  ## Check min vs max
  nm1 <- paste0("'",deparse(substitute(xmin)),"'")
  nm2 <- paste0("'",deparse(substitute(xmax)),"'")
  if(xmin>xmax) STOP(nm1," must be equal to or less than ",nm2,".")
  res <- seq(xmin,xmax,xinc)
  if (length(res)>100)
    WARN("Choices of ",nm1,", ",nm2,", and ",nm," resulted in ",length(res),
         " values.\n","  Depending on other choices the simulation may be slow.")
  ## Return sequence
  res
}

# Check initial number of fish in the population
iCheckN0 <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x))
    STOP("Need to specify an initial number of fish in the population in ",nm,".")
  if (is.null(x))
    STOP("Need to specify an initial number of fish in the population in ",nm,".")
  if (length(x)>1) {
    pnms <- c('N0','Linf','K','t0','LWalpha','LWbeta', 'maxage')
    if (length(x)!=7) STOP(nm," must contain only one value for ",nm," or 7 named\n",
                           "values for: ",paste(pnms,collapse=", "))
    if (is.null(names(x))) STOP(nm," must have named values for: ",
                                paste(pnms,collapse=", "))
    if (!all(names(x) %in% pnms)) STOP(nm," must have named values for all of: ",
                                       paste(pnms,collapse=", "))
  } else {
    iErrMore1(x,nm)
    iErrNotNumeric(x,nm)
    iErrLT(x,0,nm)
    if (!is.wholenumber(x))
      WARN("The initial number in the population is not a whole number,\n",
           "  please check value in ",nm,".")
  }
}

# Check Linf
iCheckLinf <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a mean asymptotic length (mm) in ",nm,".")
  if (is.null(x)) STOP("Need to specify a mean asymptotic length (mm) in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<200) WARN("A mean asymptotic length of ",x," mm seems too small,\n",
                  "  please check value in ",nm,".")
  if (x>2000) WARN("A mean asymptotic length of ",x," mm seems too large,\n",
                   "  please check value in ",nm,".")
}

# Check K
iCheckK <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a Brody growth coefficient in ",nm,".")
  if (is.null(x)) STOP("Need to specify a Brody growth coefficient in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<0.1) WARN("A Brody growth coefficient of ",x," seems too small,\n",
                  "  please check value in ",nm,".")
  if (x>0.6) WARN("A Brody growth coefficient of ",x," mm seems too large,\n",
                  "  please check value in ",nm,".")
}

# Check t0
iCheckt0 <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a Brody growth coefficient in ",nm,".")
  if (is.null(x)) STOP("Need to specify a Brody growth coefficient in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check length-weight beta
iCheckLWb <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a weight-length beta coefficient in ",nm,".")
  if (is.null(x)) STOP("Need to specify a weight-length beta coefficient in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<2) WARN("A weight-length beta coefficient of ",x," seems too small,\n",
                "  please check value in ",nm,".")
  if (x>4) WARN("A weight-length beta coefficient of ",x," mm seems too large,\n",
                "  please check value in ",nm,".")
}

# Check length-weight alpha
iCheckLWa <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a weight-length alpha coefficient in ",nm,".")
  if (is.null(x)) STOP("Need to specify a weight-length alpha coefficient in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}


# Check maximum age
iCheckMaxAge <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a maximum age in ",nm,".")
  if (is.null(x)) STOP("Need to specify a maximum age in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (!is.wholenumber(x)) WARN("The maximum age is not a whole number,\n",
                               "  please check value in ",nm,".")
}
