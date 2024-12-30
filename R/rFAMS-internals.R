#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in rFAMS.
#'
#' @rdname rFAMS-internals
#' @keywords internal
#' @aliases STOP WARN .onAttach

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
# Check conditional fishing mortality value
iCheckcf <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a conditional fishing mortality in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
}

# Check conditional natural mortality value
iCheckcm <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a conditional natural mortality in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
}

# Check minimum length limit for harvest
iCheckMLH <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a minimum length (mm) limit for harvest in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<100) WARN("A minimum length limit of harvest of ",x," mm seems too small,\n",
                  "please check value in ",nm,".")
  if (x>1600) WARN("A minimum length limit of harvest of ",x," mm seems too large,\n",
                   "  please check value in ",nm,".")
}

# Check initial number of new recruits entering the fishery
iCheckN0 <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify an initial number of new recruits in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (!is.wholenumber(x)) WARN("The initial number of new recruits is not a whole number,\n",
                               "  please check value in ",nm,".")
}

# Check Linf
iCheckLinf <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a mean asymptotic length (mm) in ",nm,".")
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
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check length-weight beta
iCheckLWb <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a weight-length beta coefficient in ",nm,".")
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
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}


# Check maximum age
iCheckMaxAge <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a maximum age in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (!is.wholenumber(x)) WARN("The maximum age is not a whole number,\n",
                               "  please check value in ",nm,".")
}
