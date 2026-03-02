#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in rFAMS.
#'
#' @rdname rFAMS-internals
#' @keywords internal
#' @aliases .onAttach iMakeSWmsg STOP WARN is.wholenumber iIbeta iHndlArgName iErrMore1 iErrNotNumeric iErrLT iErrGt iErrNotVector iCheckLHparms iCheckN0 iCheckMaxAge iCheckLinf iCheckK iCheckt0 iCheckLWb iCheckLWa iCheckloi iCheckCondMort iCheckMLH iCheckrecruitTL iCheckSlotTL iCheckSlotType iCheckLLinf iCheckcfcm_dpm iCheckrec iChecksimyears iCheckspecies iChecknR iCheckminR iCheckmaxR iCheckminRNorm iCheckmaxRNorm iCheckmeanR iChecksdR iCheckmeanRNth iChecknStr iChecksizeStr iCheckmeanRrandInt iCheckavgFreq iChecksizeStrRrandInt isum_by_year

# -- Sends a start-up message to the console when the package is loaded.
.onAttach <- function(libname, pkgname) {
  vers <- read.dcf(system.file("DESCRIPTION",
                               package=pkgname,lib.loc=libname),
                   fields="Version")
  msg <- paste0("## rFAMS v",vers,". See citation('rFAMS') if used in publication.\n")
  msg <- paste0(msg,"## Visit https://github.com/fishR-Core-Team/rFAMS/issues to report any bugs.")
  packageStartupMessage(msg)
}


# ===== Helper Functions
# ----- for use with STOP() and WARN()
iMakeSWmsg <- function(...) {
  # create message, wrapped according to windows size
  strwrap(paste(as.character(list(...)),collapse=""),
          width=0.9*getOption("width"),exdent=2,prefix="\n",initial="")
}

# ----- same as stop() and warning() but w/ call.=FALSE as default & wrapped msg
STOP <- function(...,call.=FALSE,domain=NULL) {
  stop(iMakeSWmsg(...),call.=call.,domain=domain)
}

WARN <- function(...,call.=FALSE,immediate.=FALSE,noBreaks.=FALSE,domain=NULL) {
  warning(iMakeSWmsg(...),call.=call.,immediate.=immediate.,
          noBreaks.=noBreaks.,domain=domain)
}

# ----- Checks if a value is a whole number
is.wholenumber <- function(x,tol=.Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# ----- Incomplete beta function ... see tests for comparison to other packages
iIbeta <- function(x,a,b) {
  if (any(x<0)) STOP("'x' in incomplete beta function must be >=0.")
  if (any(x>1)) STOP("'x' in incomplete beta function must be <=1.")
  if (any(a<0)) STOP("'a' in incomplete beta function must be >=0.")
  if (any(b<0)) STOP("'b' in incomplete beta function must be >=0.")
  beta(a,b)*stats::pbeta(x,a,b)
}

# ----- Get argument name from x, or use optname if it is missing
iHndlArgName <- function(x,optname=NULL) {
  paste0("'",ifelse(x=="",optname,x),"'")
}

# ===== General Error Checks --
# ----- Error if more than one item
iErrMore1 <- function(x,nm) {
  if(length(x)>1) {
    # some checks send name already in singe quotes ... check & adjust for this
    if (!startsWith(nm,"'")) nm <- paste0("'",nm,"'")
    STOP("Only use one value in ",nm,".")
  }
}

# ----- Error if not numeric
iErrNotNumeric <- function(x,nm) {
  if (!is.numeric(x)) STOP(nm," must be a number.")
}

# ----- Error if (any items are) less than value
iErrLT <- function(x,value,nm) {
  if (any(x<value)) {
    # some checks send name already in singe quotes ... check & adjust for this
    if (!startsWith(nm,"'")) nm <- paste0("'",nm,"'")
    pre <- ifelse(length(x)>1,"All ","")
    STOP(pre,nm," must be >=",value,".")
  }
}

# ----- Error if (any items are) greater than value
iErrGT <- function(x,value,nm) {
  if (any(x>value)) {
    # some checks send name already in singe quotes ... check & adjust for this
    if (!startsWith(nm,"'")) nm <- paste0("'",nm,"'")
    pre <- ifelse(length(x)>1,"All ","")
    STOP(pre,nm," must be <=",value,".")
  }
}

# Error if not a vector
iErrNotVector <- function(x,nm) {
  if (!is.vector(x)) STOP(nm," must be a vector.")
}


# ===== Specific Checks ... roughly ordered as general, YPR MinLL, YPR Slot, DPM
# ===== Check life history parameters vector/list
# !!!!! throughout optname= is used to provide a name to the error/warning
#       message for when the argument is missing, as it is not possible to
#       extract an argument name when the argument is missing.
# !!!!! throughout onlyone= is used to help the function distinguish whether it
#       should test whether only one value was provided. This allows checks for
#       both when one value is expected from one function (e.g., yprBH_func())
#       but multiple values may be expected for others (e.g., yprBH_minLL()).
iCheckLHparms <- function(x,optname=NULL) {
  ## check if missing
  if (missing(x))
    STOP("Need to specify a list or vector of life history parameters in '",
         optname,"'.")

  ## !! Only perform checks on x if x is NOT of class "MAKELH" ... in other words
  ##    these tests are not needed (i.e., redundant) if x came from makeLH()
  if (!("MAKELH" %in% class(x))) {
    nm <- iHndlArgName(deparse(substitute(x)),optname)
    if (is.null(x))
      STOP("Need to specify a list or vector of life history parameters in ",nm,".")

    ## check not a data.frame or matrix
    if (is.data.frame(x)) STOP(nm," must be a vector or list, not a data.frame.")
    if (is.matrix(x)) STOP(nm," must be a vector or list, not a matrix.")

    ## Check names
    # set expected names for list/vector
    nms <- c("N0","tmax","Linf","K","t0","LWalpha","LWbeta")
    # get names in vector/list
    gnms <- names(x)
    # check if vector/list is named
    if (is.null(gnms)) STOP("Life history parameters in ",nm," must be named.")
    # check that all required names are in vector/list
    tmp <- nms %in% gnms
    if (!all(tmp)) STOP(nm," is missing these life history parameters: ",
                        paste(nms[!tmp],collapse=", "))
    # check if too many items
    tmp <- gnms %in% nms
    if (!all(tmp)) STOP("These parameters should not be in ",nm,": ",
                        paste(gnms[!tmp],collapse=", "))

    ## Now check that contents are of the right type and magnitude
    N0 <- x[["N0"]]
    iCheckN0(N0)
    tmax <- x[["tmax"]]
    iCheckMaxAge(tmax)
    Linf <- x[["Linf"]]
    iCheckLinf(Linf)
    K <- x[["K"]]
    iCheckK(K)
    t0 <- x[["t0"]]
    iCheckt0(t0)
    LWalpha <- x[["LWalpha"]]
    iCheckLWa(LWalpha)
    LWbeta <- x[["LWbeta"]]
    iCheckLWb(LWbeta)
  }
}

# ----- Check initial number of fish in the population
iCheckN0 <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify an initial number of fish in the population in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  # if (!is.wholenumber(x))
  #   WARN("The initial number in the population is not a whole number,",
  #        " please check value in ",nm,".")
}

# ----- Check maximum age
iCheckMaxAge <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a maximum age in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (!is.wholenumber(x)) WARN("The maximum age in ",nm," is not a whole number.")
}

# ----- Check Linf
iCheckLinf <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a mean asymptotic length (mm) in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<200) WARN("A mean asymptotic length of ",x," mm seems too small,",
                  " please check value in ",nm,".")
  if (x>2000) WARN("A mean asymptotic length of ",x," mm seems too large,",
                   " please check value in ",nm,".")
}

# ----- Check K
iCheckK <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a Brody growth coefficient in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<0.1) WARN("A Brody growth coefficient of ",x," seems too small,",
                  " please check value in ",nm,".")
  if (x>0.6) WARN("A Brody growth coefficient of ",x," seems too large,",
                  " please check value in ",nm,".")
}

# ----- Check t0
iCheckt0 <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a time when the mean length is 0 in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# ----- Check length-weight beta
iCheckLWb <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a weight-length beta coefficient in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<2) WARN("A weight-length beta coefficient of ",x," seems too small,",
                " please check value in ",nm,".")
  if (x>4) WARN("A weight-length beta coefficient of ",x," seems too large,",
                " please check value in ",nm,".")
}

# ----- Check length-weight alpha
iCheckLWa <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a weight-length alpha coefficient in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# ----- Check length of interest "loi" input
iCheckloi <- function(x,optname=NULL) {
  #! loi is often NULL, so just pass-through (don't do anything) if it is
  if (!is.null(x)) {
    nm <- iHndlArgName(deparse(substitute(x)),optname)
    iErrNotVector(x,nm)
    iErrNotNumeric(x,nm)
    iErrLT(x,0,nm)
  }
}

# ----- Check conditional mortality value(s)
iCheckCondMort <- function(x,optname=NULL,onlyone=FALSE) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  # determine type of mortality ... fishing or natural based on nm
  type <- ifelse(startsWith(nm,"'cf"),"fishing","natural")
  # determine extra description for fishing mortality if a Slot limit
  slotdesc <- ""  # default for if nm="cf" or nm="cm"
  if (type=="fishing") {
    if (nm=="'cfunder'") slotdesc <- "under the slot limit"
    else if (nm=="'cfin'") slotdesc <- "in the slot limit"
    else if (nm=="'cfabove'") slotdesc <- "above the slot limit"
  }
  # put a message together
  tmpmsg <- paste0("Need to specify a conditional ",type,
                   " mortality ",slotdesc," in ",nm,".")
  # do the checks
  if (missing(x) || is.null(x)) STOP(tmpmsg)
  iErrNotVector(x,nm)
  if (onlyone) iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
}

# ----- Check minimum length limit for harvest
iCheckMLH <- function(x,Linf,optname=NULL,onlyone=FALSE) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a minimum length (mm) limit for harvest in ",nm,".")
  iErrNotVector(x,nm)
  if (onlyone) iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (any(x>=Linf)) {
    STOP("A minimum length limit of harvest cannot be more than Linf (="
         ,Linf,"), please check values in ",nm,".")
  }
  tmp <- x<100
  if (any(tmp)) {
    tmp <- max(x[tmp])
    WARN("A minimum length limit of harvest of ",tmp," mm seems too small,",
         " please check value(s) in ",nm,".")
  }
  tmp <- x>1600
  if (any(tmp)) {
    tmp <- min(x[tmp])
    WARN("A minimum length limit of harvest of ",tmp," mm seems too large,",
         " please check value(s) in ",nm,".")
  }
}

# ----- Check recruitment total length
iCheckRecruitmentTL <- function(x,Linf,lowerSL) {
  # !! don't check for missing as recruitmentTL is NULL by default or the user had
  #    to change it to something, very unlikely they changed it to missing.
  #    Thus, don't need optname= argument used in other functions
  # !! tests of recruitmentTL relative to type of slot limit is in iCheckSlotType()
  # Don't test, just pass through if recruitmentTL is NULL
  if (!is.null(x)) {
    nm <- iHndlArgName(deparse(substitute(x)))
    iErrMore1(x,nm)
    iErrNotNumeric(x,nm)
    iErrLT(x,0,nm)
    tmp <- paste0("; please check value in ",nm,".")
    if (x>Linf) STOP(nm," cannot be greater than 'Linf' (=",Linf,")",tmp)
    if (x>lowerSL) STOP(nm," cannot be greater than 'lowerSL' (=",lowerSL,")",tmp)
    if (x<50) WARN(nm," of ",x," mm seems too small",tmp)
    if (x>1600) WARN(nm," of ",x," mm seems too large",tmp)
  }
}

# ----- Check slot total length
iCheckSlotTL <- function(x,Linf,optname) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  # determine type of slot length ... lower or upper
  type <- substr(nm,2,6)
  if (missing(x) || is.null(x))
    STOP("Need to specify a ",type," slot limit total length (mm) in ",nm,".")
  # do the checks
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x>Linf) STOP("The ",type," slot limit total length (=",x,
                   ") mm cannot be greater than 'Linf' (=",Linf,
                   "); please check value in ",nm,".")
  if (x<50) WARN("A ",type," slot limit total length of ",x,
                 " mm seems too small, please check value in ",nm,".")
  if (x>1600) WARN("A ",type," slot limit total length of ",x,
                   " mm seems too large, please check value in ",nm,".")
}

# ----- Check combinations of cf values and recruitmentTL for Slot Limits
iCheckSlotType <- function(cfu,cfi,cfa,rtl) {
  # ===== determine what of cfunder, cfin, cfbelow, and recruitmentTL were given
  #       e.g., ug==TRUE if cfunder is given (i.e., >0)
  ug <- cfu>0
  ig <- cfi>0
  ag <- cfa>0
  rg <- !is.null(rtl)

  # ===== parts of messages
  tmp1p <- paste0("It appears you are trying to simulate a protected slot",
                  " (i.e., 'recruitmentTL'>0). If so, ")
  tmp1h <- paste0("It appears you are trying to simulate an inverse/harvest slot",
                  " (i.e., 'recruitmentTL'=NULL). If so, ")

  # ===== perform checks for cfs relative to type of slot
  # ----- Stop immediately if no mortality rates are given
  if (!ug & !ig & !ag)
    STOP("'cfunder', 'cfin', and 'cfabove' cannot all =0.") #1
  # ----- check for combinations of cfs and recruitmentTL
  if (rg & ug & ig & ag)
    STOP(tmp1p,"'cfin' should =0.")  #2
  else if (!rg & ug & ig & ag)
    STOP(tmp1h,"'cfunder' and 'cfabove' should =0.") #3
  else if (rg & !ug & ig & !ag)
    STOP("'cfin'>0, 'cfunder'=0, and 'cfabove'=0 implies you are trying to simulate an ",
         "inverse/harvest slot. If so, 'recruitmentTL' must not be 'NULL'.")
  else if (!rg & ug & !ig & ag)
    STOP("'cfin'=0, 'cfunder'>0, and 'cfabove'>0 implies you are trying to simulate a ",
         "protected slot. If so, 'recruitmentTL' must not be 'NULL'.") #5
  else if (rg & ug & ig & !ag)
    STOP(tmp1p,"'cfin' should =0 and 'cfabove' (along with 'cfunder') should be >0.") #6
  else if (!rg & ug & ig & !ag)
    STOP(tmp1h,"'cfunder' (along with 'cfabove') should =0.") #7
  else if (rg & !ug & ig & ag)
    STOP(tmp1p,"'cfin' should =0 and 'cfunder' (along with 'cfabove') should be >0.") #8
  else if (!rg & !ug & ig & ag)
    STOP(tmp1h,"'cfabove' (along with 'cfunder') should =0.") #9
  else if (rg & ug & !ig & !ag)
    STOP(tmp1p,"'cfabove' (along with 'cfunder') should be >0.") #10
  else if (!rg & ug & !ig & !ag)
    STOP(tmp1h,"'cfin' should be >0 and 'cfunder' (along with 'cfabove') should be 0.") #11
  else if (rg & ug & !ig & !ag)
    STOP(tmp1p,"'cfabove' (along with 'cfunder') should be >0.") #12
  else if (!rg & !ug & !ig & ag)
    STOP(tmp1h,"'cfin' should be >0 and 'cfabove' (along with 'cfunder') should be 0.") #13
  else if (rg & !ug & !ig & ag)
    STOP(tmp1p,"'cfunder' (along with 'cfabove') should =0.") #14
  # !!!!! rg, ug, !ig, ag is a good protected slot ... so no STOP() #15
  #       !rg, !ug, ig, !ag is a good inverse/harvest slot ... so no STOP() #16
}



iCheckCondMortForSlot <- function(cfu,cfi,cfa,rtl) {
  # determine what of cfunder, cfin, cfbelow, and recruitmentTL were given
  ugiven <- cfu>0
  igiven <- cfi>0
  agiven <- cfa>0
  rtlgiven <- !is.null(rtl)

  # determine problems with combos of cfunder, cfin, and cfabove
  if (all(c(ugiven,igiven,agiven))) {
    tmp1 <- "'cfunder', 'cfin', and 'cfabove' cannot all be >0. "
  } else if (all(c(!ugiven,!igiven,!agiven))) {
    STOP("'cfunder', 'cfin', and 'cfabove' cannot all =0.")
  } else if (igiven & (ugiven | agiven)) {
    tmp1 <- "If 'cfin'>0 then neither 'cfunder' or 'cfabove' may be >0. "
  } else if (!igiven & (!ugiven | !agiven)) {
    tmp1 <- "If 'cfin'=0 then both 'cfunder' and 'cfabove' should be >0. "
  } else tmp1 <- NULL  ## no problems

  if (!is.null(tmp1)) { ## there was a problem so STOP needs to be used
    # modify message depending on whether recruitmentTL was given or no
    if (rtlgiven) {
      tmp2 <- paste("You have provided a 'recruitmentTL' which implies you",
                    "want to simulate a protected slot. Use 'cfin'=0,",
                    "'cfunder'>0, 'cfabove'>0 and a 'recruitmentTL'",
                    "to simulate a 'protected slot'. ")
    } else {
      tmp2 <- paste("You have not provided a 'recruitmentTL' which implies you",
                    "want to simulate an inverse/harvest slot. Use 'cfin'>0,",
                    "'cfunder'=0, and 'cfabove'=0 and no 'recruitmentTL'",
                    "to simulate an inverse/harvest slot. ")
    }
    # send message
    STOP(tmp1,tmp2,"Please check your values in 'cfunder', ",
                           "'cfin', 'cfabove', and 'recruitmentTL'.")
  }
}

# Check Linf > Minimum length
iCheckLLinf <- function(x, Linf) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (sum(x > Linf) >0 ) STOP("Harvest lengths in the vector (", nm, ") can't be greater than Linf")
}



#Check that cf and cm are a numeric matrix
iCheckcfcm_dpm <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if(!is.matrix(x))
    STOP(nm, " must be a matrix")
  if(!is.numeric(x))
    STOP(nm, " must be a numeric data type")
}

#Check that recruitment is a numeric vector
iCheckrec <- function(rec) {
  if(!is.vector(rec))
    STOP("rec must be a vector")
  if(!is.numeric(rec))
    STOP("rec must be a numeric data type")
}

# Check simyears
iChecksimyears <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the number of simulations to run in ",nm,".")
  if (is.null(x)) STOP("Need to specify the number of simulations to run in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (!is.wholenumber(x)) WARN("The numer of simulation years is not a whole number,",
                               " please check value in ",nm,".")
}

iCheckspecies <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a species name in ",nm,". See the FSA::PSDlit function for a list of available species")
  if (is.null(x)) STOP("Need to specify a species name in ",nm,". See the FSA::PSDlit function for a list of available species")
}

# Check nR
iChecknR <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the number of fixed recruits per year in ",nm,".")
  if (is.null(x)) STOP("Need to specify the number of fixed recruits per year in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check minR with uniform distribution
iCheckminR <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the minimum number of recruits for the uniform method in ",nm,".")
  if (is.null(x)) STOP("Need to specify the minimum number of recruits for the uniform method in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check maxR with uniform distribution
iCheckmaxR <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the maximum number of recruits for the uniform method in ",nm,".")
  if (is.null(x)) STOP("Need to specify the maximum number of recruits for the uniform method in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check minR with normal distribution
iCheckminRNorm <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the minimum number of recruits for the normal method in ",nm,".")
  if (is.null(x)) STOP("Need to specify the minimum number of recruits for the normal method in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check maxR with normal distribution
iCheckmaxRNorm <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the maximum number of recruits for the normal method in ",nm,".")
  if (is.null(x)) STOP("Need to specify the maximum number of recruits for the normal method in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check meanR with normal distribution
iCheckmeanR <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the mean number of recruits for the normal method in ",nm,".")
  if (is.null(x)) STOP("Need to specify the mean number of recruits for the normal method in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check sdR with normal distribution
iChecksdR <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the standard deviation of recruits for the normal method in ",nm,".")
  if (is.null(x)) STOP("Need to specify the standard deviation of recruits for the normal method in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check meanR with StrYC_Nth
iCheckmeanRNth <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the mean number of recruits for the strong year class every Nth year method in ",nm,".")
  if (is.null(x)) STOP("Need to specify the mean number of recruits for the strong year class every Nth year method in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check nStr with StrYC_Nth
iChecknStr <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the Nth year that a strong year class will occur in ",nm,".")
  if (is.null(x)) STOP("Need to specify the Nth year that a strong year class will occur in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check sizeStr with StrYC_Nth
iChecksizeStr <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the multiplier for the strong year class relative to meanR for the strong year class every Nth year method in ",nm,".")
  if (is.null(x)) STOP("Need to specify the multiplier for the strong year class relative to meanR for the strong year class every Nth year method in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}


# Check meanR with randInt
iCheckmeanRrandInt <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the mean number of recruits for the strong year class at random intervals in ",nm,".")
  if (is.null(x)) STOP("Need to specify the mean number of recruits for the strong year class at random intervals in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check avgFreq with randInt
iCheckavgFreq <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the average frequency of a strong year class for the random intervals in ",nm,".")
  if (is.null(x)) STOP("Need to specify the average frequency of a strong year class for the random intervals in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

# Check sizeStr with randInt
iChecksizeStrRrandInt <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the multiplier for the strong year class relative to meanR for the random interval in ",nm,".")
  if (is.null(x)) STOP("Need to specify the multiplier for the strong year class relative to meanR for the random interval in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

#Summarize dynamic pool model by year
isum_by_year <- function(res,species,group){
  year<-gcat<-nstart<-count<-quality<-stock<-preferred<-memorable<-trophy<-age<-yield<-biomass<-nharvest<-ndie<-age_1plus<-Yield_age_1plus<-Total_biomass<-nharvest_age_1plus<-ndie_age_1plus<-NULL
  #Calculate PSD's based on number of individuals at length at the start of the year
  #Return a simplified object for calculation of PSD
  if(is.null(group)){
    psd.cuts <- FSA::psdVal(species, units = "mm")
  }else{
    psd.cuts <- FSA::psdVal(species, group=group, units = "mm")
  }

  #Return PSD age cuts
  psd.age.cuts<-rep(0,6)
  psd.age.cuts[1] <- ((log(1-unname(psd.cuts[1])/res$Linf[1]))/-res$K[1])+res$t0[1]
  psd.age.cuts[2] <- ((log(1-unname(psd.cuts[2])/res$Linf[1]))/-res$K[1])+res$t0[1]
  psd.age.cuts[3] <- ((log(1-unname(psd.cuts[3])/res$Linf[1]))/-res$K[1])+res$t0[1]
  psd.age.cuts[4] <- ((log(1-unname(psd.cuts[4])/res$Linf[1]))/-res$K[1])+res$t0[1]
  psd.age.cuts[5] <- ((log(1-unname(psd.cuts[5])/res$Linf[1]))/-res$K[1])+res$t0[1]
  psd.age.cuts[6] <- ((log(1-unname(psd.cuts[6])/res$Linf[1]))/-res$K[1])+res$t0[1]


  psd_calc<-res |>
    dplyr::mutate(
      gcat = dplyr::case_when(
        age < psd.age.cuts[2] ~ names(psd.cuts[1]),
        age < psd.age.cuts[3] ~ names(psd.cuts[2]),
        age < psd.age.cuts[4] ~ names(psd.cuts[3]),
        age < psd.age.cuts[5] ~ names(psd.cuts[4]),
        age < psd.age.cuts[6] ~ names(psd.cuts[5]),
        TRUE ~ names(psd.cuts[6])
      ))


  # it is unclear how FAMS calculates PSD. Output shows number at PSD categories
  # however, using those numbers do not match reported PSD's

  # Add length category to output
  year_summary <- psd_calc |>
    dplyr::group_by(year,gcat,length) |>
    dplyr::summarise(count = floor(sum(nstart))) |>
    tidyr::uncount(count)

  psd_crosstab <- stats::xtabs(~year + gcat, data = year_summary) #create crosstab
  psd_summary <- as.data.frame.matrix(psd_crosstab) #convert to dataframe
  psd_summary <- cbind(year = as.numeric(row.names(psd_crosstab)), psd_summary) #add row names for year

  # psdCalc(~length,data=year_summary,"Striped Bass", group="landlocked", units = "mm")

  # Add missing columns for calculating PSD
  tmp<-c()
  for(x in 1:length(names(psd.cuts))){
    if(names(psd.cuts)[x] %in% names(psd_summary)==FALSE){
      #tmp <- c(tmp,names(psd.cuts)[x])
      psd_summary[,names(psd.cuts)[x]] = 0
    }

  }

  # calculate PSD, PSD_P, PSD_M, PSD_T
  psd_summary <- psd_summary |>
    dplyr::mutate(PSD = (quality + preferred + memorable + trophy) / (stock + quality + preferred+ memorable + trophy),
                  PSD_P = (preferred + memorable + trophy) / (stock + quality + preferred+ memorable + trophy),
                  PSD_M = (memorable + trophy) / (stock + quality + preferred+ memorable + trophy),
                  PSD_T = trophy / (stock + quality + preferred+ memorable + trophy) ,
                  year = as.integer((year)))

  psd_summary[is.na(psd_summary)] <- 0 #replace NaN with 0

  # summary for age-1+
  Year_Summary <- res |>
    dplyr::filter(age > 0) |>
    dplyr::group_by(year) |>
    dplyr::summarize(age_1plus = sum(nstart), Yield_age_1plus = sum(yield),
                     Total_biomass = sum(biomass), nharvest_age_1plus = sum(nharvest),
                     ndie_age_1plus = sum(ndie)) |>
    dplyr::right_join(psd_summary, by = "year") |>
    dplyr::mutate(dplyr::across(c(age_1plus, Yield_age_1plus, Total_biomass, nharvest_age_1plus, ndie_age_1plus), ~dplyr::coalesce(., 0)))

  # merged_df <- dplyr::left_join(psd_summary,Year_Summary, by = "year") |>
  #   dplyr::mutate(dplyr::across(c(age_1plus, Yield_age_1plus, Total_biomass, N_harvest_age_1plus, N_die_age_1plus), ~dplyr::coalesce(., 0)))


  return(Year_Summary)
}




## =============================================================================
## ==== OLD CAN PROBABLY BE DELETED
## =============================================================================

# # Check min length at harvest increments (min/max should be checked prior),
# #   return sequence if everything looks good
# iCheckMLHinc <- function(xinc,xmin,xmax) {
#   ## checks of increment
#   nm <- paste0("'",deparse(substitute(xinc)),"'")
#   if (missing(xinc))
#     STOP("Need to specify an increment for minimum length (mm) limit for harvest in ",nm,".")
#   if (is.null(xinc))
#     STOP("Need to specify an increment for minimum length (mm) limit for harvest in ",nm,".")
#   iErrMore1(xinc,nm)
#   iErrNotNumeric(xinc,nm)
#   iErrLT(xinc,0,nm)
#   ## Check min vs max
#   nm1 <- paste0("'",deparse(substitute(xmin)),"'")
#   nm2 <- paste0("'",deparse(substitute(xmax)),"'")
#   if(xmin>xmax) STOP(nm1," must be equal to or less than ",nm2,".")
#   res <- seq(xmin,xmax,xinc)
#   if (length(res)>100)
#     WARN("Choices of ",nm1,", ",nm2,", and ",nm," resulted in ",length(res),
#          " values./n","  Depending on other choices the simulation may be slow.")
#   ## Return sequence
#   res
# }


# # Check conditional mortality increments (min/max should be checked prior),
# #   return sequence if everything looks good
# iCheckcfminc <- function(xinc,xmin,xmax) {
#   ## checks of increment
#   nm <- paste0("'",deparse(substitute(xinc)),"'")
#   if (missing(xinc))
#     STOP("Need to specify an increment for conditional natural mortality in ",nm,".")
#   if (is.null(xinc))
#     STOP("Need to specify an increment for conditional natural mortality in ",nm,".")
#   iErrMore1(xinc,nm)
#   iErrNotNumeric(xinc,nm)
#   iErrLT(xinc,0,nm)
#   iErrGT(xinc,1,nm)
#   ## Check min vs max
#   nm1 <- paste0("'",deparse(substitute(xmin)),"'")
#   nm2 <- paste0("'",deparse(substitute(xmax)),"'")
#   if(xmin>xmax) STOP(nm1," must be equal to or less than ",nm2,".")
#   res <- round(seq(xmin,xmax,xinc),8)
#   if (length(res)>100)
#     WARN("Choices of ",nm1,", ",nm2,", and ",nm," resulted in ",length(res),
#          " values.","  Depending on other choices the simulation may be slow.")
#   ## Return sequence
#   res
# }


# # Check length of interest "mLL" input
# iCheckminLL <- function(x,type=NULL){
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
#   if (missing(x)) STOP("Need to specify a",type,
#                        " vector for minimum length limits in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a ",type,
#                        " vector for minimum length limits in ",nm,".")
#   iErrNotVector(x,nm)
#   iErrNotNumeric(x,nm)
# }

# # Check conditional fishing mortality value
# iCheckcf <- function(x,type=NULL) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
#   if (missing(x)) STOP("Need to specify a",type,
#                        " conditional fishing mortality in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a ",type,
#                        " conditional fishing mortality in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
#   iErrLT(x,0,nm)
#   iErrGT(x,1,nm)
# }

# # Check slot limit lengths and cf
# iCheckSlotType <- function(recruitmentTL,lowerSL,upperSL,cfunder,cfin,cfabove) {
#   if(cfunder > 0 & is.null(recruitmentTL)){
#     STOP("cfunder is specified for harvest under the slot and no length is specified for recruitmentTL. You must specify a recruitmentTL to indicate what length fish are likely to beharvested.")
#   }
#
# }

# # Check lower slot limit total length
# iChecklowerSLTL <- function(x,type="") {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify a lower slot limit total length (mm) in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a lower slot limit total length (mm) in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
#   iErrLT(x,0,nm)
#   if (x<50) WARN("A lower slot limit total length of ",x," mm seems too small,",
#                  " please check value in ",nm,".")
#   if (x>1600) WARN("A lower slot limit total length of ",x," mm seems too large,",
#                    " please check value in ",nm,".")
# }

# # Check lower slot limit total length
# iCheckupperSLTL <- function(x,type="") {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify an upper slot limit total length (mm) in ",nm,".")
#   if (is.null(x)) STOP("Need to specify an upper slot limit total length (mm) in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
#   iErrLT(x,0,nm)
#   if (x<50) WARN("An upper slot limit total length of ",x," mm seems too small,",
#                  " please check value in ",nm,".")
#   if (x>1600) WARN("An upper slot limit total length of ",x," mm seems too large,",
#                    " please check value in ",nm,".")
# }

# #Check recruitment, lower slot, and upper slot are in proper order
# iCheckslotOrder <- function(recruitmentTL, lowerSL, upperSL) {
#   ## Check min vs max
#   nm1 <- paste0("'",deparse(substitute(recruitmentTL)),"'")
#   nm2 <- paste0("'",deparse(substitute(lowerSL)),"'")
#   nm3 <- paste0("'",deparse(substitute(upperSL)),"'")
#   if(!is.null(recruitmentTL)){
#     if(recruitmentTL>lowerSL) STOP(nm1," must be less than ",nm2,".")
#     if(recruitmentTL>upperSL) STOP(nm1," must be less than ",nm3,".")
#   }
#   if(lowerSL>upperSL) STOP(nm2," must be less than ",nm3,".")
# }

# # Check conditional natural mortality value
# iCheckcm <- function(x,type=NULL) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
#   if (missing(x)) STOP("Need to specify a",type,
#                        " conditional natural mortality in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a",type,
#                        " conditional natural mortality in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
#   iErrLT(x,0,nm)
#   iErrGT(x,1,nm)
# }

# # Check conditional fishing mortality value under slot
# iCheckcfunder <- function(x,type=NULL) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
#   if (missing(x)) STOP("Need to specify a",type,
#                        " conditional fishing mortality under the slot limit in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a",type,
#                        " conditional fishing mortality under the slot limit in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
#   iErrLT(x,0,nm)
#   iErrGT(x,1,nm)
# }

# # Check conditional fishing mortality value in slot
# iCheckcfin <- function(x,type=NULL) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
#   if (missing(x)) STOP("Need to specify a",type,
#                        " conditional fishing mortality in the slot limit in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a",type,
#                        " conditional fishing mortality in the slot limit in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
#   iErrLT(x,0,nm)
#   iErrGT(x,1,nm)
# }

# # Check conditional fishing mortality value under slot
# iCheckcfabove <- function(x,type=NULL) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
#   if (missing(x)) STOP("Need to specify a",type,
#                        " conditional fishing mortality above the slot limit in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a",type,
#                        " conditional fishing mortality above the slot limit in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
#   iErrLT(x,0,nm)
#   iErrGT(x,1,nm)
# }

# # Check length of interest "mLL" input
# iCheckcfVect <- function(x,type=NULL){
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
#   if (missing(x)) STOP("Need to specify a",type,
#                        " conditional fishing mortality vector in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a ",type,
#                        " conditional fishing mortality vector in ",nm,".")
#   iErrNotVector(x,nm)
#   iErrNotNumeric(x,nm)
# }

# # Check length of interest "mLL" input
# iCheckcmVect <- function(x,type=NULL){
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
#   if (missing(x)) STOP("Need to specify a",type,
#                        " conditional natural mortality in ",nm,".")
#   if (is.null(x)) STOP("Need to specify a ",type,
#                        " conditional natural mortality in ",nm,".")
#   iErrNotVector(x,nm)
#   iErrNotNumeric(x,nm)
# }

