#' Sends a start-up message to the console when the package is loaded.
#' @noRd
.onAttach <- function(libname, pkgname) {
  vers <- read.dcf(system.file("DESCRIPTION",
                               package=pkgname,lib.loc=libname),
                   fields="Version")
  msg <- paste0("## rFAMS v",vers,". See citation('rFAMS') if used in publication.\n")
  msg <- paste0(msg,"## Visit https://github.com/fishR-Core-Team/rFAMS/issues to report any bugs.")
  packageStartupMessage(msg)
}


# ===== Helper Functions
#' Wraps an error or warning message for use with STOP() and WARN()
#' @noRd
iMakeSWmsg <- function(...) {
  # create message, wrapped according to windows size
  strwrap(paste(as.character(list(...)),collapse=""),
          width=0.9*getOption("width"),exdent=2,prefix="\n",initial="")
}

#' A modification of stop() with call.=FALSE as default and wrapped message
#' @keywords internal
STOP <- function(...,call.=FALSE,domain=NULL) {
  stop(iMakeSWmsg(...),call.=call.,domain=domain)
}

#' A modification of warning() with call.=FALSE as default and wrapped message
#' @keywords internal
WARN <- function(...,call.=FALSE,immediate.=FALSE,noBreaks.=FALSE,domain=NULL) {
  warning(iMakeSWmsg(...),call.=call.,immediate.=immediate.,
          noBreaks.=noBreaks.,domain=domain)
}

#' Checks if a value is a whole number
#' @keywords internal
is.wholenumber <- function(x,tol=.Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

#' Incomplete beta function ... see tests for comparison to other packages
#' @keywords internal
iIbeta <- function(x,a,b) {
  if (any(x<0)) STOP("'x' in incomplete beta function must be >=0.")
  if (any(x>1)) STOP("'x' in incomplete beta function must be <=1.")
  if (any(a<0)) STOP("'a' in incomplete beta function must be >=0.")
  if (any(b<0)) STOP("'b' in incomplete beta function must be >=0.")
  beta(a,b)*stats::pbeta(x,a,b)
}

#' A helper to extract name from argument sent in x, or use optname if x is missing
#' @keywords internal
iHndlArgName <- function(x,optname=NULL) {
  paste0("'",ifelse(x=="",optname,x),"'")
}

# ===== General Error Checks
#' Error if more than one item in x
#' @keywords internal
iErrMore1 <- function(x,nm) {
  if(length(x)>1) {
    # some checks send name already in singe quotes ... check & adjust for this
    if (!startsWith(nm,"'")) nm <- paste0("'",nm,"'")
    STOP("Only use one value in ",nm,".")
  }
}

#' Error if x is not numeric
#' @keywords internal
iErrNotNumeric <- function(x,nm) {
  if (!is.numeric(x)) STOP(nm," must be a number.")
}

#' Error if x is (or any items in x are) less than value
#' @keywords internal
iErrLT <- function(x,value,nm) {
  if (any(x<value)) {
    # some checks send name already in singe quotes ... check & adjust for this
    if (!startsWith(nm,"'")) nm <- paste0("'",nm,"'")
    pre <- ifelse(length(x)>1,"All ","")
    STOP(pre,nm," must be >=",value,".")
  }
}

#' Error if x (or any items in x are) greater than value
#' @keywords internal
iErrGT <- function(x,value,nm) {
  if (any(x>value)) {
    # some checks send name already in singe quotes ... check & adjust for this
    if (!startsWith(nm,"'")) nm <- paste0("'",nm,"'")
    pre <- ifelse(length(x)>1,"All ","")
    STOP(pre,nm," must be <=",value,".")
  }
}

#' Error if x is not a vector
#' @keywords internal
iErrNotVector <- function(x,nm) {
  if (!is.vector(x) | !is.atomic(x)) STOP(nm," must be a vector.")
}


# ===== Specific Checks ... roughly ordered as general, YPR MinLL, YPR Slot, DPM

#' Make checks on life history parameters vector/list
#' @param x a list/vector of seven life history parameters, preferably constructed with `makeLH()`
#' @param optname a name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
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

#' Make checks of the initial number of fish in the population
#' @param x A value of N0
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
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

#' Make checks of the maximum age (usually sent as tmax)
#' @param x A value of maximum age.
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
iCheckMaxAge <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a maximum age in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (!is.wholenumber(x)) WARN("The maximum age in ",nm," is not a whole number.")
}

#' Make checks of LVB Linf parameter
#' @param x A value of Linf.
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
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

#' Make checks of LVB K parameter
#' @param x A value of K
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
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

#' Make checks of LVB t0 parameter
#' @param x A value of t0
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
iCheckt0 <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a time when the mean length is 0 in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

#' Make checks of length-weight regression beta parameter
#' @param x A value of beta from a length-weight regression.
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
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

#' Make checks of length-weight regression alpha parameter
#' @param x A value of alpha from a length-weight regression
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
iCheckLWa <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a weight-length alpha coefficient in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
}

#' Make checks of length of interest values (usually sent in loi)
#' @param x A vector (or value) for a "length-of-interest".
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
iCheckloi <- function(x,optname=NULL) {
  #! loi is often NULL, so just pass-through (don't do anything) if it is
  if (!is.null(x)) {
    nm <- iHndlArgName(deparse(substitute(x)),optname)
    iErrNotVector(x,nm)
    iErrNotNumeric(x,nm)
    iErrLT(x,0,nm)
  }
}

#' Make checks of conditional mortality value(s)
#' @param x A vector (or value) of a conditional mortality.
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @param onlyone A logical to help the function distinguish whether it should test whether only one value was provided. This allows checks for both when one value is expected from one function (e.g., `yprBH_func()`) but multiple values may be expected for others (e.g., `yprBH_minLL()`).
#' @keywords internal
iCheckCondMort <- function(x,optname=NULL,onlyone=FALSE) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  # determine type of mortality ... fishing or natural based on nm
  type <- ifelse(startsWith(nm,"'cf"),"fishing","natural")
  # determine extra description for fishing mortality if a Slot limit
  slotdesc <- ""  # default for if nm="cf" or nm="cm"
  if (type=="fishing") {
    if (nm=="'cfBelow'") slotdesc <- "under the slot limit"
    else if (nm=="'cfIn'") slotdesc <- "in the slot limit"
    else if (nm=="'cfAbove'") slotdesc <- "above the slot limit"
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

#' Make checks of minimum length limit for harvest value
#' @param x A vector (or value) of minimum length limits of harvest.
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @param onlyone A logical to help the function distinguish whether it should test whether only one value was provided. This allows checks for both when one value is expected from one function (e.g., `yprBH_func()`) but multiple values may be expected for others (e.g., `yprBH_minLL()`).
#' @keywords internal
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

#' Make checks of recruitment total length
#' @param x A recruitment total length value.
#' @param Linf A value of Linf.
#' @param lowerSL A value for the lower slot limit length.
#' @keywords internal
#' @details
#' Don't check for missing as `recruitmentTL` is `NULL` by default in the major functions or the user changed it to something (very unlikely they changed it to missing). Thus, don't need `optname=` argument used in other functions.
#'
#' Tests of `recruitmentTL` relative to the type of slot limit are in `iCheckSlotType()`.
#'
#' If `recruitmentTL=NULL`, just pass through, don't do any tests.
iCheckRecruitmentTL <- function(x,Linf,lowerSL) {
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

#' Make checks of slot total length value
#' @param x A slot total length value (lower or upper),
#' @param Linf A value of Linf
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
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

#' Make checks of combinations of `cf` values and `recruitmentTL` for slot limits
#' @param cfu A `cfBelow` value.
#' @param cfi A `cfIn` value.
#' @param cfa A `cfAbove` value.
#' @param rtl A `recruitmentTL` value.
#' @param strict A logical that indicates how strict the test should be. See details.
#' @keywords internal
#'
#' @details
#' `strict` is a logical that indicates whether strict criterion for values of `recruitmentTL`, `cfBelow`, `cfIn`, and `cfAbove` should be used. If `strict=TRUE` then the only accepted combinations are that a `recruitmentTL` is given (i.e., not `NULL`), `cfBelow`>0, `cfAbove`>0, and `cfIn`=0 (i.e., simulating a protected slot) or `recruitmentTL` is `NULL`, `cfBelow`=0, `cfAbove`=0, and `cfIn`>0 (i.e., simulating an inverse/harvest slot). If `strict=FALSE` then the only restrictions are that the three `cf`s cannot all =0, and that if `cfBelow` is given them `recruitmentTL` cannot be `NULL`. **This argument allows us to model each type of restrictions while we ultimately decide which one to use.**
iCheckSlotType <- function(cfu,cfi,cfa,rtl,strict=TRUE) {
  # ===== determine what of cfBelow, cfIn, cfbelow, and recruitmentTL were given
  #       e.g., ug==TRUE if cfBelow is given (i.e., >0)
  ug <- cfu>0
  ig <- cfi>0
  ag <- cfa>0
  rg <- !is.null(rtl)

  # ===== Stop immediately if no mortality rates are given
  if (!ug & !ig & !ag) STOP("'cfBelow', 'cfIn', and 'cfAbove' cannot all =0.") #1

  # ===== Check for combos of cfs and recruitmentTL by slot type
  if (!strict) {
    # ===== do not use strict criterion
    if (ug & !rg)
      STOP("If 'cfBelow'>0 then a value must be given to 'recruitmentLT' ",
           "(i.e., it cannot be NULL)")
  } else {
    # ===== use strict criterion
    # ----- parts of messages
    tmp1p <- paste0("It appears you are trying to simulate a protected slot",
                    " (i.e., 'recruitmentTL'>0). If so, ")
    tmp1h <- paste0("It appears you are trying to simulate an inverse/harvest slot",
                    " (i.e., 'recruitmentTL'=NULL). If so, ")

    # ..... check for combinations of cfs and recruitmentTL
    if (rg & ug & ig & ag)
      STOP(tmp1p,"'cfIn' should =0.")  #2
    else if (!rg & ug & ig & ag)
      STOP(tmp1h,"'cfBelow' and 'cfAbove' should =0.") #3
    else if (rg & !ug & ig & !ag)
      STOP("'cfIn'>0, 'cfBelow'=0, and 'cfAbove'=0 implies you are trying to simulate an ",
           "inverse/harvest slot. If so, 'recruitmentTL' must be 'NULL'.")
    else if (!rg & ug & !ig & ag)
      STOP("'cfIn'=0, 'cfBelow'>0, and 'cfAbove'>0 implies you are trying to simulate a ",
           "protected slot. If so, 'recruitmentTL' must not be 'NULL'.") #5
    else if (rg & ug & ig & !ag)
      STOP(tmp1p,"'cfIn' should =0 and 'cfAbove' (along with 'cfBelow') should be >0.") #6
    else if (!rg & ug & ig & !ag)
      STOP(tmp1h,"'cfBelow' (along with 'cfAbove') should =0.") #7
    else if (rg & !ug & ig & ag)
      STOP(tmp1p,"'cfIn' should =0 and 'cfBelow' (along with 'cfAbove') should be >0.") #8
    else if (!rg & !ug & ig & ag)
      STOP(tmp1h,"'cfAbove' (along with 'cfBelow') should =0.") #9
    else if (rg & ug & !ig & !ag)
      STOP(tmp1p,"'cfAbove' (along with 'cfBelow') should be >0.") #10
    else if (!rg & ug & !ig & !ag)
      STOP(tmp1h,"'cfIn' should be >0 and 'cfBelow' (along with 'cfAbove') should be 0.") #11
    else if (rg & ug & !ig & !ag)
      STOP(tmp1p,"'cfAbove' (along with 'cfBelow') should be >0.") #12
    else if (!rg & !ug & !ig & ag)
      STOP(tmp1h,"'cfIn' should be >0 and 'cfAbove' (along with 'cfBelow') should be 0.") #13
    else if (rg & !ug & !ig & ag)
      STOP(tmp1p,"'cfBelow' (along with 'cfAbove') should =0.") #14
    # !!!!! rg, ug, !ig, ag is a good protected slot ... so no STOP() #15
    #       !rg, !ug, ig, !ag is a good inverse/harvest slot ... so no STOP() #16
  }
}

#' Make check on label given to yprBH_SlotLL
#' @param x A character string that represents a label
#' @details Just pass through if `NULL`.
#' @keywords internal
iChecklabel <- function(x) {
  if (!is.null(x)) {
    nm <- iHndlArgName(deparse(substitute(x)))
    iErrMore1(x,nm)
    if (!is.character(x)) STOP(nm," must be a character.")
    if (x=="") STOP("String in ",nm,"is empty.")
  }
}

#' Make checks on number of recruits vector
#' @param x A recruitment vector
#' @details If `x` was created by `genRecruits()` then checking is skipped here as it would have been done there.
#' @keywords internal
iCheckrec <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a vector of recruitment abundance in ",nm,".")
  if (!any(class(x)=="GENREC")) {
    iErrNotVector(x,nm)
    iErrNotNumeric(x,nm)
    iErrLT(x,0,nm)
  }
}

#' Make check on number of years to simulate
#' @param x A single number for number of years to simulate
#' @keywords internal
iChecksimyears <- function(x,optname=NULL) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  if (missing(x) || is.null(x))
    STOP("Need to specify a number of years to simulate in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,1,nm)
  if (!is.wholenumber(x)) STOP(nm," must be a whole number of years.")
}

#' Make checks of conditional mortality matrix for DPM functions
#' @param x A matrix of conditional mortality values.
#' @param syrs Number of simulation years (i.e., `simyears`)
#' @param tmx Maximum age (i.e., `tmax`)
#' @param optname A name to the error/warning  message for when the argument is missing, as it is not possible to extract an argument name when the argument is missing.
#' @keywords internal
iCheckCondMort2 <- function(x,syrs,tmx,optname) {
  nm <- iHndlArgName(deparse(substitute(x)),optname)
  # determine type of mortality ... fishing or natural based on nm
  type <- ifelse(startsWith(nm,"'cf"),"fishing","natural")
  # do the checks
  if (missing(x) || is.null(x))
    STOP(paste0("Need to specify a matrix of conditional ",type,
                " mortalities in ",nm,"."))
  if (!is.matrix(x))
    STOP(nm," must be a matrix of conditional ",type," mortalities.")
  if (!is.numeric(x)) STOP(nm," must be a numeric matrix.")
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
  if (nrow(x)!=syrs)
    STOP("Number of rows in ",nm," (=",nrow(x),
         ") must equal 'simyears' (=",syrs,").")
  if (ncol(x)!=(tmx+1))
    STOP("Number of columns in ",nm," (",ncol(x),
         ") must equal 'tmax'+1 (=",tmx+1,").")
}

iCheckspecies <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a species name in ",nm,". See the FSA::PSDlit function for a list of available species")
  if (is.null(x)) STOP("Need to specify a species name in ",nm,". See the FSA::PSDlit function for a list of available species")
}

# Summarize dynamic pool model by year
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
  Year_Summary <- as.data.frame(Year_Summary)
  return(Year_Summary)
}


# ====== Defunct (can probably be deleted)

# # Check Linf > Minimum length
# iCheckLLinf <- function(x, Linf) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (sum(x > Linf) >0 ) STOP("Harvest lengths in the vector (", nm, ") can't be greater than Linf")
# }
#
# #Check that cf and cm are a numeric matrix
# iCheckcfcm_dpm <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if(!is.matrix(x))
#     STOP(nm, " must be a matrix")
#   if(!is.numeric(x))
#     STOP(nm, " must be a numeric data type")
# }
#
# # Check nR
# iChecknR <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the number of fixed recruits per year in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the number of fixed recruits per year in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check minR with uniform distribution
# iCheckminR <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the minimum number of recruits for the uniform method in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the minimum number of recruits for the uniform method in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check maxR with uniform distribution
# iCheckmaxR <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the maximum number of recruits for the uniform method in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the maximum number of recruits for the uniform method in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check minR with normal distribution
# iCheckminRNorm <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the minimum number of recruits for the normal method in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the minimum number of recruits for the normal method in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check maxR with normal distribution
# iCheckmaxRNorm <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the maximum number of recruits for the normal method in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the maximum number of recruits for the normal method in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check meanR with normal distribution
# iCheckmeanR <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the mean number of recruits for the normal method in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the mean number of recruits for the normal method in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check sdR with normal distribution
# iChecksdR <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the standard deviation of recruits for the normal method in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the standard deviation of recruits for the normal method in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check meanR with StrYC_Nth
# iCheckmeanRNth <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the mean number of recruits for the strong year class every Nth year method in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the mean number of recruits for the strong year class every Nth year method in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check nStr with StrYC_Nth
# iChecknStr <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the Nth year that a strong year class will occur in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the Nth year that a strong year class will occur in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check sizeStr with StrYC_Nth
# iChecksizeStr <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the multiplier for the strong year class relative to meanR for the strong year class every Nth year method in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the multiplier for the strong year class relative to meanR for the strong year class every Nth year method in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check meanR with randInt
# iCheckmeanRrandInt <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the mean number of recruits for the strong year class at random intervals in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the mean number of recruits for the strong year class at random intervals in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check avgFreq with randInt
# iCheckavgFreq <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the average frequency of a strong year class for the random intervals in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the average frequency of a strong year class for the random intervals in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
#
# # Check sizeStr with randInt
# iChecksizeStrRrandInt <- function(x) {
#   nm <- paste0("'",deparse(substitute(x)),"'")
#   if (missing(x)) STOP("Need to specify the multiplier for the strong year class relative to meanR for the random interval in ",nm,".")
#   if (is.null(x)) STOP("Need to specify the multiplier for the strong year class relative to meanR for the random interval in ",nm,".")
#   iErrMore1(x,nm)
#   iErrNotNumeric(x,nm)
# }
