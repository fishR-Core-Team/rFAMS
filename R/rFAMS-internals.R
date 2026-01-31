#' @title Internal functions.
#'
#' @description Internal functions that are common to several functions in rFAMS.
#'
#' @rdname rFAMS-internals
#' @keywords internal
#' @aliases STOP WARN .onAttach is.wholenumber iIbeta iErrMore1 iErrNotNumeric iErrLT iErrGt iCheckMLH iCheckMLHinc iCheckrecruitTL iChecklowerSLTL iCheckupperSLTL iCheckslotOrder iCheckcf iCheckcm iCheckcfminc iCheckloi iCheckcfcm_dpm iCheckN0 iCheckLLinf iCheckLinf iCheckK iCheckt0 iCheckLWb iCheckLWa iCheckMaxAge isum_by_year iChecksimyears iCheckspecies iCheckNrec iCheckMinR iCheckMaxR iCheckMinRNorm iCheckMaxRNorm iCheckmeanR iChecksdR iCheckmeanRNth iCheckNthyr iChecksizeStr iCheckmeanRrandInt iCheckavgFreq iChecksizeStrRrandInt isum_by_year iCheckrec iCheckloi iCheckcfcm_dpm iCheckcfabove iCheckcfin iCheckcfunder

# -- Sends a start-up message to the console when the package is loaded.
.onAttach <- function(libname, pkgname) {
  vers <- read.dcf(system.file("DESCRIPTION",
                               package=pkgname,lib.loc=libname),
                   fields="Version")
  msg <- paste0("## rFAMS v",vers,". See citation('rFAMS') if used in publication.\n")
  msg <- paste0(msg,"## Visit https://github.com/fishR-Core-Team/rFAMS/issues to report any bugs.")
  packageStartupMessage(msg)
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

# Check recruitment total length
iCheckrecruitTL <- function(x,type="") {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a recruitment total length (mm) in ",nm,".")
  if (is.null(x)) STOP("Need to specify a recruitment total length (mm) in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<50) WARN("A recruitment total length of ",x," mm seems too small,\n",
                  "please check value in ",nm,".")
  if (x>1600) WARN("A recruitment total length of ",x," mm seems too large,\n",
                   "  please check value in ",nm,".")
}

# Check lower slot limit total length
iChecklowerSLTL <- function(x,type="") {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify a lower slot limit total length (mm) in ",nm,".")
  if (is.null(x)) STOP("Need to specify a lower slot limit total length (mm) in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<50) WARN("A lower slot limit total length of ",x," mm seems too small,\n",
                 "please check value in ",nm,".")
  if (x>1600) WARN("A lower slot limit total length of ",x," mm seems too large,\n",
                   "  please check value in ",nm,".")
}

# Check lower slot limit total length
iCheckupperSLTL <- function(x,type="") {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify an upper slot limit total length (mm) in ",nm,".")
  if (is.null(x)) STOP("Need to specify an upper slot limit total length (mm) in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (x<50) WARN("An upper slot limit total length of ",x," mm seems too small,\n",
                 "please check value in ",nm,".")
  if (x>1600) WARN("An upper slot limit total length of ",x," mm seems too large,\n",
                   "  please check value in ",nm,".")
}

#Check recruitment, lower slot, and upper slot are in proper order
iCheckslotOrder <- function(recruitmentTL, lowerSL, upperSL) {
  ## Check min vs max
  nm1 <- paste0("'",deparse(substitute(recruitmentTL)),"'")
  nm2 <- paste0("'",deparse(substitute(lowerSL)),"'")
  nm3 <- paste0("'",deparse(substitute(upperSL)),"'")
  if(recruitmentTL>lowerSL) STOP(nm1," must be less than ",nm2,".")
  if(recruitmentTL>upperSL) STOP(nm1," must be less than ",nm3,".")
  if(lowerSL>upperSL) STOP(nm2," must be less than ",nm3,".")
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
  res <- round(seq(xmin,xmax,xinc),8)
  if (length(res)>100)
    WARN("Choices of ",nm1,", ",nm2,", and ",nm," resulted in ",length(res),
         " values.\n","  Depending on other choices the simulation may be slow.")
  ## Return sequence
  res
}

# Check conditional fishing mortality value under slot
iCheckcfunder <- function(x,type=NULL) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
  if (missing(x)) STOP("Need to specify a",type,
                       " conditional fishing mortality under the slot limit in ",nm,".")
  if (is.null(x)) STOP("Need to specify a",type,
                       " conditional fishing mortality under the slot limit in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
}

# Check conditional fishing mortality value in slot
iCheckcfin <- function(x,type=NULL) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
  if (missing(x)) STOP("Need to specify a",type,
                       " conditional fishing mortality in the slot limit in ",nm,".")
  if (is.null(x)) STOP("Need to specify a",type,
                       " conditional fishing mortality in the slot limit in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
}

# Check conditional fishing mortality value under slot
iCheckcfabove <- function(x,type=NULL) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if(!is.null(type)) type <- paste0(" ",type)  ## to handle space padding in msg
  if (missing(x)) STOP("Need to specify a",type,
                       " conditional fishing mortality above the slot limit in ",nm,".")
  if (is.null(x)) STOP("Need to specify a",type,
                       " conditional fishing mortality above the slot limit in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  iErrGT(x,1,nm)
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


# Check length of interest "loi" input
iCheckloi <- function(loi){
  #if(any(is.na(loi))){return(NULL)}
  if(is.null(loi)){return(NULL)}
  if(!is.vector(loi))
    STOP("loi must be a vector")
  if(!is.numeric(loi))
    STOP("loi must be a numeric data type")
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
    # if (!is.wholenumber(x))
    #   WARN("The initial number in the population is not a whole number,\n",
    #        "  please check value in ",nm,".")
  }
}

# Check Linf > Minimum length
iCheckLLinf <- function(x, Linf) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (x > Linf) STOP("Harvest length (", nm, ") can't be greater than Linf")
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

# Check simyears
iChecksimyears <- function(x) {
  nm <- paste0("'",deparse(substitute(x)),"'")
  if (missing(x)) STOP("Need to specify the number of simulations to run in ",nm,".")
  if (is.null(x)) STOP("Need to specify the number of simulations to run in ",nm,".")
  iErrMore1(x,nm)
  iErrNotNumeric(x,nm)
  iErrLT(x,0,nm)
  if (!is.wholenumber(x)) WARN("The numer of simulation years is not a whole number,\n",
                               "  please check value in ",nm,".")
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
  year<-gcat<-N_start<-count<-quality<-stock<-preferred<-memorable<-trophy<-age<-yield<-biomass<-N_harvest<-N_die<-age_1plus<-Yield_age_1plus<-Total_biomass<-N_harvest_age_1plus<-N_die_age_1plus<-NULL
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
    dplyr::summarise(count = floor(sum(N_start))) |>
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
    dplyr::summarize(age_1plus = sum(N_start), Yield_age_1plus = sum(yield),
                     Total_biomass = sum(biomass), N_harvest_age_1plus = sum(N_harvest),
                     N_die_age_1plus = sum(N_die)) |>
    dplyr::right_join(psd_summary, by = "year") |>
    dplyr::mutate(dplyr::across(c(age_1plus, Yield_age_1plus, Total_biomass, N_harvest_age_1plus, N_die_age_1plus), ~dplyr::coalesce(., 0)))

  # merged_df <- dplyr::left_join(psd_summary,Year_Summary, by = "year") |>
  #   dplyr::mutate(dplyr::across(c(age_1plus, Yield_age_1plus, Total_biomass, N_harvest_age_1plus, N_die_age_1plus), ~dplyr::coalesce(., 0)))


  return(Year_Summary)
}
