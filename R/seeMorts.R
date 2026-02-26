#' @title Convert vectors of conditional fishing and natural mortality rates to other mortality rates.
#'
#' @description Convert vectors of conditional fishing (cf) and natural (cm) mortality rates to instantaneous total (Z), fishing (F), and natural (M) mortality rates, total annual mortality rate (A), the annual exploitation rate (u), and the expectation of natural death (v). The primary purpose of this function is to provide a data.frame from which the user can explore the relationships between these rates and understand how choices of cf and cm effect the other rates, especially A and u.
#'
#' @param cf A numeric vector (could be of length 1) representing conditional fishing mortality. See details.
#' @param cm A numeric vector (could be of length 1) representing conditional natural mortality. See details.
#' @param type A single numeric that identifies whether the annual exploitation rate (u) and the expectation of natural death (v) should be computed for a type-\code{2} (DEFAULT) or type-\code{1} fishery (as defined by Ricker (1975); see details).
#' @param verbose A logical that indicates whether a brief note should be printed to the console. Defaults to \code{TRUE}.
#' @param object An object returned by \code{seeMorts}.
#' @param \dots Arguments to be forwarded to \code{summary}.
#'
#' @details Numeric values in the \code{cf} and \code{cm} vectors can be entered as a single value (e.g., \code{cf=0.3}), a sequence of values created with \code{\link{seq}} (e.g., \code{cf=seq(0.1,0.5,0.05)}, or as unique values with \code{\link{c}} (e.g., \code{cf=c(0.1,0.4,0.5)} depending on the user's needs. Values of \code{cf} and \code{cm} will be repeated as necessary (via \code{\link{expand.grid}}) to form all combinations of the two sets of given values. Thus, neither \code{cf} and \code{cm} should contain repeated values.
#'
#' Equations for computing the other mortality rates (F, M, Z, A, u, and v) from cf and cm are in Ricker (1975). Note that n and m in Ricker (1975) are cf and cm here.
#'
#' The formulae for u and v differ depending on whether a Type-1 or a Type-2 fishery is being considered (see \code{type}). A Type-1 fishery is where fishing mortality occurs in a very narrow part of the annual period such that it is reasonable to assume that fishing and natural mortality do not both occur (or overlap) in that portion (e.g., a fishery where the open harvest season is only a few days). A Type-2 fishery is where natural and fishing mortality substantially overlap throughout the annual period (e.g., a fishery where the open harvest season is much of the annual period).
#'
#' @return The main function returns a data.frame with the following values:
#' \itemize{
#' \item \code{cm} is the given conditional natural mortality rates.
#' \item \code{cf} is the given conditional fishing mortality rates.
#' \item \code{M} is the calculated instantaneous rate of natural mortality.
#' \item \code{F} is the calculated instantaneous rate of fishing mortality.
#' \item \code{Z} is the calculated instantaneous rate of total mortality.
#' \item \code{A} is the calculated total annual rate of mortality.
#' \item \code{u} is the calculated annual exploitation rate.
#' \item \code{v} is the calculated expectation of natural death.
#' }
#'
#' The \code{summary} function returns a data.frame with the following values for each of the mortality rates:
#' \itemize{
#' \item \code{type} is the "type" of mortality rate (cm, cf, M, F, Z, A, u, or v).
#' \item \code{unique} is the number of unique values.
#' \item \code{min} is the minimum value (rounded to 3 decimal places).
#' \item \code{max} is the maximum value (rounded to 3 decimal places).
#' }
#'
#' @seealso \code{\link{yprBH_MinLL}}, \code{\link{yprBH_SlotLL}}, and \code{\link{dpmBH_MinLL}} for functions that require the user to provide reasonable values of cf and cm.
#'
#' @references
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada. Was (is?) from \url{https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/1485.pdf}.
#'
#' @examples
#' # == Simple examples ========================================================
#' seeMorts(cf=0.3,cm=0.2)
#' seeMorts(cf=0.3,cm=0.2,type=1)
#'
#' # == More realistic example =================================================
#' test <- seeMorts(cf=seq(0,0.5,0.05),cm=c(0.2,0.3,0.4,0.5))
#' head(test)
#' tail(test)
#' summary(test)
#'
#' #-- Optional plotting examples ----------------------------------------------
#' if (require(ggplot2)) {
#'   ggplot(data=test,mapping=aes(x=cf,y=u,color=as.factor(cm))) +
#'     geom_line(linewidth=1) +
#'     theme_bw()
#'
#'   ggplot(data=test,mapping=aes(x=Z,y=A)) +
#'     geom_line(linewidth=1) +
#'     theme_bw()
#'
#'   ggplot(data=test,mapping=aes(x=cf,y=cm,z=A)) +
#'     geom_contour_filled(bins=9) +
#'     scale_fill_discrete(name="A",palette="OrRd") +
#'     theme_bw()
#' }
#'
#' @aliases seeMorts summary.SEEMORTS
#'
#' @rdname seeMorts
#' @export
seeMorts <- function(cf,cm,type=2,verbose=TRUE) {
  ## Checks
  if (!type %in% 1:2)
    STOP("'type' must be 1 or 2 to choose a 'Type-1' or 'Type-2' fishery.")
  iCheckCondMort(cm)
  iCheckCondMort(cf)
  if (any(duplicated(cm))) {
    cm <- cm[!duplicated(cm)]
    WARN("Duplicated values in 'cm' were dropped.")
  }
  if (any(duplicated(cf))) {
    cf <- cf[!duplicated(cf)]
    WARN("Duplicated values in 'cf' were dropped.")
  }
  ## Send message about type of fishery if verbose=TRUE (default)
  if (verbose) cat("Conditional mortality calculations made for a Type-",
                   type," fishery.\n", sep="")

  ## Create data.frame of all combinations of cm and cf
  tmp <- as.data.frame(expand.grid(cm=cm,cf=cf,KEEP.OUT.ATTRS=FALSE))

  ## Compute M, F, Z, A, u, and v for all combos of cm and cf
  tmp$M <- -log(1-tmp$cm)
  tmp$F <- -log(1-tmp$cf)
  tmp$Z <- tmp$M+tmp$F
  tmp$A <- 1-exp(-tmp$Z)
  if (type==2) tmp$u <- tmp$A*tmp$F/tmp$Z
    else tmp$u <- tmp$cf
  if(type==2) tmp$v <- tmp$A*tmp$M/tmp$Z
    else tmp$v <- tmp$cm*(1-tmp$u)

  ## Arrange final data.frame by values of cm and then cf
  tmp <- tmp[order(tmp$cm,tmp$cf),]

  ## Add SEEMORTS class to tmp ... to allow for summary.SEEMORTS
  class(tmp) <- c("SEEMORTS","data.frame")

  ## Return data.frame
  tmp
}

#' @rdname seeMorts
#' @export
summary.SEEMORTS <- function(object,verbose=TRUE,...) {
  ### Internal function to create summaries
  smry <- function(x) {
    c(unique=length(unique(x)),
      min=round(min(x),3),
      max=round(max(x),3))
  }

  ## Pivot longer for ease of summarizing
  tmp <- utils::stack(object)
  ## Summarize, rearrange, and rename for aesthetics
  tmp2 <- stats::aggregate(tmp$values,list(tmp$ind),smry)
  tmp2 <- cbind(tmp2[,1],as.data.frame(tmp2[,2]))
  names(tmp2) <- c("type","unique","min","max")
  ## Change type from factor to character
  tmp2$type <- as.character(tmp2$type)
  ## Return/Print summary data.frame
  if (verbose) cat("Summary of Mortality Rates\n")
  tmp2
}
