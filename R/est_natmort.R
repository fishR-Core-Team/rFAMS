#' @title Compute meta-analytic estimates of instantaneous and conditional natural mortality
#'
#' @description Several methods may be used to estimate instantaneous (M) and conditional natural mortality (cm) from other types of data, especially those saved in the life history parameters vector/list from \code{\link{makeLH}}.
#'
#' @details The default \code{method}s to use are all of those listed in \code{\link[FSA]{Mmethods}} that use some of the life history parameters required by \code{makeLH}. These methods are not all equally useful or robust, so the user may want to select a subset of them for use after learning more about them. See references in \code{\link[FSA]{metaM}}.
#'
#' Other methods that require parameters other than those required by \code{makeLH} can be used by providing the name of the method in \code{method} and the required parameters as arguments, as defined in \code{\link[FSA]{metaM}}. See \code{\link[FSA]{metaM}} for more details and the examples below for an example.
#'
#' @param method A string that indicates what methods to use to estimate M (see \code{\link[FSA]{metaM}}).
#' @param lhparms A named vector or string returned by \code{lhparms}.
#' @param incl.avg A logical that indicates whether the average cm should be computed from the estimated M of all methods.
#' @param \dots Option arguments for parameter values required by methods using parameters other than those in \code{lhparms}. See examples.
#'
#' @return A data.frame with the following items:
#'
#' * `method`: The name for the method within the function (as given in `method`).
#' * `M`: The estimated instantaneous natural mortality rate (from `metaM`)
#' * `cm`: The estimated conditional natural mortality rate (computed directly from `M`).
#' * `givens`: A string that contains the input values required by the method to estimate M.
#'
#' @examples
#' # An example lhparm as would be returned from makeLH
#' tmp <- list(N0=100,maxage=15,Linf=500,K=0.3,t0=-0.5,LWalpha=-5.16,LWbeta=3.1)
#'
#' # All methods in metaM() that use those life history parameters
#' est_natmort(tmp)
#'
#' # Same but including the average in the last row
#' est_natmort(tmp,incl.avg=TRUE)
#'
#' # Selecting just one method
#' est_natmort(tmp,method="HoenigNLS")
#'
#' # Selecting several methods
#' est_natmort(tmp,method=c("HoenigNLS","HoenigO","HoenigO2","HoenigLM"))
#'
#' # A method that uses a parameter not usually in lhparms
#' est_natmort(tmp,method="QuinnDeriso",PS=0.05)
#'
#' # Selecting all Hoenig methods using Mmethods from FSA
#' est_natmort(tmp,method=FSA::Mmethods("Hoenig"))
#'
#' # Over-riding the Linf param in parameters list, but others from tmp
#' est_natmort(tmp,method="PaulyLNoT")              # Linf from tmp
#' est_natmort(tmp,Linf=1000/10,method="PaulyLNoT") # Linf from Linf= arg
#'
#' @rdname est_natmort
#' @export

est_natmort <- function(lhparms=NULL,method="rFAMS",incl.avg=FALSE,...) {
  # Get args and names of arguments sent in dots
  margs <- list(...)
  nmargs <- names(margs)

  # Life history parameters must be in lhparms or dots
    if (is.null(lhparms)) {
    if (length(nmargs)==0)
      STOP("Life history parameters must be in 'lhparms' or individual arguments.")
  } else {
    # Extract parms from lhparms and add to margs list for metaM() later
    #   Make sure not to override parms sent in dots
    if (!"Linf" %in% nmargs) margs$Linf <- lhparms[["Linf"]]/10  ## mm in rFAMS, cm in metaM
    if (!"K" %in% nmargs) margs$K <- lhparms[["K"]]
    if (!"t0" %in% nmargs) margs$t0 <- lhparms[["t0"]]
    if (!"b" %in% nmargs) margs$b <- lhparms[["LWbeta"]]
    if (!"tmax" %in% nmargs) margs$tmax <- lhparms[["maxage"]]
  }

  # if method is not null then get methods that can use rFAMS lhparms
  if (length(method)==1) {
    if (method=="rFAMS")
      method <- c("HoenigNLS","HoenigO","HoenigOF","HoenigOM","HoenigOC",
                  "HoenigO2","HoenigO2F","HoenigO2M","HoenigO2C",
                  "HoenigLM","HewittHoenig","tmax1","PaulyLNoT","K1","K2",
                  "JensenK1","JensenK2","AlversonCarney","ChenWatanabe")
  }
  margs$method <- method

  # Get results from FSA's metaM
  res <- do.call(FSA::metaM,margs)

  # Include the average across methods if asked for
  if (incl.avg)
    res <- rbind(res,
                 data.frame(M=mean(res$M),cm=mean(res$cm),method="AVERAGE",
                            name="AVERAGE OF ALL ESTIMATES",givens=""))

  # Rearrange and return results
  res[,c("method","M","cm","givens")]
}
