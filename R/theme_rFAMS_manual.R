#' @title A theme for ggplot graphics for use in the documentation and articles.
#'
#' @description A simple theme to create "cleaner" ggplot2 graphics used in the `rFAMS` documentation and articles. The user may want to create their own theme for their specific purposes.
#'
#' @param ... Arguments to pass onto `theme_bw()`.
#'
#' @details Details
#'
#' @return  A function
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' # See examples in documentation for most other functions.
#'
#' @export

theme_rFAMS_manual <- function(...) {
 ggplot2::theme_bw(...) +
    ggplot2::theme(
     panel.grid.major=ggplot2::element_blank(),
     panel.grid.minor=ggplot2::element_blank(),
     axis.text=ggplot2::element_text(size=14,color="black"),
     axis.title=ggplot2::element_text(size=16,color="black"),
     axis.title.y=ggplot2::element_text(angle=90),
     axis.line=ggplot2::element_line(color="black"),
     panel.border=ggplot2::element_blank()
   )
}
