#' Check if \code{x} is an sfc object.
#' 
#' @param x an object to be tested.
#' @return TRUE if x is an object of the class sfc and FALSE otherwise.
#' 
#' @author Antoine Godin
#' @export
#' 
#' @examples
#' data( models )
#' is.sfc( sim )

is.sfc <- function(x) inherits(x,"sfc")
