#' Find the trace of a matrix
#' 
#' @param mat A square matrix
#' @return The trace of the matrix
#' @export
#' 
#' @author David O'Sullivan

tr = function(mat){ 
  # just a quick and dirty trace function
  sum( diag(mat) )
}
