#' Is graph a DAG.
#' 
#' Checks graph to see if it is a DAG or not.
#' 
#' @inheritParams find.loop.nodes
#' @return \code{TRUE} or \code{FALSE}.
#' 
#' @export
#' @author David O'Sullivan 
#' 
#' @examples
#' 
#' data( models )
#' is.DAG( sim$matrix )
#' g = graph.adjacency( adjmatrix = sim$matrix, mode = "directed" )
#' is.DAG( g, IGRAPH = TRUE )

is.DAG = function(adjacency, IGRAPH = FALSE){
  # function will tell you if it is a DAG or not!
  
  # if an igraph graph object is used then turn it back into a matrix
  if( IGRAPH == TRUE ){
    adjacency = get.adjacency(adjacency,sparse = FALSE)
  }
  l = dim(adjacency)[1]
  path_lengths = numeric(l)
  
  for(i in 1:l){
    path_lengths[i] = tr(adjacency %^% i)
    # print(i)
  }
  
  if(sum(path_lengths) == 0){
    return(TRUE)
  }else{
    return(FALSE)}
}
