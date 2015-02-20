#' Find nodes that form loops.
#' 
#' @param adjacency A square adjacency matrix.
#' @param IGRAPH if \code{IGRAPH} is true adjacency is already a Igraph object not a matrix.
#' @return The row/col numbers for nodes that form loops in the graph.
#'
#' @export
#' @author David O'Sullivan
#' 
#' @examples
#' data( models )
#' find.loop.nodes( sim$matrix )
#' 
find.loop.nodes = function(adjacency, IGRAPH = FALSE){
  # function will return a vector of col/row number assocaited with nodes
  # that are part of a loop
  if( IGRAPH == TRUE ){
    adjacency = get.adjacency(adjacency,sparse = FALSE)
  }
  ans = numeric() # ans will contain the nodes of interest
  l = dim(adjacency)[1] # find the number of rows/col of matrix
  
  # find the nodes with cycles
  for(i in 1:l){
    path_lengths = diag(adjacency %^% i)
    ans =c(ans,which(path_lengths > 0)) # return the positions of paths lenghts that are 
    # greater than zero ( offending nodes )
    
  }
  return(sort(unique(ans)))
}
