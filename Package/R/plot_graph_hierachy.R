#' Plot igraph objects hierarchy structure
#' 
#' @param model an sfc model.
#' @param ... plot options for graph objects
#'
#' @export
#' @author David O'Sullivan
#' 
#' @examples
#' data( models )
#' plot_graph_hierarchy( model=sim)
#' 

plot_graph_hierarchy = function(model,...){
  graph=generate.DAG.collaspe(adjacency=t(model$matrix))$orginal_graph
  if( require(Rgraphviz) ){
    graph_nel = igraph.to.graphNEL(graph)
    graph_attributes = makeNodeAttrs(g = graph_nel,label = unlist(V(graph)$name),fillcolor = V(graph)$color)
    plot(graph_nel, nodeAttrs = graph_attributes,...)  
  }else{
    stop( "Rgraphviz could not be loaded. Please install it to use this function." )
  }
  
}
