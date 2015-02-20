#' Plot igraph objects hierarchy structure
#' 
#' @param graph an igraph object.
#' @param ... plot options for graph objects
#'
#' @export
#' @author David O'Sullivan
#' 
#' @examples
#' data( models )
#' g = generate.DAG.collaspe( adjacency = sim$matrix )$orginal_graph
#' plot_graph_hierarchy( graph = g )
#' 

plot_graph_hierarchy = function(graph,...){
  if( require(Rgraphviz) ){
    graph_nel = igraph.to.graphNEL(graph)
    graph_attributes = makeNodeAttrs(g = graph_nel,label = unlist(V(graph)$name),fillcolor = V(graph)$color)
    plot(graph_nel, nodeAttrs = graph_attributes,...)  
  }else{
    stop( "Rgraphviz could not be loaded. Please install it to use this function." )
  }
  
}
