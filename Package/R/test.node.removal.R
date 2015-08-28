#' Test to see if removing certain nodes will result in a DAG
#' 
#' @inheritParams min_subset_to_remove_nodes
#' @param nodes_to_remove A vector of nodes to remove
#' @return Returns \code{TRUE} if removing nodes results in a DAG, othereise \code{FALSE} is
#' returned.
#' 
#' @author David O'Sullivan
#' @export
#' 
#' @examples
#' 
#' data( models )
#' test.node.removal( sim$matrix, 1 ) # test to see if removing the first node will create a DAG

test.node.removal = function(adjacency,nodes_to_remove,root_node = 1,PLOT = FALSE){
  ###
  # generate the graph
  g = graph.adjacency(adjacency)
  # generate the graph with this vector of nodes removed
  g2 = induced.subgraph(graph = g,vids = V(g)[ V(g)$name != nodes_to_remove ])
  
  if(PLOT == TRUE){ # plot the graphs side by side
    l = layout.reingold.tilford(graph = g,root = root_node)
    V(g)$color = "lightgreen"
    offending_nodes = V(g)[ find.loop.nodes(adjacency) ]$name  
    V(g)[ V(g)$name %in% offending_nodes ]$color = "pink"
    
    V(g2)$color = "lightgreen"
    l2 = layout.reingold.tilford(g2,root = root_node)
    offending_nodes_2 = V(g2)[ find.loop.nodes(g2,IGRAPH = TRUE) ]$name  
    V(g2)[V(g2)$name %in% offending_nodes_2]$color = "pink"
    
    
    par(mfrow = c(1,2))
    plot(g,vertex.color = V(g)$color,vertex.size = 17, layout = l)
    plot(g2,vertex.color = V(g2)$color,vertex.size = 17,layout = l2)
    par(mfrow = c(1,1))
    
  }
  
  # if removing the nodes does result in a graph then wayhay! 
  # return TRUE
  return( is.DAG(g2, IGRAPH = TRUE) )
}
