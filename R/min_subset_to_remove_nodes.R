#' Find the minimumber of nodes to remove from graph to create a DAG.
#' 
#' @inheritParams find.loop.nodes
#' @param PLOT If plot is \code{TRUE}, create a plot of the graph with nodes that are to be removed
#' highlighted in light pink and nodes outside of cycles higlighted in green.
#' @param root_node The node that forms the root of the plot.
#' 
#' @export
#' @author David O'Sullivan
#' 
#' @examples
#' data( models )
#' min_subset_to_remove_nodes( sim$matrix )

min_subset_to_remove_nodes = function( adjacency, root_node = 1, PLOT = FALSE ){
  # REMINDER: REALLY NEED TO RENAME THIS FUNCTION!
  
  # function will return the minimal number of nodes that can be removed that will
  # make the graph a DAG
  
  # generate a graph object
  g = graph.adjacency(adjacency)
  offending_nodes = V(g)[ find.loop.nodes(adjacency) ]$name
  
  # generate a list of all possible combination of problem nodes that could be removed
  test_list = list()
  k = min(length(offending_nodes),5)
  for(i in 1:k){
    test_list[[i]] = combn(x = offending_nodes,m =i)
    # print(i)
  }
  
  res = numeric(0)
  # now test each of these and save the index's
  for( i in 1:length(test_list) ){
    for( j in 1:dim( test_list[[i]] )[2] ){
      # save the index of valid DAG configs
      if(test.node.removal(adjacency = adjacency,nodes_to_remove = test_list[[i]][,j]) == TRUE){
        res = rbind(res,c(i,j))
      }
    }
  }
  
  # the minimal group of nodes that you could remove would be the min of res
  res = subset(x = res, res[,1] == min(res,1))
  chock_points = list()
  for(i in 1:dim(res)[1]){
    chock_points[[i]] = test_list[[res[i,1]]][,res[i,2]]
  }
  
  if( PLOT == TRUE){# plot the graph
    l = layout.reingold.tilford(graph = g,root = root_node)
    V(g)$color = "lightgreen"
    V(g)[ V(g)$name %in% unique(unlist(chock_points)) ]$color = "pink"
    plot(g,vertex.color = V(g)$color,vertex.size = 17, layout = l)
  }
  # return the nodes that you could remove in list form
  return(chock_points)
}
