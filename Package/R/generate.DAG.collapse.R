#' Find the DAG of a graph.
#' 
#' Take the adjacency matrix of a graph, identify nodes that form cycles and construct DAG from this system.
#' 
#' @param adjacency A square adjacency matrix.
#' @return List of three igraph objects (\code{orginal_graph},\code{SCC_graph} and \code{DAG}). First is the
#' orginal graph, then the nodes that form loops (strongly connected componet) and the DAG for the graph.
#' 
#' @export
#' @author David O'Sullivan
#' 
#' @examples
#' data( models )
#' generate.DAG.collaspe( sim$matrix )
#' 
generate.DAG.collapse = function(adjacency){
  # function will return the resulting DAG graph by collasping problem SCC nodes into one (these nodes will)
  # also be returned
  
  g = graph.adjacency(adjacency) # generte the graph object from the adj matrix
  offending_nodes = V(g)[ find.loop.nodes(adjacency)]$name # find the nodes part of SCC
  
  V(g)$color = "lightgreen" # color all nodes green
  V(g)[ V(g)$name %in% V(g)[ find.loop.nodes(adjacency) ]$name ]$color = "pink" # any nodes that is part of a SCC
  # will be pink
  
  # find and plot the SCC graph
  SCC_graph = induced.subgraph(graph = g,vids = V(g)[ find.loop.nodes(adjacency) ]$name)
  
  # create a vector that will mapall the vertex's that are part of the SCC to the first node in the SCC
  SCC_comp = table( clusters(graph = g,mode = "strong")$membership ) # find out how many nodes are in each strongly connected
  
  vertex_seq = 1:length(V(g)) # this will do the mapping part, for the condensatation graph
  
  nodes_to_be_mapped = as.numeric( names(SCC_comp[ SCC_comp > 1 ]) ) # pull the names of nodes that are greater than one
  for( q in nodes_to_be_mapped ){
    # so now want to run throught these and map their vector to the vector sequence nodes
    vertex_seq[ clusters(graph = g,mode = "strong")$membership == q ] = q
  }
  
  # genereate the graph object from the above mapping
  g_DAG = contract.vertices( graph = g, mapping = vertex_seq, vertex.attr.comb = list(name = "first", color = "first"))
  g_DAG = simplify(graph = g_DAG) # remove multiple edges from nodes
  g_DAG = induced.subgraph( graph = g_DAG,vids =  V(g_DAG)[ igraph:::degree(g_DAG)> 0] ) # more housekeeping: remove nodes
  # no degree
  
  if(is.DAG(g_DAG, IGRAPH = TRUE) == TRUE){
    results = list()
    results$orginal_graph = g
    results$SCC_graph = SCC_graph
    results$DAG = g_DAG
    return(results)
  }else{warning("woops! Somethings after going wrong, function did not result in a DAG!")}
}
