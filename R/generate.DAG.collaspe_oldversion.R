#' Find the DAG of a graph
#' 
#' Take the adjacency matrix of a graph, identify nodes that form cycles and construct DAG from this system.
#' 
#' @param adjacency The adjacency matrix for the graph
#' @return List of three igraph objects (\code{orginal_graph},\code{SCC_graph} and \code{DAG}). First is the
#' orginal graph, then the nodes that form loops (strongly connected componet) and the DAG for the graph.
#' 
#' @author David O'Sullivan

generate.DAG.collaspe_oldversion = function(adjacency){
  # function will return the resulting DAG graph by collasping problem SCC nodes into one (these nodes will)
  # also be returned
  
  g = graph.adjacency(adjacency) # generte the graph object from the adj matrix
  offending_nodes = V(g)[ find.loop.nodes(adjacency)]$name # find the nodes part of SCC
  
  V(g)$color = "lightgreen" # color all nodes green
  V(g)[ V(g)$name %in% V(g)[ find.loop.nodes(adjacency) ]$name ]$color = "pink" # any nodes that is part of a SCC
  # will be pink
  
  # find and plot the SCC graph
  SCC_graph = induced.subgraph(graph = g,vids = V(g)[ find.loop.nodes(adjacency)]$name)
  
  # create a vector that will mapall the vertex's that are part of the SCC to the first node in the SCC
  vertex_seq = 1:length(V(g))
  vertex_seq[  find.loop.nodes(adjacency) ] = find.loop.nodes(adjacency)[1]
  
  # genereate the graph object from the above mapping
  g_DAG = contract.vertices( graph = g, mapping = vertex_seq, vertex.attr.comb = list(name = "first", color = "first"))
  g_DAG = simplify(graph = g_DAG) # remove multiple edges from nodes
  g_DAG = induced.subgraph( graph = g_DAG,vids =  V(g_DAG)[ degree(g_DAG)> 0] ) # more housekeeping: remove nodes
  # no degree
  
  if(is.DAG(g_DAG, IGRAPH = TRUE) == TRUE){
    print("We now have a DAG to work with!")
    results = list()
    results$orginal_graph = g
    results$SCC_graph = SCC_graph
    results$DAG = g_DAG
    return(results)
  }else{print("woops! Somethings after going wrong, function did not results in a DAG!")}
}
