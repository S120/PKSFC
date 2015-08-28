# Representing SFC models as DAGs

In this post we provide an introductory demo for a graphical tool that exists as an extension of the pk-sfc package available [here](https://github.com/S120/PKSFC) developed in [R](http://www.r-project.org/). The graphical tools main function is to extract [Directed Acylic Graphs](http://en.wikipedia.org/wiki/Directed_acyclic_graph) (DAGs) from a [Stock Flow Consistent Model](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2492242). The SFC models will be generated using eviews files containing the SFC models from [Monetary Economics: An Integrated Approach to Credit, Money, Income Production and Wealth by Marc Lavoie and Wynne Godley](http://www.amazon.com/Monetary-Economics-Integrated-Approach-Production/dp/0230301843%3FSubscriptionId%3D0PZ7TM66EXQCXFVTMTR2%26tag%3Dstephkinse-20%26linkCode%3Dxm2%26camp%3D2025%26creative%3D165953%26creativeASIN%3D0230301843). The eviews files are available from [here](hyperlink here) and required files are available for download [here](hyperlink_here).

### Step 1: Set-up

If you do not already have R installed please follow this [link](http://cran.r-project.org/) and install the appropriate version. The code below will then install load some of the prerequisite functions and code.
```{r}
library(PKSFC)
library("Rgraphviz")
```

### Step 2: Load an eviews file

Using the model from chapter 6 ("gl06open.prg") as demo case, using the following code to parse the eviews file and extract the adjacency matrix for the system.

````{r, results='hide'}
model <- sfc.model("ch6.txt",modelName="Chapter6_openmodel")
````

The adjacency matrix for the system can be found by using `$matrix`.

### Step 3: Your very first DAG

The next segment of codes takes the adjacency matrix and returns a list of igraph objects corresponding to the original graph, the nodes that form cycles and the resulting DAG for the system.

````{r, results='hide'}
graphs = generate.DAG.collaspe( adjacency = model$matrix )
````

If instead of using an adjacency matrix, you wanted to use an igraph object, adding the argument `IGRAPH = TRUE` will allow the three graphs to be required with no errors.

````{r, eval=FALSE}
g = graph.adjacency(adjmatrix = model$matrix,mode = "directed")
graphs = generate.DAG.collaspe(g, IGRAPH = TRUE)
````

### Step 4: Generating graphics

Two methods for generating graphical representations of DAGs from the models are presented here, first is the standard `plot` function using the igraph package.

````{r}
par(mfrow = c(1,3))
# first plot the orgainal graph of the system
plot( graphs$orginal_graph,vertex.color = V(graphs$orginal_graph)$color,vertex.size = 20,main = "orginal graph" )
# just the nodes that form strongly connected componets
plot( graphs$SCC_graph, vertex.color = V(graphs$SCC_graph)$color, vertex.size = 20, main = "scc nodes")
# the resulting DAG when we take the condensation of the orginal graph
plot( graphs$DAG, vertex.color = unlist(V(graphs$DAG)$color), vertex.size = 20, vertex.label = NA, main = "resulting DAG")

````

Or using the much nicer but less flexible function based in the Rgraphivz package to generate the plots we can delve into the actual structure of the system. 

````{r}
par(mfrow = c(1,3))
# first plot the orgianl grpah
plot_graph_hierarchy( graphs$orginal_graph, main = "orginal graph" )
# plot hte nodes that form the strongly connected compoent
plot_graph_hierarchy( graphs$SCC_graph, main = "SCC nodes" )
# plot the result ing DAG when we take the condensation of the graph
plot_graph_hierarchy( graphs$DAG, main = "resulting DAG" )
````

Nodes that do not form a cycle are green while nodes that form a cycle in the system are pink. To following segments of code will produce a vector of the green or pink nodes for inspection.

````{r}
non_cycle_nodes = V(graphs$orginal_graph)[ V( graphs$orginal_graph )$color == "lightgreen" ]$name
cycle_nodes = V(graphs$orginal_graph)[ V(graphs$orginal_graph)$color == "pink" ]$name

head( non_cycle_nodes )
````

To beauty of using igraph as the back bone for this tool is that it provides a lot of flexibility in how we can construct such graphical representations of SFC models. Models can be created using adjacency matrices, edges list or other graphical objects from other packages can also handled, for the most part, pain free! 

Some useful reference materials:
<ul>
<li> The main documentation for the igraph package can be found [here](http://cran.r-project.org/web/packages/igraph/igraph.pdf).</li>
<li> A brief introduction to igraph along with some of its basics can be found here [here](http://igraph.org/r/doc/aaa-igraph-package.html). </li>
</ul>
