#netDescriptives

netDesc <- function(graph){

  g <- graph

  nv <- gorder(graph = g)   #number of vertices
  ne <- gsize(graph =g)    #number of edges
  iso <- sum(degree(g)==0) #isolated
  g.nComponents <- clusters(g)
  nComp <- g.nComponents$no #number of components
  av.path.lenght <- average.path.length(graph = g, unconnected = TRUE)
  density <- edge_density(graph = g, loops = FALSE)

  netDesc <- hellno::data.frame("Nodes"= nv, "Edges" = ne,"Isolated Nodes" =  iso, "Components" = nComp,"Ave.Path" =  av.path.lenght,
                                "Density" = density)

  return(netDesc)


}


