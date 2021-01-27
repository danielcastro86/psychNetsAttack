#netDescriptives

netDesc <- function(graph){

  g <- graph

  #igraph
  nv <- gorder(graph = g)   #number of vertices
  ne <- gsize(graph =g)    #number of edges
  iso <- sum(degree(g)==0) #isolated
  g.nComponents <- clusters(g)
  nComp <- g.nComponents$no #number of components
  av.path.lenght <- average.path.length(graph = g, unconnected = TRUE)
  density <- edge_density(graph = g, loops = FALSE)
  negEdges <- length(E(g)[which(E(g)$weight<0)])

  #pulsar
  estrada <- pulsar::estrada.class(as_adjacency_matrix(g, attr = "weight", type = "both"))

  #QuACN
  compactness <- QuACN::compactness(as_graphnel(g))
  globalClusterCoef <- QuACN::globalClusteringCoeff(as_graphnel(g))
  localClusterCoef <- QuACN::localClusteringCoeff(as_graphnel(g))
  gDistanceCompl <- QuACN::graphDistanceComplexity(as_graphnel(g))
  gIndexCompl <- QuACN::graphIndexComplexity(as_graphnel(g))
  gVertexCompl <- QuACN::graphVertexComplexity(as_graphnel(g))
  gindexBcompl <- QuACN::complexityIndexB(as_graphnel(g))

  #motifs (beta)
  motif <- motifs(g)

  #average degree
  avdeg <- gsize(graph =g) / gorder(graph = g)

  #transitivity
  globalTransit <- transitivity(g, type = "global", isolates = "zero")

  localTransit <- transitivity(g, type = "local", isolates = "zero")




  netDesc <- list("Global Properties" = c(hellno::data.frame("Nodes"= nv, "Edges" = ne,
                                                              "Negative Edges" = negEdges,
                                                              "Isolated Nodes" =  iso,
                                                              "Components" = nComp,
                                                              "Ave.Path" =  av.path.lenght,
                                                              "Density" = density,
                                                              "Compactness" = compactness,
                                                              "Global Clustering Coefficient"= globalClusterCoef,
                                                              "Global Transitivity" = globalTransit)),
                  "Network Class" = c("Estrada Class" = estrada,
                                      "Average Degree" = avdeg),
                  "Local Properties" = c("Local Clustering Coefficient" = localClusterCoef,
                                         "Local Transitivity" = localTransit),
                  "Complexity Measures" = c(hellno::data.frame("Distance" = gDistanceCompl,
                                                               "Vertex" = gVertexCompl,
                                                               "Index" = gIndexCompl,
                                                               "Index B" = gindexBcompl)),
                  "Motif" = c(motif))


  return(netDesc)


}


