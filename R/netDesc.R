netDesc <- function(graph) {

  g <- graph

  # igraph
  nv <- gorder(graph = g)   # number of vertices
  ne <- gsize(graph = g)    # number of edges
  iso <- sum(igraph::degree(g) == 0) # isolated nodes
  g.nComponents <- clusters(g)
  nComp <- g.nComponents$no # number of components
  absgraph <- g
  E(absgraph)$weight <- abs(E(absgraph)$weight)
  av.path.lenght <- average.path.length(graph = absgraph, unconnected = TRUE)
  density <- edge_density(graph = g, loops = FALSE)
  connectivity <- sum(abs(E(g)$weight))
  negEdges <- length(E(g)[which(E(g)$weight < 0)])

  # pulsar
  estrada <- pulsar::estrada.class(as_adjacency_matrix(g, attr = "weight", type = "both"))

  # motifs (beta)
  motif <- motifs(g)

  # average degree
  avdeg <- gsize(graph = g) / gorder(graph = g)

  # transitivity
  globalTransit <- igraph::transitivity(g, type = "global", isolates = "zero")
  localTransit <- igraph::transitivity(g, type = "local", isolates = "zero")

  # Data frames
  netDesc1 <- hellno::data.frame(
    "Nodes" = nv,
    "Edges" = ne,
    "Negative Edges" = negEdges,
    "Isolated Nodes" = iso,
    "Components" = nComp,
    "Ave.Path" = av.path.lenght,
    "Density" = density,
    "Global Transitivity" = globalTransit,
    "Connectivity" = connectivity,
    "Average Degree" = avdeg

  )

  netDesc2 <- hellno::data.frame(
    "Estrada Class" = estrada
)

  netDesc3 <- hellno::data.frame(
    "Local Transitivity" = localTransit
  )

  netDesc5 <- hellno::data.frame(motif)

  # Compile results into a list
  netDesc <- list(
    "Global Properties" = c(netDesc1),
    "Network Class" = c(netDesc2),
    "Local Properties" = c(netDesc3),
    "Motif" = c(netDesc5)
  )

  return(netDesc)
}
