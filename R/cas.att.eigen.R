cas.att.eigen <- function(g){

  #numero de repetições
  n <- length(V(g))

  g2 <- g

  #registo dos resultados
  numberofvertices<-integer(n-1)
  clustersizes<-integer(n-1)
  cohesion<-integer(n-1)
  averagepath<-integer(n-1)
  adhesion<-integer(n-1)
  edgedensity <- integer(n-1)
  transitivity <- integer(n-1)
  radius<- integer(n-1)
  density<- integer(n-1)
  centralization<-integer(n-1)
  strength <- integer(n-1)
  vertex <- integer(n-1)
  components <- integer(n-1)

  #remoção dos vértices
 for(i in 1:(n-1)){

    vertex[i] <- V(g2)$names[which.max(unlist(igraph::eigen_centrality(g2)$vector))] #cálculo e seleção do vértice a remover + registo do nome do vértice
    strength[i] <- max(unlist(igraph::eigen_centrality(g2)$vector)) #registo do valor máximo da força


    #cálculo e registo das propriedades das redes
    #(verificar: algumas parecem ter sempre 0 | edgedensity parece ser a mesma coisa que graph.density)

    numberofvertices[i]<-igraph::gorder(g2)
    clustersizes[i]<-max(igraph::clusters(g2)$csize)
    cohesion[i]<-igraph::cohesion(g2)
    averagepath[i]<-igraph::average.path.length(g2)
    adhesion[i]<- igraph::graph.adhesion(g2)
    edgedensity[i] <-igraph::edge_density(g2)
    transitivity[i]<- igraph::transitivity(g2, type= "global")
    radius[i]<-igraph::radius(g2)
    density[i]<- igraph::graph.density(g2)
    centralization[i]<-igraph::centr_degree(g2)$centralization
    components[i] <- igraph::components(g2)$no

    g2 <- igraph::delete.vertices(g2, v=which.max(igraph::eigen_centrality(g2)$vector)) #cálculo e seleção do vértice a remover

    #gráfico após cada remoção (layout não está fixo)
    plot(g2, vertex.size=20, vertex.color="darkolivegreen3",
         vertex.label=V(g2)$names, vertex.label.cex=0.8, edge.width=2)

  }

  #tabela com os resultados do ataque
  df<-hellno::as.data.frame(cbind(c(vertex, NA), c(strength, NA), c(numberofvertices, NA), c(clustersizes, NA), c(cohesion, NA), c(averagepath, NA), c(adhesion, NA), c(edgedensity, NA), c(transitivity, NA), c(radius, NA), c(density, NA), c(centralization, NA), c(components, NA)), stringAsFactors=F)
  names(df)<-c( "cas.att.eigen.vertex", "cas.att.eigen.value", "cas.att.eigen.number.of.vertices", "cas.att.eigen.maxcsize", "cas.att.eigen.cohesion", "cas.att.eigen.averagepath", "cas.att.eigen.adhesion", "cas.att.eigen.edgedensity", "cas.att.eigen.transitivity", "cas.att.eigen.radius", "cas.att.eigen.density", "cas.att.eigen.centralization", "cas.att.eigen.components")
  return(df)
}
