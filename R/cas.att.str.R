cas.att.str <- function(g){

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
  diameter <- integer(n-1)
  strength <- integer(n-1)
  vertex <- integer(n-1)
  components <- integer(n-1)



  #remoção dos vértices
  animation::saveGIF({ for(i in 1:(n-1)){
    vertex[i] <- V(g2)$names[which.max(igraph::strength(g2))] #cálculo e seleção do vértice a remover + registo do nome do vértice
    strength[i] <- max(igraph::strength(g2)) #registo do valor máximo da força


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
    diameter[i] <- igraph::diameter(g2)
    components[i] <- igraph::components(g2)$no

    g2 <- igraph::delete.vertices(g2, v=which.max(igraph::strength(g2))) #cálculo e seleção do vértice a remover

    #gráfico após cada remoção (layout não está fixo)
    plot(g2, vertex.size=20, vertex.color="darkolivegreen3",
         vertex.label=V(g2)$names, vertex.label.cex=0.8, edge.width=2)

  }}, movie.name = "graph.cas.att.str.gif")

  #tabela com os resultados do ataque

  df<-hellno::as.data.frame(cbind(c(vertex, NA), c(strength, NA), c(numberofvertices, NA), c(clustersizes, NA), c(cohesion, NA), c(averagepath, NA), c(adhesion, NA), c(edgedensity, NA), c(transitivity, NA), c(radius, NA), c(density, NA), c(centralization, NA), c(diameter, NA), c(components, NA)), stringAsFactors=F)
  names(df)<-c( "cas.att.str.vertex", "cas.att.str.value", "cas.att.str.number.of.vertices", "cas.att.str.maxcsize", "cas.att.str.cohesion", "cas.att.str.averagepath", "cas.att.str.adhesion", "cas.att.str.edgedensity", "cas.att.str.transitivity", "cas.att.str.radius", "cas.att.str.density", "cas.att.str.centralization", "cas.att.str.diameter", "cas.att.str.components")
  return(df)
}
