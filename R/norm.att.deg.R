norm.att.deg <- function(g){
  #numero de repetições
  n <- length(V(g))

  #criar tabela
  mat <- matrix(ncol=2,nrow=n, 0)
  #adicionar o nome dos vértices
  mat[,1] <- V(g)$names
  #calcular medida de centralidade
  deg <- igraph::degree(g)
  #adcionar valores de centralidade à tabela
  mat[,2] <- deg
  #ordenar vértices pelo valor da centralidade
  matri <- mat[order(as.numeric(as.character(mat[,2])), decreasing = TRUE),]

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
  components <- integer(n-1)


 animation::saveGIF({ for(i in 1:(n-1)){


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

    g2 <- igraph::delete.vertices(g2, v=which(V(g2)$names==matri[i,1])) #remover vértice pela ordem na tabela

    #gráfico após cada remoção (layout não está fixo)
    plot(g2, vertex.size=20, vertex.color="darkolivegreen3",
         vertex.label=labels, vertex.label.cex=0.8, edge.width=2)
  }}, movie.name = "graph.norm.att.deg.gif")
  #tabela com os resultados do ataque
  df<-hellno::as.data.frame(cbind(matri, c(numberofvertices, NA), c(clustersizes, NA), c(cohesion, NA), c(averagepath, NA), c(adhesion, NA), c(edgedensity, NA), c(transitivity, NA), c(radius, NA), c(density, NA), c(centralization, NA), c(diameter, NA), c(components, NA)), stringAsFactors=FALSE)
  names(df)<-c( "norm.att.deg.vertex", "norm.att.deg.value", "norm.att.deg.number.of.vertices", "norm.att.deg.maxcsize", "norm.att.deg.cohesion", "norm.att.deg.averagepath", "norm.att.deg.adhesion", "norm.att.deg.edgedensity", "norm.att.deg.transitivity", "norm.att.deg.radius", "norm.att.deg.density", "norm.att.deg.centralization", "norm.att.deg.diameter", "norm.att.deg.components")
  return(df)
}
