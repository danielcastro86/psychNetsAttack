nSampleNet <- function(nsamples, samplesize, data, method){

  n <- nsamples

  global <- list()

  for(i in 1:n){
    if(method == "ggmModSelect"){
      gRand <- estimateNetwork(sample_n(data, samplesize), default = "ggmModSelect")
    } else if(method == "BGGM1"){
      gRand <- BGGM::estimate(sample_n(data, samplesize), type = "ordinal")
      gRand <- BGGM::select(gRand)
    } else {
      stop("Method not recognized. Use 'ggmModSelect' or 'BGGM1'.")
    }

    qRand <- qgraph::qgraph(gRand$graph, layout="spring", directed=FALSE, theme="TeamFortress", DoNotPlot=T)
    iRand <- as.igraph(qRand, attributes=TRUE)

    global[[i]] <- netDesc(iRand)
  }

  # Inicializa as variáveis de propriedades
  nodes <- integer(n)
  edges <- integer(n)
  negEdges <- integer(n)
  isoNodes <- integer(n)
  comp <- integer(n)
  avePath <- numeric(n)
  dens <- numeric(n)
  globalTransit <- numeric(n)

  # Preenche as variáveis com os valores extraídos
  for (i in 1:n) {
    nodes[i] <- global[[i]][["Global Properties"]][["Nodes"]]
    edges[i] <- global[[i]][["Global Properties"]][["Edges"]]
    negEdges[i] <- global[[i]][["Global Properties"]][["Negative.Edges"]]
    isoNodes[i] <- global[[i]][["Global Properties"]][["Isolated.Nodes"]]
    comp[i] <- global[[i]][["Global Properties"]][["Components"]]
    avePath[i] <- global[[i]][["Global Properties"]][["Ave.Path"]]
    dens[i] <- global[[i]][["Global Properties"]][["Density"]]
    globalTransit[i] <- global[[i]][["Global Properties"]][["Global.Transitivity"]]
  }

  # Função para calcular intervalos de confiança
  calculate_ci <- function(x) {
    se <- sd(x) / sqrt(length(x))  # Erro padrão
    mean_x <- mean(x)
    lower <- mean_x - 1.96 * se
    upper <- mean_x + 1.96 * se
    return(c(mean = mean_x, lower = lower, upper = upper))
  }

  # Cálculo de médias e IC para cada métrica
  nodes_ci <- calculate_ci(nodes)
  edges_ci <- calculate_ci(edges)
  negEdges_ci <- calculate_ci(negEdges)
  isoNodes_ci <- calculate_ci(isoNodes)
  comp_ci <- calculate_ci(comp)
  avePath_ci <- calculate_ci(avePath)
  dens_ci <- calculate_ci(dens)
  globalTransit_ci <- calculate_ci(globalTransit)

  # Criação do data frame de resultados
  df <- data.frame(
    Metric = c("nodes", "edges", "negative edges", "isolated nodes", "components",
               "average path length", "density", "global transitivity"),
    Mean = c(nodes_ci["mean"], edges_ci["mean"], negEdges_ci["mean"], isoNodes_ci["mean"],
             comp_ci["mean"], avePath_ci["mean"], dens_ci["mean"], globalTransit_ci["mean"]),
    Lower_CI = c(nodes_ci["lower"], edges_ci["lower"], negEdges_ci["lower"], isoNodes_ci["lower"],
                 comp_ci["lower"], avePath_ci["lower"], dens_ci["lower"], globalTransit_ci["lower"]),
    Upper_CI = c(nodes_ci["upper"], edges_ci["upper"], negEdges_ci["upper"], isoNodes_ci["upper"],
                 comp_ci["upper"], avePath_ci["upper"], dens_ci["upper"], globalTransit_ci["upper"])
  )

  res.df <- list("Mean Values and CI" = df, "Complete Results" = global)

  return(res.df)
}
