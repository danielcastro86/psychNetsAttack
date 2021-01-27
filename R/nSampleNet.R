

nSampleNet <- function(nsamples, samplesize, data){
  
  n <- nsamples
  t <- type
  
 
  global <- list()
  
  for(i in 1:n){
    
    gRand <- estimateNetwork(sample_n(data, samplesize), default = "ggmModSelect")
    qRand <- qgraph::qgraph(gRand$graph, layout="spring", directed= FALSE, theme = "TeamFortress", DoNotPlot=T)
    iRand <- as.igraph(qRand, attributes=TRUE)
    
    global[[i]] <- netDesc(iRand)
  }
  
  nodes <- integer()
  edges <- integer()
  negEdges <- integer()
  isoNodes <- integer()
  comp <- integer()
  avePath <- integer()
  dens <- integer()
  compact <- integer()
  globalClus <- integer()
  globalTransit <- integer()
  
  for (i in 1:5) {
    
  nodes[i] <- global[[i]][["Global Properties"]][["Nodes"]]
  edges[i] <- global[[i]][["Global Properties"]][["Edges"]]
  negEdges[i] <- global[[i]][["Global Properties"]][["Negative.Edges"]]
  isoNodes[i] <- global[[i]][["Global Properties"]][["Isolated.Nodes"]]
  comp[i] <- global[[i]][["Global Properties"]][["Components"]]
  avePath[i] <- global[[i]][["Global Properties"]][["Ave.Path"]]
  dens[i] <- global[[i]][["Global Properties"]][["Density"]]
  compact[i] <- global[[i]][["Global Properties"]][["Compactness"]]
  globalClus[i] <- global[[i]][["Global Properties"]][["Global.Clustering.Coefficient"]]
  globalTransit[i] <- global[[i]][["Global Properties"]][["Global.Transitivity"]]
  
    
    
  }
  
  mn <- mean(nodes)
  me <- mean(edges)
  mne <- mean(negEdges)
  miN <- mean(isoNodes)
  mcomp <- mean(comp)
  mcompact <- mean(compact)
  mavePath <- mean(avePath)
  mdens <- mean(dens)
  mglobalClus <- mean(globalClus)
  mglobalTransit <- mean(globalTransit)
  
  df <- data.frame(mn, me, mne, miN, mcomp, mcompact, mavePath, mdens, mglobalClus, mglobalTransit)
  names(df) <- c("nodes", "edges", "negative edges", "isolated nodes", "components",
                 "compactness", "average path lenght", "density", "global cluster.",
                 "global transit.")
  
  res.df <- list("Mean Values" = c(df), "Complete Results" = c(global))
  
  return(res.df)
  
}


 
 
 
 
