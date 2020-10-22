#network properties at specified number of vertex

netprop.at <- function(att.scores, rand.scores, remainingvertex){

  dat <- att.scores
  rand <- rand.scores
  rv <- remainingvertex

  dat[,c(2:ncol(dat))] <- sapply(dat[,c(2:ncol(dat))], as.numeric)
  dat$proportion.of.nodes <- dat[,3] / max(dat[,3], na.rm = T)
  dat <- mutate(dat, proportion.of.nodes= 1- dat$proportion.of.nodes)

  rand[,c(2:ncol(rand))] <- sapply(rand[,c(2:ncol(rand))], as.numeric)
  rand$proportion.of.nodes <- rand[,3] / max(rand[,3], na.rm = T)
  rand <- mutate(rand, proportion.of.nodes= 1- rand$proportion.of.nodes)

  pNodes <- subset(dat, dat[,3] == rv)

  pNodes.components <- pNodes[,13]
  pNodes.pathlength <- pNodes[,6]
  pNodes.density <- pNodes[,11]

  pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)
  colnames(pNodes.df)[1] <- "Components"
  colnames(pNodes.df)[2] <- "Ave.Path.Length"
  colnames(pNodes.df)[3] <- "Density"

  pNodes.rand <- subset(rand, rand[,3] == rv)

  pNodes.rand.components <- pNodes.rand[,13]
  pNodes.rand.pathlength <- pNodes.rand[,6]
  pNodes.rand.density <- pNodes.rand[,11]

  pNodes.rand.df <- hellno::data.frame(pNodes.rand.components, pNodes.rand.pathlength, pNodes.rand.density)
  colnames(pNodes.rand.df)[1] <- "Components"
  colnames(pNodes.rand.df)[2] <- "Ave.Path.Length"
  colnames(pNodes.rand.df)[3] <- "Density"


  prop.at <- list(pNodes.df, pNodes.rand.df)

  names(prop.at) = c(paste0("Attack results after removing ",round(abs((rv*100)/max(dat[,3], na.rm = T)-100), digits = 0), "% of the nodes"),
                     paste0("Random attack results after removing ",round(abs((rv*100)/max(rand[,3], na.rm = T)-100), digits = 0), "% of the nodes"))

  return(prop.at)

}

