#attack results function

peakOutcome <- function(att.scores, rand.scores){

  dat <- att.scores
  rand <- rand.scores

  dat[,c(2:ncol(dat))] <- sapply(dat[,c(2:ncol(dat))], as.numeric)
  dat$proportion.of.nodes <- dat[,3] / max(dat[,3])
  dat <- mutate(dat, proportion.of.nodes= 1- dat$proportion.of.nodes)

  rand[,c(2:ncol(rand))] <- sapply(rand[,c(2:ncol(rand))], as.numeric)
  rand$proportion.of.nodes <- rand[,3] / max(rand[,3])
  rand <- mutate(rand, proportion.of.nodes= 1- rand$proportion.of.nodes)

  pOut.pathlength <- max(dat[,6], na.rm = T) - subset(dat[,6], dat[,3] == nrow(dat))
  pOut.component <- max(dat[,13], na.rm = T) - subset(dat[,13], dat[,3] == nrow(dat))

  pOut.df <- hellno::data.frame("Components" = pOut.component,"Ave.Path.Length" = pOut.pathlength)

  pOut.rand.pathlength <- max(rand[,6], na.rm = T) - subset(rand[,6], rand[,3] == nrow(rand))
  pOut.rand.component <- max(rand[,13], na.rm = T) - subset(rand[,13], dat[,3] == nrow(rand))

  pOut.rand.df <- hellno::data.frame("Components" = pOut.rand.component,"Ave.Path.Length" = pOut.rand.pathlength)

  return(list("Attack Peak Outcome" = c(pOut.df), "Random Peak Outcome" = c(pOut.rand.df)))
}




