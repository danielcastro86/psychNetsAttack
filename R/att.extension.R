#attack extension

att.extension <- function(att.scores, rand.scores){

  dat <- att.scores
  rand <- rand.scores

  dat[,c(2:ncol(dat))] <- sapply(dat[,c(2:ncol(dat))], as.numeric)
  dat$proportion.of.nodes <- dat[,3] / max(dat[,3], na.rm = T)
  dat <- mutate(dat, proportion.of.nodes= 1- dat$proportion.of.nodes)

  rand[,c(2:ncol(rand))] <- sapply(rand[,c(2:ncol(rand))], as.numeric)
  rand$proportion.of.nodes <- rand[,3] / max(rand[,3], na.rm = T)
  rand <- mutate(rand, proportion.of.nodes= 1- rand$proportion.of.nodes)

  pExt.components <- dat$proportion.of.nodes[max(dat[,13], na.rm = T)]
  pExt.pathlength <- dat$proportion.of.nodes[max(dat[,6], na.rm = T)]

  pExt.df <- hellno::data.frame("Components"=pExt.components, "Ave.Path.Length"= pExt.pathlength)

  pExt.rand.components <- rand$proportion.of.nodes[which.max(dat[,13])]
  pExt.rand.pathlength <- rand$proportion.of.nodes[which.max(dat[,6])]


  pExt.rand.df <- hellno::data.frame("Components"= pExt.rand.components, "Ave.Path.Length" = pExt.rand.pathlength)

  return(list("Attack Extension" = c(pExt.df), "Random Attack Extension" = c(pExt.rand.df)))
}
