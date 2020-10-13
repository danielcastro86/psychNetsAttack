att.results <- function(attackscores, attacktype, centmeas, totalvertex, remainingvertex, randomscores){

  dat <- attackscores
  n <- totalvertex
  rn <- remainingvertex

  dat[,c(2:ncol(dat))] <- sapply(dat[,c(2:ncol(dat))], as.numeric)
  dat$proportion.of.nodes <- dat[,3] / totalvertex
  dat <- mutate(dat, proportion.of.nodes= 1- dat$proportion.of.nodes)

  rand <- randomscores
  rand[,c(2:ncol(rand))] <- sapply(rand[,c(2:ncol(rand))], as.numeric)
  rand$proportion.of.nodes <- rand[,3] / totalvertex
  rand <- mutate(rand, proportion.of.nodes= 1- rand$proportion.of.nodes)

  if(attacktype == "normal" & centmeas == "strength"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.str.averagepath, na.rm = T) - subset(dat$norm.att.str.averagepath, dat$norm.att.str.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.str.components, na.rm = T) - subset(dat$norm.att.str.components, dat$norm.att.str.number.of.vertices==n)

    pOut.df <- hellno::data.frame("Peak.Outcome.Components" = pOut.components,"Peak.Outcome.Ave.Path.Length" = pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.str.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.str.components
    pNodes.pathlength <- pNodes$norm.att.str.averagepath
    pNodes.density <- pNodes$norm.att.str.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)
    colnames(pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.str.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.str.averagepath)]

    pExt.df <- hellno::data.frame("Attack.Extension.Components"=pExt.components, "Attack.Extension.Ave.Path.Length"= pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype == "cascade" & centmeas=="strength"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.str.averagepath, na.rm = T) - subset(dat$cas.att.str.averagepath, dat$cas.att.str.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.str.components, na.rm = T) - subset(dat$cas.att.str.components, dat$cas.att.str.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.str.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.str.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.str.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.str.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)
    colnames(cas.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(cas.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(cas.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.str.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.str.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype == "normal" & centmeas=="degree"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.deg.averagepath, na.rm = T) - subset(dat$norm.att.deg.averagepath, dat$norm.att.deg.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.deg.components, na.rm = T) - subset(dat$norm.att.deg.components, dat$norm.att.deg.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.deg.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.deg.components
    pNodes.pathlength <- pNodes$norm.att.deg.averagepath
    pNodes.density <- pNodes$norm.att.deg.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)
    colnames(pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.deg.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.deg.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype=="cascade" & centmeas== "degree"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.deg.averagepath, na.rm = T) - subset(dat$cas.att.deg.averagepath, dat$cas.att.deg.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.deg.components, na.rm = T) - subset(dat$cas.att.deg.components, dat$cas.att.deg.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.deg.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.deg.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.deg.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.deg.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)
    colnames(cas.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(cas.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(cas.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.deg.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.deg.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype=="normal" & centmeas=="eigenvector"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.eigen.averagepath, na.rm = T) - subset(dat$norm.att.eigen.averagepath, dat$norm.att.eigen.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.eigen.components, na.rm = T) - subset(dat$norm.att.eigen.components, dat$norm.att.eigen.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.eigen.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.eigen.components
    pNodes.pathlength <- pNodes$norm.att.eigen.averagepath
    pNodes.density <- pNodes$norm.att.eigen.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.eigen.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.eigen.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype== "cascade" & centmeas=="eigenvector"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.eigen.averagepath, na.rm = T) - subset(dat$cas.att.eigen.averagepath, dat$cas.att.eigen.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.eigen.components, na.rm = T) - subset(dat$cas.att.eigen.components, dat$cas.att.eigen.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.eigen.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.eigen.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.eigen.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.eigen.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.eigen.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.eigen.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype== "normal" & centmeas=="bridge strength"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.avecont.averagepath, na.rm = T) - subset(dat$norm.att.avecont.averagepath, dat$norm.att.avecont.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.avecont.components, na.rm = T) - subset(dat$norm.att.avecont.components, dat$norm.att.avecont.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.avecont.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.avecont.components
    pNodes.pathlength <- pNodes$norm.att.avecont.averagepath
    pNodes.density <- pNodes$norm.att.avecont.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.avecont.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.avecont.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype== "cascade" & centmeas== "bridge strength"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.bridstr.averagepath, na.rm = T) - subset(dat$cas.att.bridstr.averagepath, dat$cas.att.bridstr.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.bridstr.components, na.rm = T) - subset(dat$cas.att.bridstr.components, dat$cas.att.bridstr.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.bridstr.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.bridstr.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.bridstr.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.bridstr.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.bridstr.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.bridstr.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype=="normal" & centmeas== "average control"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.avecont.averagepath, na.rm = T) - subset(dat$norm.att.avecont.averagepath, dat$norm.att.avecont.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.avecont.components, na.rm = T) - subset(dat$norm.att.avecont.components, dat$norm.att.avecont.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.avecont.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.avecont.components
    pNodes.pathlength <- pNodes$norm.att.avecont.averagepath
    pNodes.density <- pNodes$norm.att.avecont.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.avecont.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.avecont.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype=="cascade" & centmeas== "averagecontrol"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.avecont.averagepath, na.rm = T) - subset(dat$cas.att.avecont.averagepath, dat$cas.att.avecont.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.avecont.components, na.rm = T) - subset(dat$cas.att.avecont.components, dat$cas.att.avecont.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.avecont.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.avecont.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.avecont.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.avecont.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.avecont.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.avecont.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype=="normal" & centmeas=="modal control"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.modcontrol.averagepath, na.rm = T) - subset(dat$norm.att.modcontrol.averagepath, dat$norm.att.modcontrol.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.modcontrol.components, na.rm = T) - subset(dat$norm.att.modcontrol.components, dat$norm.att.modcontrol.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.modcontrol.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.modcontrol.components
    pNodes.pathlength <- pNodes$norm.att.modcontrol.averagepath
    pNodes.density <- pNodes$norm.att.modcontrol.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.modcontrol.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.modcontrol.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype=="cascade" & centmeas== "modal control"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.modcontrol.averagepath, na.rm = T) - subset(dat$cas.att.modcontrol.averagepath, dat$cas.att.modcontrol.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.modcontrol.components, na.rm = T) - subset(dat$cas.att.modcontrol.components, dat$cas.att.modcontrol.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.modcontrol.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.modcontrol.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.modcontrol.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.modcontrol.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.modcontrol.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.modcontrol.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype== "normal" & centmeas == "bridge expected influence 1-step"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.bridExpInfs1.averagepath, na.rm = T) - subset(dat$norm.att.bridExpInfs1.averagepath, dat$norm.att.bridExpInfs1.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.bridExpInfs1.components, na.rm = T) - subset(dat$norm.att.bridExpInfs1.components, dat$norm.att.bridExpInfs1.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.bridExpInfs1.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.bridExpInfs1.components
    pNodes.pathlength <- pNodes$norm.att.bridExpInfs1.averagepath
    pNodes.density <- pNodes$norm.att.bridExpInfs1.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.bridExpInfs1.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.bridExpInfs1.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype=="cascade" & centmeas== "bridge expected influence 1-step"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.bridExpInfs1.averagepath, na.rm = T) - subset(dat$cas.att.bridExpInfs1.averagepath, dat$cas.att.bridExpInfs1.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.bridExpInfs1.components, na.rm = T) - subset(dat$cas.att.bridExpInfs1.components, dat$cas.att.bridExpInfs1.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.bridExpInfs1.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.bridExpInfs1.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.bridExpInfs1.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.bridExpInfs1.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.bridExpInfs1.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.bridExpInfs1.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype== "normal" & centmeas == "bridge expected influence 2-step"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.bridExpInfs2.averagepath, na.rm = T) - subset(dat$norm.att.bridExpInfs2.averagepath, dat$norm.att.bridExpInfs2.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.bridExpInfs2.components, na.rm = T) - subset(dat$norm.att.bridExpInfs2.components, dat$norm.att.bridExpInfs2.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.bridExpInfs2.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.bridExpInfs2.components
    pNodes.pathlength <- pNodes$norm.att.bridExpInfs2.averagepath
    pNodes.density <- pNodes$norm.att.bridExpInfs2.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.bridExpInfs2.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.bridExpInfs2.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype== "cascade" & centmeas== "bridge expected influence 2-step"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.bridExpInfs2.averagepath, na.rm = T) - subset(dat$cas.att.bridExpInfs2.averagepath, dat$cas.att.bridExpInfs2.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.bridExpInfs2.components, na.rm = T) - subset(dat$cas.att.bridExpInfs2.components, dat$cas.att.bridExpInfs2.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.bridExpInfs2.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.bridExpInfs2.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.bridExpInfs2.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.bridExpInfs2.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.bridExpInfs2.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.bridExpInfs2.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))

  }

  if(attacktype== "normal" & centmeas== "expected influence 1-step"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.expInfs1.averagepath, na.rm = T) - subset(dat$norm.att.expInfs1.averagepath, dat$norm.att.expInfs1.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.expInfs1.components, na.rm = T) - subset(dat$norm.att.expInfs1.components, dat$norm.att.expInfs1.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.expInfs1.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.expInfs1.components
    pNodes.pathlength <- pNodes$norm.att.expInfs1.averagepath
    pNodes.density <- pNodes$norm.att.expInfs1.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.expInfs1.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.expInfs1.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))

  }

  if(attacktype== "cascade" & centmeas== "expected influence 1-step"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.expInfs1.averagepath, na.rm = T) - subset(dat$cas.att.expInfs1.averagepath, dat$cas.att.expInfs1.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.expInfs1.components, na.rm = T) - subset(dat$cas.att.expInfs1.components, dat$cas.att.expInfs1.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.expInfs1.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.expInfs1.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.expInfs1.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.expInfs1.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.expInfs1.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.expInfs1.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }

  if(attacktype== "normal" & centmeas== "expected influence 2-step"){

    #peak outcome [difference between the maximum and initial values]
    pOut.pathlength <- max(dat$norm.att.expInfs2.averagepath, na.rm = T) - subset(dat$norm.att.expInfs2.averagepath, dat$norm.att.expInfs2.number.of.vertices==n)
    pOut.components <- max(dat$norm.att.expInfs2.components, na.rm = T) - subset(dat$norm.att.expInfs2.components, dat$norm.att.expInfs2.number.of.vertices==n)

    pOut.df <- hellno::data.frame(pOut.components, pOut.pathlength)

    #network properties with the specified remaining vertex
    pNodes <- subset(dat, norm.att.expInfs2.number.of.vertices == rn)

    pNodes.components <- pNodes$norm.att.expInfs2.components
    pNodes.pathlength <- pNodes$norm.att.expInfs2.averagepath
    pNodes.density <- pNodes$norm.att.expInfs2.density

    pNodes.df <- hellno::data.frame(pNodes.components, pNodes.pathlength, pNodes.density)

    #proportion of nodes removed at maximum values
    pExt.components <- dat$proportion.of.nodes[which.max(dat$norm.att.expInfs2.components)]
    pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$norm.att.expInfs2.averagepath)]

    pExt.df <- c(pExt.components, pExt.pathlength)

    df <- cbind(pExt.df, pNodes.df, pOut.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))

  }

  if(attacktype== "cascade" & centmeas== "expected influence 2-step"){

    #peak outcome
    cas.pOut.pathlength <- max(dat$cas.att.expInfs2.averagepath, na.rm = T) - subset(dat$cas.att.expInfs2.averagepath, dat$cas.att.expInfs2.number.of.vertices==n)
    cas.pOut.components <- max(dat$cas.att.expInfs2.components, na.rm = T) - subset(dat$cas.att.expInfs2.components, dat$cas.att.expInfs2.number.of.vertices==n)

    cas.pOut.df <- hellno::data.frame(cas.pOut.components, cas.pOut.pathlength)

    #network properties with the specified remaining vertex
    cas.pNodes <- subset(dat, cas.att.expInfs2.number.of.vertices == rn)

    cas.pNodes.components <- cas.pNodes$cas.att.expInfs2.components
    cas.pNodes.pathlength <- cas.pNodes$cas.att.expInfs2.averagepath
    cas.pNodes.density <- cas.pNodes$cas.att.expInfs2.density

    cas.pNodes.df <- hellno::data.frame(cas.pNodes.components, cas.pNodes.pathlength, cas.pNodes.density)

    #proportion of nodes removed at maximum values
    cas.pExt.components <- dat$proportion.of.nodes[which.max(dat$cas.att.expInfs2.components)]
    cas.pExt.pathlength <- dat$proportion.of.nodes[which.max(dat$cas.att.expInfs2.averagepath)]

    cas.pExt.df <- hellno::data.frame(cas.pExt.components, cas.pExt.pathlength)

    df <- cbind(cas.pOut.df, cas.pNodes.df, cas.pExt.df)

    #add random results
    #Peak Outcome
    rand.pOut.pathlength <- max(rand$random.att.averagepath, na.rm = T) - subset(rand$random.att.averagepath, rand$randon.att.number.of.vertices==n)
    rand.pOut.components <- max(rand$random.att.components, na.rm = T) - subset(rand$random.att.components, rand$randon.att.number.of.vertices==n)

    rand.pOut.df <- hellno::data.frame("Peak.Outcome.Components"= rand.pOut.components, "Peak.Outcome.Ave.Path.Length" = rand.pOut.pathlength)

    #network properties with the specified remaining vertex
    rand.pNodes <- subset(rand, randon.att.number.of.vertices == rn)

    rand.pNodes.components <- rand.pNodes$random.att.components
    rand.pNodes.pathlength <- rand.pNodes$random.att.averagepath
    rand.pNodes.density <- rand.pNodes$random.att.density

    rand.pNodes.df <- hellno::data.frame(rand.pNodes.components, rand.pNodes.pathlength, rand.pNodes.density)
    colnames(rand.pNodes.df)[1] <- c(paste0("Components.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[2] <- c(paste0("Ave.Path.Length.With.", rn, ".Nodes.Network"))
    colnames(rand.pNodes.df)[3] <- c(paste0("Density.With.", rn, ".Nodes.Network"))

    #proportion of nodes removed at maximum values
    rand.pExt.components <- rand$proportion.of.nodes[which.max(rand$random.att.components)]
    rand.pExt.pathlength <- rand$proportion.of.nodes[which.max(rand$random.att.averagepath)]

    rand.pExt.df <- hellno::data.frame("Attack.Extension.Components" = rand.pExt.components, "Attack.Extension.Ave.Path.Length"= rand.pExt.pathlength)

    rand.att.df <- cbind(rand.pExt.df, rand.pNodes.df, rand.pOut.df)

    #add net descriptives
    netDesc.pathLength <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.averagepath)
    netDesc.components <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.components)
    netDesc.density <- subset(dat, dat$norm.att.str.number.of.vertices==n) %>% dplyr::pull(norm.att.str.density)

    netDesc.df <- hellno::data.frame(netDesc.density, netDesc.components, netDesc.pathLength)

    return(list("attack results" = c(df), "net descriptives" = c(netDesc.df), "random attack" = c(rand.att.df)))
  }


}
