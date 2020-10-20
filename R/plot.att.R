plot.att <- function(attackscores, attacktype, centmeas){

  if(attacktype=="normal" & centmeas== "strength"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="normal" & centmeas== "degree"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Degree", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Degree", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Degree", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="normal" & centmeas== "eigenvector"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Eigenvector", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Eigenvector", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Eigenvector", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="normal" & centmeas== "bridge strength"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="normal" & centmeas== "modal control"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Modal Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Modal Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Modal Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="normal" & centmeas== "average control"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Average Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Average Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Average Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="normal" & centmeas== "bridge expected influence 1-step"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Exp. Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Exp. Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Exp. Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))


  }

  if(attacktype=="normal" & centmeas== "bridge expected influence 2-step"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Exp. Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Exp. Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Bridge Exp. Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="normal" & centmeas== "expected influence 1-step"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Expected Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Expected Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Expected Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))
  }

  if(attacktype=="normal" & centmeas== "expected influence 2-step"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Expected Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Expected Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Normal Att. Expected Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))
  }

  if(attacktype=="cascade" & centmeas== "strength"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="cascade" & centmeas== "degree"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Degree", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Degree", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Degree", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="cascade" & centmeas== "eigenvector"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Eigenvector", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Eigenvector", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Eigenvector", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="cascade" & centmeas== "bridge strength"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Strength", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="cascade" & centmeas== "modal control"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Modal Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Modal Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Modal Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="cascade" & centmeas== "average control"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Average Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Average Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Average Control.", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="cascade" & centmeas== "bridge expected influence 1-step"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Exp. Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Exp. Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Exp. Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))


  }

  if(attacktype=="cascade" & centmeas== "bridge expected influence 2-step"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Exp. Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Exp. Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Bridge Exp. Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

  }

  if(attacktype=="cascade" & centmeas== "expected influence 1-step"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Expected Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Expected Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Expected Influence 1-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))
  }

  if(attacktype=="cascade" & centmeas== "expected influence 2-step"){

    scores <- attackscores

    #average path
    graphdata <- scores$`attack results` %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.averagepath)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Expected Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores$`attack results` %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.density)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Expected Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores$`attack results` %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores$`random attack`$random.att.components)

    colnames(graphdata) <- c("Number of Nodes in the Network", "Cascade Att. Expected Influence 2-step", "Random Att.")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))), limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T))) + theme_gray()

    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))
  }

}
