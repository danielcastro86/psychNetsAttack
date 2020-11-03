plot.att <- function(attackscores){


    scores <- nsscores

    #average path
    graphdata <- scores[[1]] %>% dplyr::select(3,6)
    graphdata <- cbind(graphdata, scores[[2]][[6]])

    colnames(graphdata) <- c("Number of Nodes in the Network", "Attack Score", "Random Attack Score")

    graphdata <- melt(graphdata, id.vars = "Number of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Number of Nodes in the Network` <- as.numeric(as.character(graphdata$`Number of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


  ggplot(graphdata, aes(x=`Number of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 1, to = max(graphdata$`Number of Nodes in the Network`, na.rm = T), by = 1))),
                         limits = c(1, max(graphdata$`Number of Nodes in the Network`, na.rm = T)))
    + theme_gray()

    #density
    graphdata <- scores[[1]] %>% dplyr::select(3,11)
    graphdata <- cbind(graphdata, scores[[2]][[11]])

    colnames(graphdata) <- c("Number of Nodes in the Network", "Attack Score", "Random Attack Score")

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
    graphdata <- scores[[1]] %>% dplyr::select(3,13)
    graphdata <- cbind(graphdata, scores[[2]][[13]])

    colnames(graphdata) <- c("Number of Nodes in the Network", "Attack Score", "Random Attack Score")

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

