plot.att <- function(attackscores){


    scores <- attackscores


    scores[[1]][,c(2:ncol(scores[[1]]))] <- sapply(scores[[1]][,c(2:ncol(scores[[1]]))], as.numeric)
    scores[[1]]$proportion.of.nodes <- scores[[1]][,3] / max(scores[[1]][,3], na.rm = T)
    scores[[1]] <- mutate(scores[[1]], proportion.of.nodes= 1- scores[[1]]$proportion.of.nodes)

    scores[[2]][,c(2:ncol(scores[[2]]))] <- sapply(scores[[2]][,c(2:ncol(scores[[2]]))], as.numeric)
    scores[[2]]$proportion.of.nodes <- scores[[2]][,3] / max(scores[[2]][,3], na.rm = T)
    scores[[2]] <- mutate(scores[[2]], proportion.of.nodes= 1- scores[[2]]$proportion.of.nodes)

    #average path
    graphdata <- scores[[1]] %>% dplyr::select(14,6)
    graphdata <- cbind(graphdata, scores[[2]][[6]])

    colnames(graphdata) <- c("Proportion of Nodes in the Network", "Attack Score", "Random Attack Score")

    graphdata <- melt(graphdata, id.vars = "Proportion of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Proportion of Nodes in the Network` <- as.numeric(as.character(graphdata$`Proportion of Nodes in the Network`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Proportion of Nodes in the Network`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 0, to = max(graphdata$`Proportion of Nodes in the Network`, na.rm = T), by= 0.5))),
                         limits = c(0, max(graphdata$`Proportion of Nodes in the Network`, na.rm = T))) + theme_gray()

    #density
    graphdata <- scores[[1]] %>% dplyr::select(14,11)
    graphdata <- cbind(graphdata, scores[[2]][[11]])

    colnames(graphdata) <- c("Proportion of Nodes in the Network", "Attack Score", "Random Attack Score")

    graphdata <- melt(graphdata, id.vars = "Proportion of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Proportion of Nodes in the Network` <- as.numeric(as.character(graphdata$`Proportion of Nodes in the Network`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Proportion of Nodes in the Network`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 0, to = max(graphdata$`Proportion of Nodes in the Network`, na.rm = T), by=0.5))), limits = c(0, max(graphdata$`Proportion of Nodes in the Network`, na.rm = T))) + theme_gray()

    #components
    graphdata <- scores[[1]] %>% dplyr::select(14,13)
    graphdata <- cbind(graphdata, scores[[2]][[13]])

    colnames(graphdata) <- c("Proportion of Nodes in the Network", "Attack Score", "Random Attack Score")

    graphdata <- melt(graphdata, id.vars = "Proportion of Nodes in the Network")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Proportion of Nodes in the Network` <- as.numeric(as.character(graphdata$`Proportion of Nodes in the Network`))
    colnames(graphdata)[3] <- "Proportion of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Proportion of Nodes in the Network`,
                                            y= `Proportion of Components`,
                                            group= variable,
                                            color=variable)) +

      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(breaks = c(rev(seq(from = 0, to = max(graphdata$`Proportion of Nodes in the Network`, na.rm = T), by=0.5))), limits = c(0, max(graphdata$`Proportion of Nodes in the Network`, na.rm = T))) + theme_gray()


    return(grid.arrange(AvePathPlot, DensityPlot, ComponentsPlot, nrow=3, ncol=1, top=""))

}
