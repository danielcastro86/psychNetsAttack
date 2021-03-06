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

    colnames(graphdata) <- c("Proportion of Nodes Removed", paste0(to_any_case(names(scores)[[1]], case = "title")), "Random Attack Scores")

    graphdata <- melt(graphdata, id.vars = "Proportion of Nodes Removed")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Proportion of Nodes Removed` <- as.numeric(as.character(graphdata$`Proportion of Nodes Removed`))
    colnames(graphdata)[3] <- "Average Path Length"


    AvePathPlot <- ggplot(graphdata, aes(x=`Proportion of Nodes Removed`,
                                         y= `Average Path Length`,
                                         group= variable,
                                         color=variable)) +
      scale_color_manual(values= c("#46ACC8", "#E58601")) +
      labs(color="Attack Type") +
      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(labels = function(x) paste0(x * 100, '%'), breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)) + theme_light()

    #density
    graphdata <- scores[[1]] %>% dplyr::select(14,11)
    graphdata <- cbind(graphdata, scores[[2]][[11]])

    colnames(graphdata) <- c("Proportion of Nodes Removed", paste0(to_any_case(names(scores)[[1]], case = "title")), "Random Attack Scores")

    graphdata <- melt(graphdata, id.vars = "Proportion of Nodes Removed")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Proportion of Nodes Removed` <- as.numeric(as.character(graphdata$`Proportion of Nodes Removed`))
    colnames(graphdata)[3] <- "Network Density"

    DensityPlot <- ggplot(graphdata, aes(x=`Proportion of Nodes Removed`,
                                         y= `Network Density`,
                                         group= variable,
                                         color=variable)) +
      scale_color_manual(values= c("#46ACC8", "#E58601")) +
      labs(color="Attack Type") +
      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(labels = function(x) paste0(x * 100, '%'), breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)) + theme_light()

    #components
    graphdata <- scores[[1]] %>% dplyr::select(14,13)
    graphdata <- cbind(graphdata, scores[[2]][[13]])

    colnames(graphdata) <- c("Proportion of Nodes Removed", paste0(to_any_case(names(scores)[[1]], case = "title")), "Random Attack Scores")

    graphdata <- melt(graphdata, id.vars = "Proportion of Nodes Removed")
    graphdata$value <- as.numeric(as.character(graphdata$value))
    graphdata$`Proportion of Nodes Removed` <- as.numeric(as.character(graphdata$`Proportion of Nodes Removed`))
    colnames(graphdata)[3] <- "Number of Components"

    ComponentsPlot <- ggplot(graphdata, aes(x=`Proportion of Nodes Removed`,
                                            y= `Number of Components`,
                                            group= variable,
                                            color=variable)) +
      scale_color_manual(values= c("#46ACC8", "#E58601")) +
      labs(color="Attack Type") +
      geom_line(linetype = "dotted") +
      geom_point(shape = 19, size = 2) +
      scale_y_continuous() +
      scale_x_continuous(labels = function(x) paste0(x * 100, '%'), breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)) + theme_light()




    p <- ggpubr::ggarrange(AvePathPlot, DensityPlot, ComponentsPlot, ncol = 3, nrow = 1, common.legend = T, legend = "bottom")
   p1 <- ggpubr::annotate_figure(p, top=ggpubr::text_grob(paste0(to_any_case(names(scores)[[1]], case = "title")), face = "bold"))

    return(print(p1))
}

