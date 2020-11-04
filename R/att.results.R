#attack results function
att.results <- function(attackscores){

                att <- attackscores[[1]]
                rand <- attackscores[[2]]

                pOut <- peakOutcome(att, rand)

                pExt <- att.extension(att, rand)

                pHalf <- netprop.at(att, rand, remainingvertex = floor(nrow(att) / 2))


                res.df <- list("Magnitude" = c(pOut), "Extension" = c(pExt), "Half Nodes" = c(pHalf))


                return(res.df)


}
