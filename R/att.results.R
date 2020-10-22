#attack results function
att.results <- function(attackscores, randomscores){

                att <- attackscores
                rand <- randomscores

                pOut <- peakOutcome(att, rand)

                pExt <- att.extension(att, rand)

                pHalf <- netprop.at(att, rand, remainingvertex = floor(nrow(att) / 2))


                res.df <- list("Peak Outcome" = pOut, "Extension" = pExt, "Half Nodes" = pHalf)


                return(res.df)


}
