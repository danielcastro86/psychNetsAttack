attack <- function(attack, measure, graph){

  g <- graph

  gMat <- as_adjacency_matrix(g, attr = "weight")

  fixed <- layout.fruchterman.reingold(g)

  #atribuir um nome unico aos vértices
  V(g)$names <- paste0("v", 1:length(V(g)))
  V(g)$x <- fixed[,1]
  V(g)$y <- fixed[,2]

  colnames(gMat) <- V(g)$names

  if(attack=="normal" & measure=="strength"){

    rand.df <- random.att(g)
    att.df <- norm.att.str(g)

    return(list("normal attack strength scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))
    assign(paste0(attack, " attack scores ", measure), att.df)

  }

  if(attack=="cascade" & measure== "strength"){

    rand.df <- random.att(g)
    att.df <- cas.att.str(g)

    return(list("cascade attack strength scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="normal" & measure == "degree"){

    rand.df <- random.att(g)
    att.df <- norm.att.deg(g)

    return(list("normal attack degree scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "degree"){

    rand.df <- random.att(g)
    att.df <- cas.att.deg(g)

    return(list("cascade attack degree scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "bridge strength"){

    rand.df <- random.att(g)
    att.df <- norm.att.bridstr(g, gMat)

    return(list("normal attack bridge strength scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "bridge strength"){

    rand.df <- random.att(g)
    att.df <- cas.att.bridstr(g, gMat)

    return(list("cascade attack bridge strength scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "eigenvector"){

    rand.df <- random.att(g)
    att.df <- norm.att.eigen(g)

    return(list("normal attack eigenvector scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "eigenvector"){

    rand.df <- random.att(g)
    att.df <- cas.att.eigen(g)

    return(list("cascade attack eigenvector scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "average control"){

    rand.df <- random.att(g)
    att.df <- norm.att.avecontrol(g, gMat)

    return(list("normal attack average control scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "average control"){

    rand.df <- random.att(g)
    att.df <- cas.att.avecontrol(g, gMat)

    return(list("cascade attack average control scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "modal control"){

    rand.df <- random.att(g)
    att.df <- norm.att.modcontrol(g, gMat)

    return(list("normal attack modal control scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "modal control"){

    rand.df <- random.att(g)
    att.df <- cas.att.modcontrol(g, gMat)

    return(list("cascade attack modal control scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "bridge expected influence 1-step"){

    rand.df <- random.att(g)
    att.df <- norm.att.bridExpInfs1(g, gMat)

    return(list("normal attack bridge expected influence 1-step scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="cascade" & measure == "bridge expected influence 1-step"){

    rand.df <- random.att(g)
    att.df <- cas.att.bridExpInfs1(g, gMat)

    return(list("cascade attack bridge expected influence 1-step scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

    }

  if(attack=="normal" & measure == "expected influence 1-step"){

    rand.df <- random.att(g)
    att.df <- norm.att.expInfs1(g)

    return(list("normal attack expected influence 1-step scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="cascade" & measure == "expected influence 1-step"){

    rand.df <- random.att(g)
    att.df <- cas.att.expInfs1(g)

    return(list("cascade attack expected influence 1-step scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="normal" & measure == "bridge expected influence 2-step"){

    rand.df <- random.att(g)
    att.df <- norm.att.bridExpInfs2(g, gMat)

    return(list("normal attack bridge expected influence 2-step scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="cascade" & measure == "bridge expected influence 2-step"){

    rand.df <- random.att(g)
    att.df <- cas.att.bridExpInfs2(g, gMat)

    return(list("cascade attack bridge expected influence 2-step scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="normal" & measure == "expected influence 2-step"){

    rand.df <- random.att(g)
    att.df <- norm.att.expInfs2(g)

    return(list("normal attack expected influence 2-step scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }


  if(attack=="cascade" & measure == "expected influence 2-step"){

    rand.df <- random.att(g)
    att.df <- cas.att.expInfs1(g)

    return(list("cascade attack expected influence 2-step scores" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

}



