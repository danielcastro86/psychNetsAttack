attack <- function(attack, measure, graph, adjmat){

  g<- graph

  if(attack=="normal" & measure=="strength"){

    rand.df <- random.att(g)
    att.df <- norm.att.str(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))
  }


  if(attack=="cascade" & measure== "strength"){

    rand.df <- random.att(g)
    att.df <- cas.att.str(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="normal" & measure == "degree"){

    rand.df <- random.att(g)
    att.df <- norm.att.deg(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "degree"){

    rand.df <- random.att(g)
    att.df <- cas.att.deg(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "bridge strength"){

    rand.df <- random.att(g)
    att.df <- norm.att.bridstr(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "bridge strength"){

    rand.df <- random.att(g)
    att.df <- cas.att.bridstr(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "eigenvector"){

    rand.df <- random.att(g)
    att.df <- norm.att.eigen(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "eigenvector"){

    rand.df <- random.att(g)
    att.df <- cas.att.eigen(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "average control"){

    rand.df <- random.att(g)
    att.df <- norm.att.avecontrol(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "average control"){

    rand.df <- random.att(g)
    att.df <- cas.att.avecontrol(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "modal control"){

    rand.df <- random.att(g)
    att.df <- norm.att.modcontrol(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "modal control"){

    rand.df <- random.att(g)
    att.df <- cas.att.modcontrol(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "bridge expected influence 1-step"){

    rand.df <- random.att(g)
    att.df <- norm.att.bridExpInfs1(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="cascade" & measure == "bridge expected influence 1-step"){

    rand.df <- random.att(g)
    att.df <- cas.att.bridExpInfs1(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

    }

  if(attack=="normal" & measure == "expected influence 1-step"){

    rand.df <- random.att(g)
    att.df <- norm.att.expInfs1(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="cascade" & measure == "expected influence 1-step"){

    rand.df <- random.att(g)
    att.df <- cas.att.expInfs1(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="normal" & measure == "bridge expected influence 2-step"){

    rand.df <- random.att(g)
    att.df <- norm.att.bridExpInfs2(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="cascade" & measure == "bridge expected influence 2-step"){

    rand.df <- random.att(g)
    att.df <- cas.att.bridExpInfs2(g, adjmat)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

  if(attack=="normal" & measure == "expected influence 2-step"){

    rand.df <- random.att(g)
    att.df <- norm.att.expInfs2(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }


  if(attack=="cascade" & measure == "expected influence 2-step"){

    rand.df <- random.att(g)
    att.df <- cas.att.expInfs1(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))

  }

}



