attackMe <- function(attack, measure, graph){

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
    att.df <- norm.att.bridstr(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "bridge strength"){

    rand.df <- random.att(g)
    att.df <- cas.att.bridstr(g)

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
    att.df <- norm.att.avecontrol(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "average control"){

    rand.df <- random.att(g)
    att.df <- cas.att.avecontrol(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="normal" & measure == "modal control"){

    rand.df <- random.att(g)
    att.df <- norm.att.modcontrol(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }

  if(attack=="cascade" & measure == "modal control"){

    rand.df <- random.att(g)
    att.df <- cas.att.modcontrol(g)

    return(list("attack results" = data.frame(att.df), "random attack" = data.frame(rand.df)))


  }
}
