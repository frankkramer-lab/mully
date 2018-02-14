getLayout<-function(g,layout){
  if(layout=="random")
    return(getRandomLayout(g))
  if(layout=="scaled")
    return(getScaledLayout(g))
  return(NULL)
}

getRandomLayout<-function(g){
  y=as.vector(V(g)$n)
  g2=g
  for(i in 1:getLayersCount(g2)){
    layer=getLayerByID(g,i)
    if(is.null(layer) || is.na(layer))
      next

    xs=runif(length(as.vector(layer)),-4,4)
    V(g2)[which(V(g)$n==i)]$x=xs
  }
  return(cbind(V(g2)$x,y))
}

getScaledLayout<-function(g){
  y=as.vector(V(g)$n)
  g2=g
  for(i in 1:getLayersCount(g2)){
    layer=getLayerByID(g,i)
    if(is.null(layer) || is.na(layer))
      next

    xs=(1:length(layer))*8/length(layer)
    V(g2)[which(V(g)$n==i)]$x=xs
  }
  return(cbind(V(g2)$x,y))
}

getMarkGroups<-function(g){
  l=list()
  for(i in 1:g$iLayer){
    li=list(which(V(g)$n==i))
    l=append(l,li)
  }
  return(l)
}

plot.mully<-function(g,layout){
  plot.igraph(g,mark.groups = getMarkGroups(g),layout=getLayout(g,layout))
}
