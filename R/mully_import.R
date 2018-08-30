
importGraph<-function(name,direct="F",layers,nodes,edges){
  if(missing(name) || name="" ||
     missing(layers) || !file.exists(layers) ||
     missing(nodes) || !file.exists(nodes) ||
     missing(edges) || !file.exists(edges)){
    stop("Invalid arguments")
  }
  g=mully(name,direct)
  g=importLayers(g,layers)
  g=importNodes(g,nodes)
  g=importEdges(g,edges)
  return(g)
}

importLayers<-function(g,file){
  if(missing(g) || missing(file) || !file.exists(file)){
    stop("Invalid arguments")
  }
  layers=read.csv(file)

  return(g)
}

importNodes<-function(g,file){
  if(missing(g) || missing(file) || !file.exists(file)){
    stop("Invalid arguments")
  }
  nodes=read.csv(file)

  return(g)
}

importEdges<-function(g,file){
  if(missing(g) || missing(file) || !file.exists(file)){
    stop("Invalid arguments")
  }
  edges=read.csv(file)

  return(g)
}
