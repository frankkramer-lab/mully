########### Constructor ##################

#' Create an empty multilayered graph
#'
#' @param name The name to be assigned to the graph.
#' @param direct A boolean value, if the graph is directed or not. By default TRUE.
#'
#' @return The created multilayered graph.
#' @export
mully <- function(name = NA, direct = TRUE) {
  #Create the layers Indexation Table
  #Vector for the layers indexation
  #One row is not enough (in case we delete layers)
  layersVect = data.frame(ID = is.numeric(c()),
                          Name = is.character(c()),
                          NameLower = is.character(c()))
  #Delete first entry F F F
  layersVect = layersVect[-1,]

  #Create an empty igraph instance
  g <- graph.empty(directed = direct)

  #Assign the attributes to the igraph instance
  g$name = name
  #Last inserted layer number
  g$iLayer = 0
  #DataFrame that contains the layers
  g$layers = layersVect

  #name the class
  class(g) = c("mully", class(g))
  return(g)
}

is.mully<-function(g){
  if(!is.igraph(g) || is.na(g$layers) || is.na(g$iLayer))
    return(F)
  return(T)
}

#' Merge or unite two graphs
#'
#' @param g1 The first graph to merge. This is the base of the merge.
#' @param g2 The second graph to merge. All of its elements are added to the first graph.
#'
#' @return The merge of the two graphs. The merge is based on the first given graph
#' @export
merge<-function(g1,g2){
  if(!is.mully(g1) || !is.mully(g2))
   stop("Invalid Arguments")

  #Merge layer Vector
  g1=addLayer(g1,g2$layers$Name)
  g1$layers

  #Merge Nodes
  nodesToAdd=getNodeAttributes(g2,V(g2)$name)
  nodesToAdd$n=g2$layers$Name[nodesToAdd$n]
  rows=dim(nodesToAdd)[1]
  for(i in 1:rows){
    node=nodesToAdd[i,]
    attr=as.list(node[-1][-1])
    g1=addNode(g1,nodeName = as.character(node$name),layerName = node$n,attributes = attr)
  }

  #Merge Edges
  edgesToAdd=getEdgeAttributes(g2)
  rows=dim(edgesToAdd)[1]
  for(i in 1:rows){
    edge=edgesToAdd[i,]
    attr=as.list(edge[-1][-1])
    g1=addEdge(g1,nodeStart = as.character(edge$V1),nodeDest = as.character(edge$V2),attributes = attr)
  }

  return(g1)
}

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







