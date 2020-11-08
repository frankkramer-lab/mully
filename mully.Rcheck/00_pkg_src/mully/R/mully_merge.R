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

  #name the class
  class(g) = c("mully",class(g))
  return(g1)
}
