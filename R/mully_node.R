########### Node Functions ##################



#' Get a node from a graph
#'
#' @param g The input graph.
#' @param name The name of the node.
#'
#' @return The node as igraph.vs
#' @export
getNode <- function(g, nameNode) {
  if (missing(g) || !is.mully(g) || missing(nameNode)) {
    stop("Invalid Arguments")
  }
  if(!nameNode%in%V(g)$name){
    print("Node Does not Exist")
    return(NULL)
  }
  return(V(g)[which(V(g)$name == nameNode)])
}


#' Get the id of a node
#'
#' @param g The input graph
#' @param nameNode The name of the node
#'
#' @return The id of the specified node
#' @export
getIDNode <- function(g, nameNode) {
  if (missing(g) ||
      missing(nameNode) || nameNode == "" ||
      !is.character(nameNode) || !is.mully(g)) {
    stop("Invalid Arguments")
  }
  if(!nameNode%in%V(g)$name){
    print("Node Does not Exist")
    return(NULL)
  }
  return(which(V(g)$name == nameNode))
}


#' Add a node with assigned layer and attributes to a graph
#'
#' @param g The input graph.
#' @param nodeName The name of the node to add.
#' @param layerName The name of the layer to be assigned to the node.
#' @param attributes The attributes of the node to add. This argument must be a named list.
#'
#' @return The graph, with the new node.
#' @export
addNode <- function(g, nodeName, layerName, attributes = NA) {
  #Check Arguments
  if (missing(g) ||
      missing(nodeName) ||
      missing(layerName) ||
      nodeName == "" || !is.character(nodeName) || !is.mully(g)) {
    stop("Invalid Arguments")
  }
  #Check if the node exists
  nodeToAdd=getNode(g,nodeName)
  if (!is.null(nodeToAdd) && nodeToAdd$n==getIDLayer(g,layerName)) {
    stop("Node already exists")
  }

  #Check if the layer exists
  if (!isLayer(g, layerName)) {
    stop("Layer does not exist")
  }
  g <- g + vertices(nodeName)
  idNode = getIDNode(g, nodeName)
  #Assign the layer to the node
  idLayer = getIDLayer(g, layerName)
  g <- set.vertex.attribute(g, "n", index = idNode, idLayer)

  if (!is.null(attributes)) {
    #Assign attributes to the created edge
    for (key in names(attributes)) {
      g <- set.vertex.attribute(g, key, index = idNode, attributes[[key]])
    }
  }

  return(g)
}


#' Delete a node or a set of nodes from a graph
#'
#' @param g The input graph.
#' @param name The name or the list of names of the nodes to be deleted.
#' @param trans A boolean whether to insert transitive edges or not
#'
#' @return The graph, with the nodes deleted.
#' @export
removeNode <- function(g, name,trans=F) {
  #Check arguments
  if (missing(g) ||
      missing(name) || !is.mully(g)) {
    stop("Invalid Arguments")
  }
  for (node in name) {
    if (node %in% V(g)$name) {
      if(isTRUE(trans)){
        g=addTransEdges(g,node)
      }
      #TODO create subgraph and save it
      g <- g - vertices(node)
    }
    else{
      message(paste(c(
        "Node ", node, " Does Not Exist and will be skipped"
      )))
      next
    }
  }
  return(g)
}


#' Get the attributes of a node
#'
#' @param g The input graph
#' @param nameNode The name of the node
#'
#' @return A framework containing the attributes of the specified node
#' @export
getNodeAttributes<-function(g,nameNode){

  if(missing(g) || !is.mully(g)){
    stop("Invalid Arguments")
  }
  attributes=as.data.frame(get.vertex.attribute(g))
  if(missing(nameNode)){
    return(attributes)
  }
  if(!nameNode%in%V(g)$name || !is.character(nameNode)){
    stop("Node Does not Exist")
  }
  return(attributes[which(attributes$name==nameNode),])

}


getIDCommonDF<-function(df,df1){
  #Get the common rows
  common=merge.data.frame(df,df1)

  dfNames=colnames(df)
  df1Names=colnames(df1)
  for(name in dfNames){
    if(!name%in%df1Names){
      df1=cbind(df1,list(name=NA))
    }
  }

  for(name in df1Names){
    if(!name%in%dfNames){
      df=cbind(df,list(name=NA))
    }
  }

  #No Common Rows
  if(dim(common)[1]==0){
    return(NULL)
  }
  rownb=dim(df)[1]
  id=c()
  for(i in 1:dim(common)[1]){
    for(j in 1:rownb){
      test=(df[j,]==common[i,])
      if(!is.na(test) && !FALSE%in%test){
        id<-append(id,rownames(df)[j])
      }
    }
  }
  return(id)
}
