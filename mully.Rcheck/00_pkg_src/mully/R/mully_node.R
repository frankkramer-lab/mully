########### Node Functions ##################



#' Get a node from a graph
#'
#' @param g The input graph.
#' @param nameNode The name of the node.
#'
#' @return The node as igraph.vs
#'
#'
#' @export
#' @import igraph
#' @examples
#' g=mully::demo()
#' getNode(g,"g1")
getNode <- function(g, nameNode) {
  if (missing(g) || !is.mully(g) || missing(nameNode)) {
    stop("Invalid Arguments")
  }
  if(!nameNode%in%V(g)$name){
    #print("Node Does not Exist")
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
#' @import igraph
#' @examples
#' g=mully::demo()
#' getIDNode(g,"g1")
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
#' @import igraph
#' @examples
#' g=mully::demo()
#' attributes=list("specie"="Homo Sapiens")
#' addNode(g = g,nodeName = "g3",layerName = "Gene",attributes = attributes)
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

  #name the class
  class(g) = c("mully",class(g))
  idNode = getIDNode(g, nodeName)
  #Assign the layer to the node
  idLayer = getIDLayer(g, layerName)
  g <- set.vertex.attribute(g, "n", index = idNode, idLayer)

  #name the class
  class(g) = c("mully",class(g))
  if (!is.null(attributes)) {
    #Assign attributes to the created edge
    for (key in names(attributes)) {
      g <- set.vertex.attribute(g, key, index = idNode, attributes[[key]])

      #name the class
      class(g) = c("mully",class(g))
    }
  }

  #name the class
  class(g) = c("mully",class(g))
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
#' @import igraph
#' @examples
#' g=mully::demo()
#' removeNode(g,"dr1",trans=TRUE)
removeNode <- function(g, name,trans=FALSE) {
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
      g <- g - vertices(node)
    }
    else{
      message(paste(c(
        "Node ", node, " Does Not Exist and will be skipped"
      )))
      next
    }
  }

  #name the class
  class(g) = c("mully",class(g))
  return(g)
}


#' Get the attributes of a node
#'
#' @param g The input graph
#' @param nameNode The name of the node
#' @param layerByName A boolean to specify whether to export the layers by name or by ID
#'
#' @return A dataframe containing the attributes of the specified node
#' @export
#' @import igraph
#' @examples
#' g=mully::demo()
#' getNodeAttributes(g,layerByName = TRUE)
getNodeAttributes<-function(g,nameNode,layerByName=FALSE){

  if(missing(g) || !is.mully(g)){
    stop("Invalid Arguments")
  }
  attributes=as.data.frame(get.vertex.attribute(g),stringsAsFactors = FALSE)
  if(layerByName==TRUE){
    attributes$n=g$layers$Name[attributes$n]
  }
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
