########### Edge Functions ##################



#' Get the ids of the edges connecting two nodes
#'
#' @param g The input graph
#' @param nodeStart The first endpoint of the edge
#' @param nodeDest The second endpoint of the edge
#'
#' @return A list containing the ids of the edges connecting the nodes
#' @export
#'
getIDEdge <- function(g, nodeStart, nodeDest) {
  if(missing(g) || missing(nodeStart) || missing(nodeDest) || !is.igraph(g)){
    stop("Invalid Arguments")
  }
  v1 = getNode(g, nodeStart)
  v2 = getNode(g, nodeDest)
  if(is.null(v1) || is.null(v2)){
    stop("Invalid Nodes")
  }
  if(!are.connected(g,v1,v2)){
    return(0)
  }
  edgeList=as.data.frame(get.edgelist(g))
  if(is.directed(g)){
    e=which(edgeList$V1==nodeStart & edgeList$V2==nodeDest)
  }
  else{
    e=which((edgeList$V1==nodeStart & edgeList$V2==nodeDest) | (edgeList$V2==nodeStart & edgeList$V1==nodeDest))
  }
  return(e)
}

#' Get the attributes of the edges connecting two nodes
#'
#' @param g The input graph
#' @param nodeStart The first endpoint of the edge
#' @param nodeDest The second endpoint of the edge
#'
#' @return A framework containing the edges with their attributes
#' @export
#'
getEdgeAttributes<-function(g,nodeStart,nodeDest){

  if(missing(g) || !is.igraph(g)){
    stop("Invalid Arguments")
  }
  edgeList=as.data.frame(get.edgelist(g))
  attributes=as.data.frame(get.edge.attribute(g))
  edgeAttributes=cbind(edgeList,attributes)
  edge=c(1:dim(edgeAttributes)[1])
  if(!missing(nodeStart) && !missing(nodeDest)){
    if(!nodeStart%in%V(g)$name || !nodeDest%in%V(g)$name){
      stop("Invalid Nodes")
    }
    edge=getIDEdge(g,nodeStart,nodeDest)
  }
  return(edgeAttributes[edge,])
}



#' Add an edge
#'
#' @param g The input graph
#' @param nodeStart The first endpoint of the edge
#' @param nodeDest The second endpoint of the edge
#' @param attributes The attributes to assign to the edge
#'
#' @return The graph, with the added edge
#' @export
addEdge <- function(g, nodeStart, nodeDest, attributes) {
  #Check arguments
  if (missing(g) || missing(nodeStart) || missing(nodeDest)) {
    stop("Invalid Arguments")
  }

  if (are.connected(g, nodeStart, nodeDest)) {
    if(missing(attributes)){
      stop("Nodes are already Connected. Please provide attributes for the new Edge")
    }
    df=cbind(as.data.frame(list(V1=nodeStart,V2=nodeDest)),as.data.frame(attributes))
    df1=cbind(as.data.frame(list(V1=nodeDest,V2=nodeStart)),as.data.frame(attributes))
    allEdges=getEdgeAttributes(g,nodeStart,nodeDest)
    if(is.directed(g) && !is.null(getIDCommonDF(allEdges,df))){
      stop("Edge Already Exists ")
    }
    if(!is.directed(g) && (!is.null(getIDCommonDF(allEdges,df)) || !is.null(getIDCommonDF(allEdges,df1)))){
      stop("Edge Already Exists ")
    }
  }
  g <- g + edge(nodeStart, nodeDest)
  edges=getIDEdge(g, nodeStart, nodeDest)
  idEdge = edges[length(edges)]

  #Assign attributes to the created edge

  for (key in names(attributes)) {
    g <- set.edge.attribute(g, key, index = idEdge, attributes[[key]])
  }
  return(g)
}



#' Delete an edge
#'
#' @param g The input graph
#' @param nodeStart The first endpoint of the edge
#' @param nodeDest The second endpoint of the edge
#' @param attributes The attributes of the edge to delete. Required if the nodes are multi-connected
#' @param multi A boolean. Specifies whether to delete multiple edges or not, in case they exist.
#'
#' @return The graph with the deleted edges
#' @export
#'
removeEdge <- function(g, nodeStart, nodeDest,attributes=NA, multi=FALSE) {

  if (!are.connected(g,nodeStart,nodeDest)) {
    stop("Nodes are not connected")
  }

  idEdge = as.numeric(getIDEdge(g, nodeStart, nodeDest))

  if(length(idEdge)>1 && missing(attributes) && multi==FALSE){
    print("Nodes have multiple Edges. Please provide specific attributes.")
    print(getEdgeAttributes(g,nodeStart,nodeDest))
    stop()
  }

  df=cbind(as.data.frame(list(V1=nodeStart,V2=nodeDest)),as.data.frame(attributes))
  df1=cbind(as.data.frame(list(V1=nodeDest,V2=nodeStart)),as.data.frame(attributes))
  allEdges=getEdgeAttributes(g,nodeStart,nodeDest)

  c1=getIDCommonDF(allEdges,df)
  c2=getIDCommonDF(allEdges,df1)

  comAll=c()

  if(is.directed(g)){
    comAll=c1
  }
  if(!is.directed(g)){
    comAll=c(c1,c2)
  }

  if(is.null(comAll)){
    stop("Edge Does not Exist ")
  }

  if(length(comAll)>1 && multi==FALSE){
    print("Nodes have multiple Edges. Please provide specific attributes.")
    print(getEdgeAttributes(g,nodeStart,nodeDest))
    stop()
  }

  #TODO create subgraph and save it
  g <- g - edge(as.numeric(comAll))
  return(g)
}


addTransEdges<-function(g,nodes){

  if(missing(g) || missing(nodes) || !is.igraph(g)){
    stop("Invalid Arguments")
  }
  allEdges=as.data.frame(get.edgelist(g))
  inN=c()
  outN=c()

  attributes=list(type="trans",via="")

  for(node in nodes){
    attributes$via=node
    #Unique or Not???
    inN=unique(as.character(allEdges[which(allEdges$V2==node),]$V1))
    outN=unique(as.character(allEdges[which(allEdges$V1==node),]$V2))

    if(is.directed(g)){
    for(inNode in inN){
      for(outNode in outN){
        print(inNode)
        print(outNode)
        g<-addEdge(g,inNode,outNode,attributes)
      }
    }
    }
    else{
      all=append(inN,outN)
      print(all)
      for(i in 1:(length(all)-1)){
        for(j in i+1:(length(all))){
          if(j>length(all)){
            break
          }
          g<-addEdge(g,all[i],all[j],attributes)
        }
      }
    }
  }
  return(g)
}

