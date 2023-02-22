########### Edge Functions ##################



#' Get the ids of the edges connecting two nodes
#'
#' @param g The input graph
#' @param nodeStart The first endpoint of the edge
#' @param nodeDest The second endpoint of the edge
#'
#' @return A list containing the ids of the edges connecting the nodes
#'
#' @export
#' @import igraph
#' @examples
#' g=mully::demo()
#' getIDEdge(g,"d2","dr1")
getIDEdge <- function(g, nodeStart, nodeDest) {
  if(missing(g) || !is.mully(g) || missing(nodeStart) || missing(nodeDest)){
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
  edgeList=as.data.frame(get.edgelist(g),stringsAsFactors = FALSE)
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
#' @return A dataframe containing the edges with their attributes. If both nodes' arguments are missing, it returns all the edges with their attributes.
#' @export
#' @import igraph
#' @examples
#' g=mully::demo()
#' #Print all Edges
#' getEdgeAttributes(g)
#' #Get a Single Edge
#' getEdgeAttributes(g,"d2","g1")
getEdgeAttributes<-function(g,nodeStart,nodeDest){

  if(missing(g) || !is.mully(g)){
    stop("Invalid Arguments")
  }
  edgeList=as.data.frame(get.edgelist(g),stringsAsFactors = FALSE)
  attributes=as.data.frame(get.edge.attribute(g),stringsAsFactors = FALSE)
  edgeAttributes=cbind(edgeList,attributes)
  if(missing(nodeStart) && missing(nodeDest)){
    return(edgeAttributes)
  }
  edge=c(1:dim(edgeAttributes)[1])
  if(!missing(nodeStart) && !missing(nodeDest)){
    if(!nodeStart%in%V(g)$name || !nodeDest%in%V(g)$name){
      stop("Invalid Nodes")
    }
    edge=getIDEdge(g,nodeStart,nodeDest)
    return(edgeAttributes[edge,])
  }
  if(missing(nodeStart))
    return(edgeAttributes[(edgeAttributes$V1==nodeDest) | (edgeAttributes$V2==nodeDest),])
  return(edgeAttributes[(edgeAttributes$V1==nodeStart) | (edgeAttributes$V2==nodeStart),])
}


#' Add an edge
#'
#' @param g The input graph
#' @param nodeStart The first endpoint of the edge
#' @param nodeDest The second endpoint of the edge
#' @param attributes The attributes to assign to the edge
#'
#' @return The mully graph, with the added edge
#'
#' @export
#' @import igraph
#' @examples
#' g=mully::demo()
#' addEdge(g,"dr3","g2",attributes=list(name="newEdge"))
addEdge <- function(g, nodeStart, nodeDest, attributes) {
  #Check arguments
  if (missing(g) || missing(nodeStart) || missing(nodeDest)) {
    stop("Invalid Arguments")
  }

  if (are.connected(g, nodeStart, nodeDest)) {
    if(missing(attributes)){
      stop("Nodes are already Connected. Please provide attributes for the new Edge")
    }
    df=cbind(as.data.frame(list(V1=nodeStart,V2=nodeDest),stringsAsFactors = FALSE),as.data.frame(attributes))
    df1=cbind(as.data.frame(list(V1=nodeDest,V2=nodeStart),stringsAsFactors = FALSE),as.data.frame(attributes))
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

  #name the class
  class(g) = c("mully",class(g))
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
#' @return The mully graph with the deleted edges
#'
#' @export
#' @import igraph
#' @examples
#' g=mully::demo()
#' removeEdge(g,"dr1","d2",multi=TRUE)
removeEdge <- function(g, nodeStart, nodeDest,attributes=NA, multi=FALSE) {

  if (!are.connected(g,nodeStart,nodeDest)) {
    stop("Nodes are not connected")
  }

  idEdge = as.numeric(getIDEdge(g, nodeStart, nodeDest))

  if(length(idEdge)>1 && missing(attributes) && multi==FALSE){
    message("Nodes have multiple Edges. Please provide specific attributes.")
    message(getEdgeAttributes(g,nodeStart,nodeDest))
    stop()
  }

  if(multi==TRUE && is.na(attributes)){
    g <- g - edge(idEdge)
    #name the class
    class(g) = c("mully",class(g))
    return(g)
  }
  allEdges=getEdgeAttributes(g,nodeStart,nodeDest)
  df=as.data.frame(list(V1=nodeStart,V2=nodeDest),stringsAsFactors = FALSE)
  df1=as.data.frame(list(V1=nodeDest,V2=nodeStart),stringsAsFactors = FALSE)

  if(!is.na(attributes)){
    df=cbind(df,as.data.frame(attributes))
    df1=cbind(df1,as.data.frame(attributes))
    colnames(df)=c(c("V1","V2",names(attributes)))
    colnames(df1)=c(c("V1","V2",names(attributes)))
  }

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
    message("Nodes have multiple Edges. Please provide specific attributes.")
    message(getEdgeAttributes(g,nodeStart,nodeDest))
    stop()
  }

  g <- g - edge(as.numeric(comAll))


  #name the class
  class(g) = c("mully",class(g))
  return(g)
}


addTransEdges<-function(g,nodes){

  if(missing(g) || missing(nodes) || !is.mully(g)){
    stop("Invalid Arguments")
  }
  allEdges=as.data.frame(get.edgelist(g),stringsAsFactors = FALSE)
  inN=c()
  outN=c()
  attributes=list("type"="trans","via"="")
  for(node in nodes){
    attributes$via=node
    #Unique or Not???
    inN=unique(as.character(allEdges[which(allEdges$V2==node),]$V1))
    outN=unique(as.character(allEdges[which(allEdges$V1==node),]$V2))
    #No Transitive edges to add
    if(length(inN)==0 | length(outN)==0)
      next
    if(is.directed(g)){
    for(inNode in inN){
      for(outNode in outN){
        message(paste("The edge ",inN,"-->",outN," will be added.",sep=""))
        g<-addEdge(g,inNode,outNode,attributes)
      }
    }
    }
    else{
      all=append(inN,outN)
      for(i in 1:(length(all)-1)){
        for(j in i+1:(length(all))){
          if(j>length(all)){
            break
          }
          message(paste("The edge ",all[i],"--",all[j]," will be added.",sep=""))
          g<-addEdge(g,all[i],all[j],attributes)
        }
      }
    }
  }

  #name the class
  class(g) = c("mully",class(g))
  return(g)
}

