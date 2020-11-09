#' Import a mully graph from CSV files
#'
#' @param name The name of the graph
#' @param direct A boolean to indicate if the graph is directed or not
#' @param layers The path to the CSV file containing the layers' information
#' @param nodes The path to the CSV file containing the nodes' information
#' @param edges The path to the CSV file containing the edges' information
#'
#' @return A new mully graph
#' @export
importGraphCSV<-function(name,direct="F",layers,nodes,edges){
  if(missing(name) || name=="" ||
     missing(layers) || !file.exists(layers) ||
     missing(nodes) || !file.exists(nodes) ||
     missing(edges) || !file.exists(edges)){
    stop("Invalid arguments")
  }
  g=mully(name,direct)
  g=importLayersCSV(g,layers)
  g=importNodesCSV(g,nodes)
  g=importEdgesCSV(g,edges)

  #name the class
  class(g) = c("mully",class(g))
  return(g)
}

#' Import Layers to a mully graph from a CSV file
#'
#' @param g The mully graph to which the layers will be added. If missing, a new mully graph is created
#' @param file The path to the CSV file containing the layers' information
#'
#' @return The mully graph with the added layers
#' @export
#' @import igraph
#' @importFrom utils read.csv
#' @import igraph
importLayersCSV<-function(g,file){
  if(missing(g) || missing(file) || !file.exists(file)){
    stop("Invalid arguments")
  }
  layers=read.csv(file,header=TRUE,stringsAsFactors = FALSE)
  g=addLayer(g,layers$Name)
  return(g)
}

#' Import Nodes to a mully graph from a CSV file
#'
#' @param g The mully graph to which the nodes will be added. The graph should already have the layers.
#' @param file The path to the CSV file containing the nodes' information
#' @param name The name of the column containing the names of the nodes
#'
#' @return The mully graph with the added nodes
#' @export
#' @importFrom utils read.csv
#' @import igraph
importNodesCSV<-function(g,file,name="name"){
  if(missing(g) || missing(file) || !file.exists(file)){
    stop("Invalid arguments")
  }
  nodes=read.csv(file,header=TRUE,stringsAsFactors = FALSE)
  rows=dim(nodes)[1]
  for(i in 1:rows){
    node=nodes[i,]
    attr=node
    attr$n=NULL
    attr$name=NULL
    attr=as.list(attr)
    if(!isLayer(g,node$n)){
      next
      g=addLayer(g,node$n)
    }
    g=addNode(g,nodeName = as.character(node$name),layerName = node$n,attributes = attr)
  }
  V(g)$n=g$layers[V(g)$n]
  class(g)=c("mully",class(g))
  return(g)
}

#' Import Edges to a mully graph from a CSV file
#'
#' @param g The mully graph to which the nodes will be added. The graph should already have the layers and the nodes.
#' @param file The path to the CSV file containing the edges' information
#'
#' @return The mully graph with the added edges
#'
#'
#' @export
#' @importFrom utils read.csv
importEdgesCSV<-function(g,file){
  if(missing(g) || missing(file) || !file.exists(file)){
    stop("Invalid arguments")
  }
  edges=read.csv(file,header=TRUE,stringsAsFactors = FALSE)
  rows=dim(edges)[1]
  for(i in 1:rows){
    edge=edges[i,]
    attr=as.list(edge[-1][-1])
    g=addEdge(g,nodeStart = as.character(edge$V1),nodeDest = as.character(edge$V2),attributes = attr)
  }
  return(g)
}
