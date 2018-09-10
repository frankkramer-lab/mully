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
#'
importGraphCSV<-function(name,direct="F",layers,nodes,edges){
  if(missing(name) || name=="" ||
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

#' Import Layers to a mully graph from a CSV file
#'
#' @param g The mully graph to which the layers will be added. If missing, a new mully graph is created
#' @param file The path to the CSV file containing the layers' information
#'
#' @return The mully graph with the added layers
#' @export
#'
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
#'
#' @return The mully graph with the added nodes
#' @export
#'
importNodesCSV<-function(g,file){
  if(missing(g) || missing(file) || !file.exists(file)){
    stop("Invalid arguments")
  }
  nodes=read.csv(file,header=TRUE,stringsAsFactors = FALSE)
  nodes$n=g$layers$Name[nodes$n]
  rows=dim(nodes)[1]
  for(i in 1:rows){
    node=nodes[i,]
    attr=as.list(node[-1][-1])
    g=addNode(g,nodeName = as.character(node$name),layerName = node$n,attributes = attr)
  }
  return(g)
}

#' Title
#'
#' @param g The mully graph to which the nodes will be added. The graph should already have the layers and the nodes.
#' @param file The path to the CSV file containing the edges' information
#'
#' @return The mully graph with the added edges
#' @export
#'
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
