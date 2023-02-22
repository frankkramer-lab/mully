#' Export mully into CSV files
#'
#' @param g The input graph
#' @param target The target file in which the files will be generated.
#' @return No return value. Exports the given graph into different CSV files saved in the target folder
#'
#' @export
#' @import igraph
#' @importFrom utils write.csv
#' @examples
#' \dontrun{
#' g=mully::demo()
#' exportCSV(g,directory)
#' }
exportCSV<-function(g,target){
  if(missing(g)){
    stop("invalid argument")
  }
  if(missing(target)){
    stop("Please provide a target directory to export the files in")
  }
  if(!dir.exists(target)){
    stop("target directory doesn't exist")
  }
  if(g$iLayer==0){
    stop("This graph is empty and cannot be exported")
  }
  now=Sys.time()
  now=format(now, "%Y%m%d-%H%M%S")

  #Export the layers
  layers=g$layers[,-dim(g$layers)[2]]
  write.csv(layers, file = paste(target,"/","Layers-",now,".csv",sep=""),row.names=FALSE)

  #Export the Nodes
  if(length(V(g))!=0){
    nodes=getNodeAttributes(g,layerByName = TRUE)
    write.csv(nodes, file = paste(target,"/","Nodes-",now,".csv",sep=""),row.names=FALSE)
  }

  #Export the Edges
  if(length(E(g))!=0){
    edges=getEdgeAttributes(g)
    write.csv(edges, file = paste(target,"/","Edges-",now,".csv",sep=""),row.names=FALSE)
  }

  message(paste("Files generated successfully under",target,sep=""))
}
