#' Export mully into CSV files
#'
#' @param g The input graph
#' @param target The target file in which the files will be generated. By default the WD.
#'
#' @return
#' @export
#'
exportCSV<-function(g,target){
  if(missing(g)){
    stop("invalid argument")
  }
  if(missing(target)){
    target=setwd()
  }
  if(!dir.exists(target)){
    stop("target directory doesn't exist")
  }
  nodes=getNodeAttributes(g)
  edges=getEdgeAttributes(g)
  layers=g$layers[,-dim(g$layers)[2]]
  now=Sys.time()
  now=format(now, "%Y%m%d-%H%M%S")
  write.csv(nodes, file = paste(target,"/","Nodes-",now,".csv",sep=""),row.names=FALSE)
  write.csv(edges, file = paste(target,"/","Edges-",now,".csv",sep=""),row.names=FALSE)
  write.csv(layers, file = paste(target,"/","Layers-",now,".csv",sep=""),row.names=FALSE)
  print(paste("Files generated successfully under",target,sep=""))
}
