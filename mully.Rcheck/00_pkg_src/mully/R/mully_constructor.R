########### Constructor ##################

#' Create an empty multilayered graph
#'
#' @param name The name to be assigned to the graph.
#' @param direct A boolean value, if the graph is directed or not. By default TRUE.
#'
#' @return The created multilayered graph.
#'
#' @export
#' @import igraph
mully <- function(name = NA, direct = TRUE) {
  #Create the layers Indexation Table
  #Vector for the layers indexation
  #One row is not enough (in case we delete layers)
  layersVect = data.frame(ID = is.numeric(c()),
                          Name = is.character(c()),
                          NameLower = is.character(c()),
                          stringsAsFactors = FALSE)
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
  class(g) = c("mully",class(g))
  return(g)
}

#' Is this a mully graph?
#'
#' @param g The input graph
#'
#' @return A boolean whether the graph is or not a mully object
#' @export
#' @import igraph
is.mully<-function(g){
  if(!is.igraph(g) || is.null(g$layers) || is.null(g$iLayer))
    return(F)
  return(T)
}


#' Print function
#'
#' @param x The input graph
#' @param ... Other arguments to be passed to \code{print}
#' @export
#' @import igraph
#' @importFrom utils capture.output
print.mully<-function(x,...){
  if(missing(x) || !is.mully(x)){
   stop("Invalid Arguments")
  }
  cat("mully")
  if(!is.na(x$name)){
    cat(paste(" -- ",x$name))
  }
  if(x$iLayer==0){
    cat("\nEmpty Graph")
    return()
  }
  else{
    olayers=capture.output(print.data.frame(x$layers))
    olayers <- paste(olayers, "\n", sep="")
    cat("\n",x$iLayer ,"Layers:\n")
    cat(olayers)
  }

  if(length(V(x))!=0){
    nodes=getNodeAttributes(x)
    onodes=capture.output(print.data.frame(nodes))
    onodes <- paste(onodes, "\n", sep="")
    cat("\n",dim(nodes)[1],"Nodes:\n")
    cat(onodes)
  }
  else{
    cat("\nNo Nodes\n")
  }
  if(length(E(x))!=0){
    edges=getEdgeAttributes(x)
    oedges=capture.output(print.data.frame(edges))
    oedges <- paste(oedges, "\n", sep="")
    cat("\n",dim(edges)[1],"Edges:\n")
    cat(oedges)
  }
  else{
    cat("\nNo Edges")
  }
}
