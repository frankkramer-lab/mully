########### Constructor ##################

#' Create an empty multilayered graph
#'
#' @param name The name to be assigned to the graph.
#' @param direct A boolean value, if the graph is directed or not. By default TRUE.
#'
#' @return The created multilayered graph.
#' @export
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
#'
is.mully<-function(g){
  if(!is.igraph(g) || is.null(g$layers) || is.null(g$iLayer))
    return(F)
  return(T)
}


#' Print function
#'
#' @param g The input graph
#'
#' @export
#'
print.mully<-function(g){
  if(missing(g) || !is.mully(g)){
   stop("Invalid Arguments")
  }
  cat("mully")
  if(!is.na(g$name)){
    cat(paste(" -- ",g$name))
  }
  if(g$iLayer==0){
    cat("\nEmpty Graph")
    return()
  }
  else{
    olayers=capture.output(print.data.frame(g$layers))
    olayers <- paste(olayers, "\n", sep="")
    cat("\n",g$iLayer ,"Layers:\n")
    cat(olayers)
  }

  if(length(V(g))!=0){
    nodes=getNodeAttributes(g)
    onodes=capture.output(print.data.frame(nodes))
    onodes <- paste(onodes, "\n", sep="")
    cat("\n",dim(nodes)[1],"Nodes:\n")
    cat(onodes)
  }
  else{
    cat("\nNo Nodes\n")
  }
  if(length(E(g))!=0){
    edges=getEdgeAttributes(g)
    oedges=capture.output(print.data.frame(edges))
    oedges <- paste(oedges, "\n", sep="")
    cat("\n",dim(edges)[1],"Edges:\n")
    cat(oedges)
  }
  else{
    cat("\nNo Edges")
  }
}
