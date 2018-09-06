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
                          NameLower = is.character(c()))
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
  class(g) = c("mully", class(g))
  return(g)
}

is.mully<-function(g){
  if(!is.igraph(g) || is.null(g$layers) || is.null(g$iLayer))
    return(F)
  return(T)
}


# #TODO overwrite igraph.print
# mully.print<-function(g){
#
# }
