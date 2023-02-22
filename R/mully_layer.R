########### Layer Functions ##################




#' Add a layer or a set of layers to a graph
#'
#' @param g The input graph.
#' @param nameLayer The name or the list of the names of the layers to be added. The layer names must be unique.
#'
#' @return The graph, with the layers added.
#'
#' @export
#' @import igraph
#' @examples
#' g = mully("MyFirstMully",direct = FALSE)
#' g = addLayer(g, c("Gene", "Drug", "Disease"))
addLayer <- function(g, nameLayer) {
  if (missing(g) || !is.mully(g) || missing(nameLayer)) {
    stop("Invalid Argument")
  }
  for (layer in nameLayer) {
    if (isLayer(g, layer)) {
      warning(paste(c(
        "Layer ", layer, " Already Exists and will be skipped"
      )))
      next
    }
    if (layer == "") {
      warning("Empty string and will be skipped")
      next
    }
    g$iLayer = g$iLayer + 1
    nameLayerLowerCase = casefold(layer, upper = FALSE)
    g$layers[getLayersCount(g) + 1,] <-
      c(g$iLayer, layer, nameLayerLowerCase)
  }
  return(g)
}


#' Verify if the layer exists in a graph
#'
#' @param g The input graph.
#' @param name The name of the layer.
#'
#' @return A boolean value.
#'
#' @export
#' @import igraph
#' @examples
#' g = mully("MyFirstMully",direct = FALSE)
#' g = addLayer(g, c("Gene", "Drug", "Disease"))
#' isLayer(g,"Drug")
isLayer <- function(g, name) {
  nameLayerLowerCase = casefold(name, upper = FALSE)
  if (nameLayerLowerCase %in% g$layers$NameLower) {
    return(TRUE)
  }
  return(FALSE)
}


#' Get the number of layers in a graph
#'
#' @param g The input graph.
#'
#' @return The count of the layers.
#'
#' @export
#' @import igraph
#' @examples
#' g = mully("MyFirstMully",direct = FALSE)
#' g = addLayer(g, c("Gene", "Drug", "Disease"))
#' getLayersCount(g)
getLayersCount <- function(g) {
  return(dim(g$layers)[1])
}

getIDLayer <- function(g, nameLayer) {
  if (missing(g) || !is.mully(g) ||
      missing(nameLayer) || nameLayer == "" ||
      !is.character(nameLayer)) {
    stop("Invalid Argument")
  }
  nameLayerLowerCase = casefold(nameLayer, upper = FALSE)
  if (!isLayer(g,nameLayer)) {
    return(-1)
  }
  return(as.numeric(g$layers$ID[which(g$layers$NameLower == nameLayerLowerCase)]))
}


#' Get the nodes on a layer in a graph
#'
#' @param g The input graph.
#' @param nameLayer The name of the layer.
#'
#' @return A List of the nodes on the given layer.
#'
#' @import igraph
#' @export
#' @examples
#' g = mully::demo()
#' getLayer(g,"gene")
getLayer <- function(g, nameLayer) {
  if (missing(g) || !is.mully(g) || missing(nameLayer)) {
    stop("Invalid Argument")
  }
  id = getIDLayer(g, nameLayer)
  l=which(V(g)$n == id)
  return(V(g)[l])
}

getLayerByID <- function(g, id) {
  if (missing(g) || !is.mully(g) || missing(id)) {
    stop("Invalid Argument")
  }
  l=which(V(g)$n == id)
  return(V(g)[l])
}

#' Delete a layer or a set of layers from a graph
#'
#' @param g The input graph.
#' @param name The name or the list of the names of the layers to be deleted.
#' @param trans A boolean whether to insert transitive edges or not
#'
#' @return The mully graph, with the given layer and its corresponding nodes and edges removed.
#'
#' @export
#' @import igraph
#' @examples
#' g = mully::demo()
#' removeLayer(g,"gene",trans=TRUE)
removeLayer <- function(g, name,trans=FALSE) {
  if (missing(g) ||
      missing(name) || name == "" || !is.mully(g)) {
    stop("Invalid Arguments")
  }
  for (layer in name) {
    layer=tolower(layer)
    if (!isLayer(g, layer)) {
      message(paste(c(
        "Layer ", layer, " Does Not Exist and will be skipped"
      )))
      next
    }
    nodes = getLayer(g, layer)$name
    for(j in 1:length(nodes))
      g <- removeNode(g, nodes[j],trans)
    g$layers=g$layers[-which(g$layers$NameLower==layer),]
  }
  return(g)
}


