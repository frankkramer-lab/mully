#' A demo function to test the package
#' @param name Optional name for the graph to be created. By default set to "MyFirstMully"
#' @return A mully test graph with few layers, nodes and edges
#' @export
demo <- function(name="MyFirstMully") {

  g <- mully(name,direct = FALSE)

  g <- addLayer(g, c("Gene", "Drug", "Disease"))

  g=addNode(g,"d1","disease",attributes=list(type="t1"))
  message("Node d1 added as disease")
  g=addNode(g,"d2","disease",attributes=list(type="t1"))
  message("Node d2 added as disease")
  g=addNode(g,"d3","disease",attributes=list(type="t1"))
  message("Node d3 added as disease")
  g=addNode(g,"dr1","drug",attributes=list(effect="strong"))
  message("Node dr1 added as drug")
  g=addNode(g,"dr2","drug",attributes=list(effect="strong"))
  message("Node dr2 added as drug")
  g=addNode(g,"dr3","drug",attributes=list(effect="moderate"))
  message("Node dr3 added as drug")
  g=addNode(g,"g1","gene",attributes=list(desc="AF"))
  message("Node g1 added as gene")
  g=addNode(g,"g2","gene",attributes=list(desc="BE"))
  message("Node g2 added as gene")

  #See vertices attributes
  as.data.frame(get.vertex.attribute(g))


  g=addEdge(g,"dr1","d2",list(name="treats"))
  g=addEdge(g,"dr1","d2",list(name="extraEdge"))
  g=addEdge(g,"d2","g1",list(name="targets"))
  g=addEdge(g,"g2","dr3",list(name="mutates and causes"))
  g=addEdge(g,"dr3","d3",list(name="treats"))
  print(getEdgeAttributes(g))

  removeEdge(g,"d2","dr1",multi=TRUE)

  return(g)

}
