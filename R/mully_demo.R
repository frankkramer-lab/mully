
demo <- function() {

  g <- mully("MyFirstMully",direct = F)

  g <- addLayer(g, c("Gene", "Drug", "Drug", "Disease"))

  g=addNode(g,"d1","disease",attributes=list(type="t1"))
  g=addNode(g,"d2","disease",attributes=list(type="t1"))
  g=addNode(g,"d3","disease",attributes=list(type="t1"))
  g=addNode(g,"dr1","drug",attributes=list(effect="strong"))
  g=addNode(g,"dr2","drug",attributes=list(effect="strong"))
  g=addNode(g,"dr3","drug",attributes=list(effect="moderate"))
  g=addNode(g,"g1","gene",attributes=list(desc="AF"))
  g=addNode(g,"g2","gene",attributes=list(desc="BE"))

  #See vertices attributes
  as.data.frame(get.vertex.attribute(g))


  g=addEdge(g,"dr1","d2",list(name="treats"))
  g=addEdge(g,"dr1","d2",list(name="extraEdge"))
  g=addEdge(g,"d2","g1",list(name="targets"))
  g=addEdge(g,"g2","dr3",list(name="mutates and causes"))
  g=addEdge(g,"dr3","d3",list(name="treats"))
  print(getEdgeAttributes(g))

  removeEdge(g,"d2","dr1",multi=T)

  return(g)

}
