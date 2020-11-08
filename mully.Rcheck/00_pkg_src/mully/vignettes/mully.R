## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,eval=FALSE---------------------------------------------------------
#  require(devtools)
#  install_github("frankkramer-lab/mully")

## -----------------------------------------------------------------------------
library("mully")

## -----------------------------------------------------------------------------
  g <- mully("MyFirstMully",direct = F)

## -----------------------------------------------------------------------------
  g <- addLayer(g, c("Gene", "Drug", "Drug", "Disease"))

## -----------------------------------------------------------------------------
  g=addNode(g,"d1","disease",attributes=list(type="t1"))
  print("Node d1 added as disease")
  
  g=addNode(g,"d2","disease",attributes=list(type="t1"))
  print("Node d2 added as disease")
  
  g=addNode(g,"d3","disease",attributes=list(type="t1"))
  print("Node d3 added as disease")
  
  g=addNode(g,"dr1","drug",attributes=list(effect="strong"))
  print("Node dr1 added as drug")
  
  g=addNode(g,"dr2","drug",attributes=list(effect="strong"))
  print("Node dr2 added as drug")
  
  g=addNode(g,"dr3","drug",attributes=list(effect="moderate"))
  print("Node dr3 added as drug")
  
  g=addNode(g,"g1","gene",attributes=list(desc="AF"))
  print("Node g1 added as gene")
  
  g=addNode(g,"g2","gene",attributes=list(desc="BE"))
  print("Node g2 added as gene")

  #See vertices attributes
  print(getNodeAttributes(g))
  

## -----------------------------------------------------------------------------
  g=addEdge(g,"dr1","d2",list(name="treats"))
  g=addEdge(g,"dr1","d2",list(name="extraEdge"))
  g=addEdge(g,"d2","g1",list(name="targets"))
  g=addEdge(g,"g2","dr3",list(name="mutates and causes"))
  g=addEdge(g,"dr3","d3",list(name="treats"))
  
  print(getEdgeAttributes(g))
        
  removeEdge(g,"d2","dr1",multi=T)

## -----------------------------------------------------------------------------
  #Create a Second graph
  g1=mully()

  g1=addLayer(g1,c("protein","drug","gene"))

  g1=addNode(g1,"dr4","drug",attributes=list(effect="strong"))
  g1=addNode(g1,"dr5","drug",attributes=list(effect="strong"))
  g1=addNode(g1,"dr6","drug",attributes=list(effect="moderate"))

  g1=addNode(g1,"p1","protein")
  g1=addNode(g1,"p2","protein")
  g1=addNode(g1,"p3","protein")

  g1=addNode(g1,"g3","gene")
  g1=addNode(g1,"g4","gene")


  g1=addEdge(g1,nodeStart = "p2",nodeDest = "p3",attributes = list(name="interacts"))
  g1=addEdge(g1,nodeStart = "dr6",nodeDest = "g4",attributes = list(name="targets"))

  #Merge both graphs
  g12=merge(g,g1)

  #Print the graph
  print(g12)

## ----eval=FALSE---------------------------------------------------------------
#    plot(g12,layout = "scaled")

## ----eval=FALSE---------------------------------------------------------------
#    plot3d(g12)

## -----------------------------------------------------------------------------
g=demo()
addEdge(g,"dr3","g2",attributes=list(name="newEdge"))

## -----------------------------------------------------------------------------
g=mully::demo()
addLayer(g,"Complex")

## -----------------------------------------------------------------------------
g=mully::demo()
attributes=list("specie"="Homo Sapiens")
addNode(g = g,nodeName = "g3",layerName = "Gene",attributes = attributes)

## ----eval=FALSE---------------------------------------------------------------
#  g=mully::demo()
#  exportCSV(g)

## -----------------------------------------------------------------------------
g=mully::demo()
##Print all Edges
getEdgeAttributes(g)
##Get a Single Edge
getEdgeAttributes(g,"d2","g1")

## -----------------------------------------------------------------------------
g=mully::demo()
getIDEdge(g,"d2","dr1")

## -----------------------------------------------------------------------------
g=mully::demo()
getLayer(g,"gene")

## -----------------------------------------------------------------------------
g=mully::demo()
getLayersCount(g)

## -----------------------------------------------------------------------------
g=mully::demo()
getNodeAttributes(g,layerByName = TRUE)

## -----------------------------------------------------------------------------
g=mully::demo()
isLayer(g,"Gene")
isLayer(g,"Complex")

## -----------------------------------------------------------------------------

#Create First Graph
g=mully::demo()

#Create Second Graph
g1 <- mully("MySecondMully",direct = F)

g1 <- addLayer(g1, c("gene", "Protein", "Drug"))

g1=addNode(g1,"p1","Protein",attributes=list(type="t1"))

g1=addNode(g1,"p2","Protein",attributes=list(type="t1"))

g1=addNode(g1,"p3","Protein",attributes=list(type="t1"))

g1=addNode(g1,"dr6","drug",attributes=list(effect="strong"))

g1=addNode(g1,"dr7","drug",attributes=list(effect="strong"))

g1=addNode(g1,"dr8","drug",attributes=list(effect="moderate"))

g1=addNode(g1,"g3","gene",attributes=list(desc="AF"))

g1=addNode(g1,"g9","gene",attributes=list(desc="BE"))

g1=addEdge(g1,"dr8","g9",list(name="targets"))

g1=addEdge(g1,"p3","p2",list(name="interactWith"))

#Merge Graphs
merge(g,g1)

## -----------------------------------------------------------------------------
g=mully::demo()
print(g)

## -----------------------------------------------------------------------------
g=mully::demo()
removeEdge(g,"dr1","d2",multi=TRUE)

## -----------------------------------------------------------------------------
g=mully::demo()
removeLayer(g,"gene",trans=TRUE)

## -----------------------------------------------------------------------------
g=mully::demo()
removeNode(g,"dr1",trans=TRUE)

