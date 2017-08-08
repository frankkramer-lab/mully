# mully
![alt text](https://github.com/frankkramer-lab/mully/blob/master/R/img/mully.png "mully")
## Introduction
Network theory has been used for many years in the modeling and analysis of complex systems, as epidemiology, biology and biomedicine . As the data evolves and becomes more heterogeneous and complex, monoplex networks become an oversimplification of the corresponding systems. This imposes a need to go beyond traditional networks into a richer framework capable of hosting objects and relations of different scales, called Multilayered Network
**Mully**, **mul**ti**l**a**y**er networks, is an R package that provides a multilayer network framework.
Using this package, the user can create, modify and visualize graphs with multiple layers. This package is an extension to the [igraph package](https://github.com/igraph/rigraph) that provides a monolayer graph framework.
The package is implemented as a part of [the Multipath Project](http://www.ams.med.uni-goettingen.de/p-mgmt/Multih.html)  directed by [Dr. Frank Kramer]([Dr. Frank Kramer](http://www.ams.med.uni-goettingen.de/kramer.shtml) ).
## Installation
### Installation via Github

```R
require(devtools)
install_github("frankkramer-lab/mully")
library(mully)
```
## Test the package
In this section, we provide a demo to test the package by calling some of the function. After running this script, you will have a graph g with 3 layers and 8 nodes. the graph can also be modified by calling other functions. Please refer to help to see the available functions.

```R
  g <- mully("MyFirstMully",direct = F)

  g <- addLayer(g, c("Gene", "Drug", "Drug", "Disease"))

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
  as.data.frame(get.vertex.attribute(g))


  g=addEdge(g,"dr1","d2",list(name="treats"))
  g=addEdge(g,"dr1","d2",list(name="extraEdge"))
  g=addEdge(g,"d2","g1",list(name="targets"))
  g=addEdge(g,"g2","dr3",list(name="mutates and causes"))
  g=addEdge(g,"dr3","d3",list(name="treats"))
  
  print(getEdgeAttributes(g))

  removeEdge(g,"d2","dr1",multi=T)
```
### Available Functions
mully functions are divided into four categories: [Constructor](https://github.com/frankkramer-lab/mully/blob/master/R/mully.R) , [Layers Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_layer.R) , [Node Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_node.R) , and [Edge Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_edge.R) .

| Function |Description|
| --------------- |-----------|
|`mully(name,direct)`|Constructor Function, Create an empty multilayered graph|
|`addLayer(g, nameLayer)`| Add a layer or a set of layers to a graph|
|`removeLayer(g, name,trans)`|Delete a layer or a set of layers from a graph|
|`isLayer(g, name)`|Verify if the layer exists in a graph|
|`getLayersCount(g)`|Get the number of layers in a graph|
|`getLayer(g, nameLayer)`|Get the nodes on a layer in a graph|
|`getNode(g,nameNode)`|Get a node from a graph|
|`getIDNode(g,nameNode)`|Get the id of a node|
|`addNode(g, nodeName, layerName, attributes)`|Add a node with assigned layer and attributes to a graph|
|`removeNode(g, name,trans)`|Delete a node or a set of nodes from a graph|
|`getNodeAttributes(g,nameNode)`|Get the attributes of a node|
|`addEdge(g, nodeStart, nodeDest, attributes)`|Add an edge|
|`removeEdge(g, nodeStart, nodeDest,attributes, multi)`|Delete an edge|
|`getEdgeAttributes(g,nodeStart,nodeDest)`|Get the attributes of the edges connecting two nodes|
|`getIDEdge(g,nodeStart,nodeDest)`|Get the ids of the edges connecting two nodes|
