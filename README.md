# mully
![alt text](https://github.com/frankkramer-lab/mully/blob/master/R/img/mully.png "mully")
## Introduction
Network theory has been used for many years in the modeling and analysis of complex systems, as epidemiology, biology and biomedicine . As the data evolves and becomes more heterogeneous and complex, monoplex networks become an oversimplification of the corresponding systems. This imposes a need to go beyond traditional networks into a richer framework capable of hosting objects and relations of different scales, called Multilayered Network
**Mully**, **mul**ti**l**a**y**er networks, is an R package that provides a multilayer network framework.
Using this package, the user can create, modify and visualize graphs with multiple layers. This package is an extension to the [igraph package](https://github.com/igraph/rigraph) that provides a monolayer graph framework.
The package is implemented as a part of [the Multipath Project](http://www.ams.med.uni-goettingen.de/p-mgmt/Multih.html)  directed by [Dr. Frank Kramer](http://www.ams.med.uni-goettingen.de/kramer.shtml) .
## Installation
### Installation via Github

```R
require(devtools)
install_github("frankkramer-lab/mully")
library(mully)
```
## Test the package
In this section, we provide a demo to test the package by calling some of the function. After running this script, you will have a graph g with 3 layers and 8 nodes. the graph can also be modified by calling other functions. Please refer to help to see the available functions.
### Create new mully graph
```R
  g <- mully("MyFirstMully",direct = F)
```
### Add Layers
```R
  g <- addLayer(g, c("Gene", "Drug", "Drug", "Disease"))
```
### Add/print Nodes
```R
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
  
  #The Result:
  # name n type   effect desc
  #   1   d1 3   t1     <NA> <NA>
  #   2   d2 3   t1     <NA> <NA>
  #   3   d3 3   t1     <NA> <NA>
  #   4  dr1 2 <NA>   strong <NA>
  #   5  dr2 2 <NA>   strong <NA>
  #   6  dr3 2 <NA> moderate <NA>
  #   7   g1 1 <NA>     <NA>   AF
  #   8   g2 1 <NA>     <NA>   BE

```
### Add/print/remove Edges
```R
  g=addEdge(g,"dr1","d2",list(name="treats"))
  g=addEdge(g,"dr1","d2",list(name="extraEdge"))
  g=addEdge(g,"d2","g1",list(name="targets"))
  g=addEdge(g,"g2","dr3",list(name="mutates and causes"))
  g=addEdge(g,"dr3","d3",list(name="treats"))
  
  print(getEdgeAttributes(g)
  
  #The Result:
  #      V1  V2               name
  #   1  d2 dr1             treats
  #   2  d2 dr1          extraEdge
  #   3  d2  g1            targets
  #   4 dr3  g2 mutates and causes
  #   5  d3 dr3             treats
  
  removeEdge(g,"d2","dr1",multi=T)
  
```
### Merge two graphs
```R
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
  print.mully(g12)
  
  # Printing this graph gives this result:
  #   mully --  MyFirstMully
  # 4 Layers:
  #     ID    Name NameLower
  #   1  1    Gene      gene
  #   2  2    Drug      drug
  #   3  3 Disease   disease
  #   4  4 protein   protein
  # 
  # 16 Nodes:
  #     name n type   effect desc
  #   1    d1 3   t1     <NA> <NA>
  #   2    d2 3   t1     <NA> <NA>
  #   3    d3 3   t1     <NA> <NA>
  #   4   dr1 2 <NA>   strong <NA>
  #   5   dr2 2 <NA>   strong <NA>
  #   6   dr3 2 <NA> moderate <NA>
  #   7    g1 1 <NA>     <NA>   AF
  #   8    g2 1 <NA>     <NA>   BE
  #   9   dr4 2 <NA>   strong <NA>
  #   10  dr5 2 <NA>   strong <NA>
  #   11  dr6 2 <NA> moderate <NA>
  #   12   p1 4 <NA>     <NA> <NA>
  #   13   p2 4 <NA>     <NA> <NA>
  #   14   p3 4 <NA>     <NA> <NA>
  #   15   g3 1 <NA>     <NA> <NA>
  #   16   g4 1 <NA>     <NA> <NA>
  #   
  # 7 Edges:
  #      V1  V2               name
  #   1  d2 dr1             treats
  #   2  d2 dr1          extraEdge
  #   3  d2  g1            targets
  #   4 dr3  g2 mutates and causes
  #   5  d3 dr3             treats
  #   6  p2  p3          interacts
  #   7 dr6  g4            targets

```
### Visualization
```R
  plot.mully(g12,layout = "scaled")
```
  <span style="display:block;text-align:center">![alt text](https://github.com/frankkramer-lab/mully/blob/master/R/img/2DVisualizer_Scaled.png "2D Visualization Scaled")</span>

  ```R
  plot.mully.3d(g12)
```

  <span style="display:block;text-align:center">![alt text](https://github.com/frankkramer-lab/mully/blob/master/R/img/3DVisualizer.png "3D Visualization")</span>

## Available Functions
mully functions are divided into different files depending on their functionnality range:
[Constructor](https://github.com/frankkramer-lab/mully/blob/master/R/mully_constructor.R) ,
[Layers Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_layer.R) ,
[Node Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_node.R) ,
[Edge Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_edge.R) ,
[Merge Function](https://github.com/frankkramer-lab/mully/blob/master/R/mully_merge.R) ,
[Visualization Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_visualization.R) ,
[Import Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_import.R) ,
[Export Functions](https://github.com/frankkramer-lab/mully/blob/master/R/mully_export.R) ,
[Demo](https://github.com/frankkramer-lab/mully/blob/master/R/mully_demo.R).


| Function |Description|
| --------------- |-----------|
|`mully(name,direct)`|Constructor Function, Create an empty multilayered graph|
|`print.mully(g)`|Print function|
|`addLayer(g, nameLayer)`| Add a layer or a set of layers to a graph|
|`removeLayer(g, name,trans)`|Delete a layer or a set of layers from a graph|
|`isLayer(g, name)`|Verify if the layer exists in a graph|
|`getLayersCount(g)`|Get the number of layers in a graph|
|`getLayer(g, nameLayer)`|Get the nodes on a layer in a graph|
|`getNode(g,nameNode)`|Get a node from a graph|
|`getIDNode(g,nameNode)`|Get the id of a node|
|`addNode(g, nodeName, layerName, attributes)`|Add a node with assigned layer and attributes to a graph|
|`removeNode(g, name,trans)`|Delete a node or a set of nodes from a graph|
|`getNodeAttributes(g,nameNode)`|Get the attributes of one or all nodes|
|`addEdge(g, nodeStart, nodeDest, attributes)`|Add an edge|
|`removeEdge(g, nodeStart, nodeDest,attributes, multi)`|Delete an edge|
|`getEdgeAttributes(g,nodeStart,nodeDest)`|Get the attributes of the edges connecting two nodes or all the edges in the graph|
|`getIDEdge(g,nodeStart,nodeDest)`|Get the ids of the edges connecting two nodes|
|`merge(g1,g2)`|Merge or unite two graphs|
|`plot.mully(g,layout)`|Plot the graph in 2D|
|`plot.mully.3d(g)`|Plot the graph in 3D using rgl|
|`importGraphCSV(name,direct,layers,nodes,edges)`|Import a mully graph from csv files|
|`importLayersCSV(g,file)`|Import layers to a mully graph from a CSV file|
|`importNodesCSV(g,file)`|Import nodes to a mully graph from a CSV file|
|`importEdgesCSV(g,file)`|Import edges to a mully graph from a CSV file|

