getLayout <- function(g, layout) {
  if (layout == "random")
    return(getRandomLayout(g))
  if (layout == "scaled")
    return(getScaledLayout(g))
  return(NULL)
}

getRandomLayout <- function(g) {
  y = as.vector(V(g)$n)
  g2 = g
  for (i in 1:getLayersCount(g2)) {
    layer = getLayerByID(g, i)
    if (is.null(layer) || is.na(layer))
      next

    repeat {
      xs = runif(length(as.vector(layer)), -4, 4)
      if (!TRUE %in% duplicated(xs)) {
        V(g2)[which(V(g)$n == i)]$x = xs
        break
      }
    }
  }
  return(cbind(V(g2)$x, y))
}

getScaledLayout <- function(g) {
  y = as.vector(V(g)$n)
  g2 = g
  for (i in 1:getLayersCount(g2)) {
    layer = getLayerByID(g, i)
    if (is.null(layer) || is.na(layer))
      next

    xs = (1:length(layer)) * 8 / length(layer)
    V(g2)[which(V(g)$n == i)]$x = xs
  }
  return(cbind(V(g2)$x, y))
}

getMarkGroups <- function(g) {
  l = list()
  for (i in 1:g$iLayer) {
    li = list(which(V(g)$n == i))
    l = append(l, li)
  }
  return(l)
}

#' Plot the graph in 2D
#'
#' @param x The input graph
#' @param layout The layout. Can either be random or scaled
#' @param  ... Other arguments to be passed to \link[igraph]{plot.igraph}
#'
#' @export
#' @import igraph
#' @importFrom randomcoloR randomColor
#' @importFrom stats runif
#' @importFrom graphics plot.new
#' @examples
#' g=mully::demo()
#' plot(g,"Scaled")
plot.mully <- function(x, layout,...) {
  gps = getMarkGroups(x)

  cols = randomColor(count = x$iLayer)
  usedCols = unique(V(x)$color)
  if (is.null(V(x)$color))
    V(x)$color = NA
  for (i in 1:dim(x$layers)[1]) {
    nodesinlayer=getLayerByID(x,i)
    if (NA %in%nodesinlayer$color) {
      if (!cols[i] %in% usedCols) {
        nodesinlayer$color[which(is.na(nodesinlayer$color))] = cols[i]
        usedCols = c(usedCols, cols[i])
      }
      else{
        c = randomColor(count = 1)
        while (c %in% usedCols) {
          c = randomColor(count = 1)
        }
        usedCols = c(usedCols, c)
        V(x)[which(V(x)$n == i)]$color = c
      }
    }
  }
  plot.new()
  # filledrectangle(wx = 1, wy = 0.5, col = "gray",mid = c(0, 0), angle = 0)
  plot.igraph(x,
              vertex.color = V(x)$color,
              layout = getLayout(x, layout))
}

#Create 3d coordinates of the network layout on a circle
#Function copied from: https://www.blopig.com/blog/2016/10/plotting-and-storing-a-3d-network-in-r/
circpos = function(n, r = 1) {
  if(n==1){
    x=cos(runif(1,0, r)*r)
    z=sin(runif(1,0, r)*r)
    return(cbind(x,z))
  }
  #Coordinates on a circle
  rad = seq(0, 2 * pi, length.out = n + 1)[-1]
  x = cos(rad) * r
  z = sin(rad) * r
  return(cbind(x, z))
}

#Create 3d coordinates of the network layout on a circle
#Function inspired by: https://www.blopig.com/blog/2016/10/plotting-and-storing-a-3d-network-in-r/
discpos = function(n, r = 1) {
  if(n==1){
    x=cos(runif(1,0, r)*r)
    z=sin(runif(1,0, r)*r)
    return(cbind(x,z))
  }
  #Coordinates on a circle
  rad = seq(0, 2 * pi, length.out = n + 1)[-1]
  rad1 = runif(n+1,0, r)[-1]
  x = cos(rad) * rad1
  z = sin(rad) * rad1
  return(cbind(x, z))
}

#' Plot the graph in 3D using rgl
#'
#' @param g The input graph
#' @param layers A boolean whether to add the layers or not
#' @param vertex.label The vertices' labels
#' @param vertex.label.color The vertices' colors. If not specified, the colors will be chosen randomly
#' @param vertex.plac The placement form of the vertices on the layer. Can either be "circle" which will place them on a circle, or "disc" which will place them randomly on a disc. The default is "circle"
#' @param edge.color The edges' colors. If not specified, inter-edges are black, and intra-edges have the same color as the nodes on the layer
#' @param edge.width The edge width. Default set to 5.
#' @param edge.arrow.size The edges' arrow size. Default set to 10
#' @param edge.arrow.width The  edges' arrow width. Default set to 1
#'
#'
#'
#' @export
#' @import igraph
#' @import rgl
#' @importFrom randomcoloR randomColor
#' @importFrom stats runif
#' @note
#' This function can take the following arguments supported and not ignored by \link[igraph]{rglplot}:
#' vertex.label, vertex.label.color, edge.color, edge.width, edge.arrow.size,edge.arrow.width.
#' @examples
#' g=mully::demo()
#' labels=getNodeAttributes(g)$name
#' plot3d(g, layers=TRUE, vertex.label=labels,edge.width=6)
plot3d <- function(g, layers = TRUE,
                         vertex.label=NA,vertex.label.color = NA,vertex.plac="circle",
                         edge.color=NA,edge.width=5,
                         edge.arrow.size=10,edge.arrow.width=1) {
  #Check if Graph is Empty
  if(dim(g$layers)[1]==0){
    stop("This mully Graph is empty.")
  }
  #Check if Graph has no nodes
  if(length(V(g))==0){
    stop("This mully Graph has no nodes.")
  }
  rgl.open()
  rgl.bg(
    sphere = TRUE,
    color = c("white", "blue"),
    lit = FALSE,
    back = "lines"
  )
  gps = getMarkGroups(g)

  colrs = randomColor(count = g$iLayer)
  assignedColors=V(g)$color
  usedCols = unique(assignedColors)
  if (is.null(V(g)$color))
    V(g)$color = NA
  for (i in 1:dim(g$layers)[1]) {
    idLayer=as.integer(g$layers$ID[i])
    nodesid=which(V(g)$n == idLayer)
    if(is.null(nodesid) || length(nodesid)==0)
      next
    if (NA%in%V(g)[nodesid]$color) {
      if (!colrs[idLayer] %in% usedCols) {
        V(g)[nodesid]$color = colrs[idLayer]
        usedCols = c(usedCols, colrs[idLayer])
      }
      else{
        c = randomColor(count = 1)
        while (c %in% usedCols) {
          c = randomColor(count = 1)
        }
        usedCols = c(usedCols, c)
        V(g)[nodesid]$color = c
      }
    }
  }
  #Re-add assigned colors
  originalColors=assignedColors[which(!is.na(assignedColors))]
  if(length(originalColors)!=0)
    V(g)[which(!is.na(assignedColors))]$color=originalColors
  edgecolors=NULL
  if(length(E(g))!=0){
    #Add edge colors
    if (is.null(E(g)$color))
      E(g)$color = NA
    if(is.na(edge.color)){
      edgecolors = c()
      AllEdges = getEdgeAttributes(g)
      for (i in 1:dim(AllEdges)[1]) {
        #Pre-assigned color
        if(!is.na(E(g)$color[i]))
          edgecolors=c(edgecolors,E(g)$color[i])
        V1 = V(g)[which(V(g)$name == AllEdges[i, 1])]
        V2 = V(g)[which(V(g)$name == AllEdges[i, 2])]
        if (V1$n == V2$n)
          edgecolors = c(edgecolors, V1$color)
        else
          edgecolors = c(edgecolors, "black")
      }
      edge.color=edgecolors
      }
    }

  layout = get3DLayout(g,vertex.plac)

  open3d()
  igraph::rglplot(
    g,
    vertex.color = V(g)$color,
    layout = layout,
    rescale = FALSE,
    vertex.label=vertex.label,
    vertex.label.color = V(g)$color,
    vertex.label.dist = 0,
    edge.color=edgecolors,
    edge.width=edge.width,
    edge.arrow.size=edge.arrow.size,
    edge.arrow.width=edge.arrow.width,
    grouplist = unlist(gps)
  )
  rgl::aspect3d(1, 1, 1)
  #Add layers
  if (layers == TRUE) {
    layout1=as.matrix(layout)
    if(dim(layout1)[1]>1){
      layout1 = layout[order(V(g)$n), ]
    }
    clrs = unique(V(g)$color[order(V(g)$n)])
    temp = 1
    iColr=1
    for (i in 1:dim(g$layers)[1]) {
      idLayer=as.integer(g$layers$ID[i])
      nameLayer=g$layers$Name[i]
      nNodes = length(which(V(g)$n == idLayer))
      if(nNodes==0)
        next
      if(nNodes==1)
        coord = t(as.matrix(layout1[temp, ]))
      else
        coord = as.matrix(layout1[temp:(temp + nNodes - 1), ])
      plane = suppressWarnings(get3DPlane(coord, dim(g$layers)[1],nNodes))
      rgl.planes(
        0,
        b = plane[2],
        0,
        d = plane[4],
        col = clrs[iColr],
        alpha = 0.2
      )
      #Add layers' names
      text3d(
        x = -max(abs(layout[, 1]))-1,
        y = coord[1, 2],
        z = min(abs(layout[, 3])) - 2,
        texts = paste0(nameLayer," Layer",sep=""),
        color = clrs[iColr],
        alpha = 1
      )
      iColr=iColr+1
      temp = temp + nNodes
    }
  }
}

get3DLayout <- function(g,plac) {
  yinit = 4
  layers = getMarkGroups(g)
  layout = list()
  for (i in 1:length(layers)) {
    nodesID = unlist(layers[i])

    nodesInLayerCount = length(nodesID)
    #layer deleted or layer empty
    if(length(nodesID)==0 || nodesInLayerCount==0)
      next
    xz= circpos(nodesInLayerCount, r = length(layers))
    if(plac=="disc")
      xz = discpos(nodesInLayerCount, r = length(layers))
    x = xz[, 1]
    z = xz[, 2]
    y = runif(n = length(nodesInLayerCount), yinit, yinit + 2)
    xyz = cbind(x, y)
    xyz = cbind(xyz, z)
    xyz = cbind(nodesID, xyz)
    layout = rbind(layout, xyz)
    yinit = yinit - 4
  }
  dfLayout = as.data.frame(layout)
  dfLayout = dfLayout[order(unlist(dfLayout$nodesID)), ]
  return(cbind(
    x = unlist(dfLayout$x),
    y = unlist(dfLayout$y),
    z = unlist(dfLayout$z)
  ))
}
getEquationPlane <- function(x1, y1, z1, x2, y2, z2, x3, y3, z3){

  a1 = x2 - x1

  b1 = y2 - y1

  c1 = z2 - z1

  a2 = x3 - x1

  b2 = y3 - y1

  c2 = z3 - z1

  a = (b1 * c2) - (b2 * c1)

  b = (a2 * c1) - (a1 * c2)

  c = (a1 * b2) - (b1 * a2)

  d = (-(a * x1) - (b * y1) - (c * z1))

  return(c(a, b, c, d))
}

get3DPlane <- function(coord, iLayer,nNodes) {
  p = circpos(3 + nNodes, iLayer)
  temp = cbind(p[dim(p)[1] - 1:dim(p)[1], 1], rep(coord[1, 2], 3+nNodes), p[dim(p)[1] -
                                                                       1:dim(p)[1], 2])
  coord = rbind(coord, temp)
  plane = getEquationPlane(coord[1, 1],
                           coord[1, 2],
                           coord[1, 3],
                           coord[2, 1],
                           coord[2, 2],
                           coord[2, 3],
                           coord[3, 1],
                           coord[3, 2],
                           coord[3, 3])
  return(plane)
}
