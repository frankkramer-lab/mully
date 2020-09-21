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
#' @param g The input graph
#' @param layout The layout. Can either be random or scaled
#'
#' @export
#'
plot.mully <- function(g, layout) {
  gps = getMarkGroups(g)

  cols = randomColor(count = g$iLayer)
  usedCols = unique(V(g)$color)
  if (is.null(V(g)$color))
    V(g)$color = NA
  for (i in 1:g$iLayer) {
    if (is.na(V(g)[which(V(g)$n == i)]$color)) {
      if (!cols[i] %in% usedCols) {
        V(g)[which(V(g)$n == i)]$color = cols[i]
        usedCols = c(usedCols, cols[i])
      }
      else{
        c = randomColor(count = 1)
        while (c %in% usedCols) {
          c = randomColor(count = 1)
        }
        usedCols = c(usedCols, c)
        V(g)[which(V(g)$n == i)]$color = c
      }
    }
  }
  plot.new()
  # filledrectangle(wx = 1, wy = 0.5, col = "gray",mid = c(0, 0), angle = 0)
  plot.igraph(g,
              vertex.color = V(g)$color,
              layout = getLayout(g, layout))
}

#Create 3d coordinates of the network layout
circpos = function(n, r = 1) {
  #Coordinates on a circle
  rad = seq(0, 2 * pi, length.out = n + 1)[-1]
  x = cos(rad) * r
  z = sin(rad) * r
  return(cbind(x, z))
}


#' Plot the graph in 3D using rgl
#'
#' @param g The input graph
#' @param layers A boolean whether to add the layers or not
#'
#' @export
#'
#' @note
#' This function can take all arguments supported and not ignored by \link[igraph]{rglplot} which are:
#' vertex.shape, vertex.label, edge.width. All others are set in the function and cannot be modified.
#'
plot3d.mully <- function(g, layers = T,
                         vertex.shape="circle",vertex.label=NA
                         ,edge.width=1) {
  rgl.open()
  rgl.bg(
    sphere = TRUE,
    color = c("white", "blue"),
    lit = FALSE,
    back = "lines"
  )
  gps = getMarkGroups(g)

  cols = randomColor(count = g$iLayer)
  usedCols = unique(V(g)$color)
  if (is.null(V(g)$color))
    V(g)$color = NA
  for (i in 1:g$iLayer) {
    if (is.na(V(g)[which(V(g)$n == i)]$color)) {
      if (!cols[i] %in% usedCols) {
        V(g)[which(V(g)$n == i)]$color = cols[i]
        usedCols = c(usedCols, cols[i])
      }
      else{
        c = randomColor(count = 1)
        while (c %in% usedCols) {
          c = randomColor(count = 1)
        }
        usedCols = c(usedCols, c)
        V(g)[which(V(g)$n == i)]$color = c
      }
    }
  }
  #Add edge colors
  edgecolors = c()
  AllEdges = getEdgeAttributes(g)
  for (i in 1:dim(AllEdges)[1]) {
    V1 = V(g)[which(V(g)$name == AllEdges[i, 1])]
    V2 = V(g)[which(V(g)$name == AllEdges[i, 2])]
    if (V1$n == V2$n)
      edgecolors = c(edgecolors, V1$color)
    else
      edgecolors = c(edgecolors, "black")
  }

  layout = get3DLayout(g)

  open3d()

  rglplot(
    g,
    vertex.color = V(g)$color,
    layout = layout,
    rescale = F,
    vertex.label=vertex.label,
    vertex.shape=vertex.shape,
    label.color = V(g)$color,
    edge.color=edgecolors,
    edge.width=edge.width,
    vertex.label.dist = 0,
    grouplist = unlist(gps)
  )
  aspect3d(1, 1, 1)
  #Add layers
  if (layers == T) {
    layout1 = layout[order(V(g)$n), ]
    clrs = unique(V(g)$color[order(V(g)$n)])
    temp = 1
    for (i in 1:g$iLayer) {
      nNodes = length(which(V(g)$n == i))
      coord = layout1[temp:(temp + length(which(V(g)$n == i)) - 1), ]
      plane = get3DPlane(coord, g$iLayer)
      rgl.planes(
        0,
        b = plane[2],
        0,
        d = plane[4],
        col = clrs[i],
        alpha = 0.3
      )
      #Add layers' names
      text3d(
        x = -max(abs(layout[, 1])),
        y = coord[1, 2],
        z = min(abs(layout[, 3])) - 1,
        texts = paste0(g$layers$Name[i]," Layer",sep=""),
        color = clrs[i],
        alpha = 1
      )
      temp = temp + nNodes
    }
  }
}

get3DLayout <- function(g) {
  yinit = 4
  layers = getMarkGroups(g)
  layout = list()
  for (i in 1:length(layers)) {
    nodesID = unlist(layers[i])
    nodesInLayerCount = length(nodesID)
    xz = circpos(nodesInLayerCount, r = length(layers))
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
getEquationPlane <- function(x1, y1, z1, x2, y2, z2, x3, y3, z3)
{
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

get3DPlane <- function(coord, iLayer) {
  p = circpos(2 + dim(coord)[1], iLayer)
  temp = cbind(p[dim(p)[1] - 1:dim(p)[1], 1], rep(coord[1, 2], 2), p[dim(p)[1] -
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
