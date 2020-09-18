getLayout<-function(g,layout){
  if(layout=="random")
    return(getRandomLayout(g))
  if(layout=="scaled")
    return(getScaledLayout(g))
  return(NULL)
}

getRandomLayout<-function(g){
  y=as.vector(V(g)$n)
  g2=g
  for(i in 1:getLayersCount(g2)){
    layer=getLayerByID(g,i)
    if(is.null(layer) || is.na(layer))
      next

    repeat{
      xs=runif(length(as.vector(layer)),-4,4)
      if(!TRUE%in%duplicated(xs)){
      V(g2)[which(V(g)$n==i)]$x=xs
      break
      }
    }
  }
  return(cbind(V(g2)$x,y))
}

getScaledLayout<-function(g){
  y=as.vector(V(g)$n)
  g2=g
  for(i in 1:getLayersCount(g2)){
    layer=getLayerByID(g,i)
    if(is.null(layer) || is.na(layer))
      next

    xs=(1:length(layer))*8/length(layer)
    V(g2)[which(V(g)$n==i)]$x=xs
  }
  return(cbind(V(g2)$x,y))
}

getMarkGroups<-function(g){
  l=list()
  for(i in 1:g$iLayer){
    li=list(which(V(g)$n==i))
    l=append(l,li)
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
plot.mully<-function(g,layout){
  gps=getMarkGroups(g)

  cols=randomColor(count=g$iLayer)
  usedCols=unique(V(g)$color)
  if(is.null(V(g)$color))
    V(g)$color=NA
  for(i in 1:g$iLayer){
    if(is.na(V(g)[which(V(g)$n==i)]$color)){
      if(!cols[i]%in%usedCols){
        V(g)[which(V(g)$n==i)]$color=cols[i]
        usedCols=c(usedCols,cols[i])
      }
      else{
        c=randomColor(count=1)
        while(c%in%usedCols){
          c=randomColor(count=1)
        }
        usedCols=c(usedCols,c)
        V(g)[which(V(g)$n==i)]$color=c
      }
    }
  }
  plot.new()
  # filledrectangle(wx = 1, wy = 0.5, col = "gray",mid = c(0, 0), angle = 0)
  plot.igraph(g,vertex.color=V(g)$color,layout=getLayout(g,layout))
}

#Create 3d coordinates of the network layout
circpos=function(n,r=1){#Coordinates on a circle
  rad=seq(0,2*pi,length.out=n+1)[-1];x=cos(rad)*r;z=sin(rad)*r
  return(cbind(x,z))
}


#' Plot the graph in 3D using rgl
#'
#' @param g The input graph
#'
#' @export
#'
plot3d.mully<-function(g){
  rgl.open()
  rgl.bg(sphere=TRUE, color=c("white","blue"), lit=FALSE, back="lines" )
  # zmin=1
  # for(i in 1:length(gps)){
  #   #shapelist3d(cube3d(),x=-5,y=-10,z=2*i+1,size=2,color="pink")
  #   #rgl.quads(x=c(4,4,-4,-4),y=c(2,-2,2,-2),z=c(zmin,zmin+2,zmin,zmin+2))
  #   rgl.spheres(x=0,y=0,z=zmin+1,r=1)
  #   zmin=zmin+6
  #   }
  gps=getMarkGroups(g)

  cols=randomColor(count=g$iLayer)
  usedCols=unique(V(g)$color)
  if(is.null(V(g)$color))
    V(g)$color=NA
  for(i in 1:g$iLayer){
    if(is.na(V(g)[which(V(g)$n==i)]$color)){
      if(!cols[i]%in%usedCols){
        V(g)[which(V(g)$n==i)]$color=cols[i]
        usedCols=c(usedCols,cols[i])
      }
      else{
        c=randomColor(count=1)
        while(c%in%usedCols){
          c=randomColor(count=1)
        }
        usedCols=c(usedCols,c)
        V(g)[which(V(g)$n==i)]$color=c
      }
    }
  }

  layout=get3DLayout(g)


  # for(i in 1:g$iLayer){
  #   gp=gps[[i]]
  #   xx=layout[gp,1]
  #   yy=layout[gp,2]
  #   zz=layout[gp,3]
  #   ellips <- ellipse3d(cov(cbind(xx,yy,zz)),
  #                       centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95)
  #   shade3d(ellips, col = usedCols[i], alpha = 0.1, lit = FALSE)
  # }

  rglplot(g,vertex.color=V(g)$color,layout=layout,vertex.size=8,vertex.label=NA,grouplist=unlist(gps))
}

get3DLayout<-function(g){
  yinit=1
  layers=getMarkGroups(g)
  layout=list()
  for(i in 1:length(layers)){
    nodesID=unlist(layers[i])
    nodesInLayerCount=length(nodesID)
    xz=circpos(nodesInLayerCount,r=i)
    x=xz[,1]
    z=xz[,2]
    y=runif(n=length(nodesInLayerCount),yinit,yinit+2)
    xyz=cbind(x,y)
    xyz=cbind(xyz,z)
    xyz=cbind(nodesID,xyz)
    layout=rbind(layout,xyz)
    yinit=yinit-6
  }
  dfLayout=as.data.frame(layout)
  dfLayout=dfLayout[order(unlist(dfLayout$nodesID)),]
  return(cbind(x=unlist(dfLayout$x),y=unlist(dfLayout$y),z=unlist(dfLayout$z)))
}


