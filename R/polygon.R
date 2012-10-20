#' Determine if point is inside a polygon.
#'
#' This function determines whether a point
#' is inside a polygon characterized by
#' its vertices.
#'
#' @param xv X coordinates of the polygon's vertices.
#' @param yv Y coordinates of the polygon's vertices.
#' @param x X coordinate of the point.
#' @param y Y coordinate of the point.
#' @return TRUE if the point is inside the polygon.
#' @export
pnpoly <- function(xv, yv, x, y)
  .Call(Rpoly_pnpoly, xv, yv, x, y)

#' Determine if point is inside a grid.
#'
#' Find out whether a point is inside a grid.
#' The grid is defined as a tecplot 2D structured zone.
#' A structured grid is determined by an a 2D array of points
#' where index I runs faster that index J. This function searches
#' each quadrilateral that makes up the grid and determines
#' in which one  the point is. If found, it returns the corresponding
#' (i,j) index. Else return NULL.
#'
#' @param z Tecplot 2D structured zone.
#' @param x X coordinate of the point.
#' @param y Y coordinate of the point.
#' @return Indices (i,j) of the grid where the point is located or NULL.
#' @export
#The grid is defined by a 2D tecplot structured mesh.
pointInZone <- function(z, x, y)
  .Call(Rpoly_find_zone, z$data[,'x'], z$data[,'y'], z$I, z$J, x, y)

#' Find where a point is in a mesh of zones.
#'
#' This function runs throuh a mesh of zones to determine
#' where a point is located using \code{\link{pointInZone}}.
#' If the point is located return the zone index and grid index.
#'
#' @param mesh List of 2D structured tecplot zones.
#' @param x X coordinate of the point.
#' @param y Y coordinate of the point.
#' @return index of zone and grid or NULL if point is not located.
#' @export
searchZones <- function(mesh, x, y){

  n <- length(mesh)
  for (i in 1:n){
    p <- pointInZone(mesh[[i]], x, y)
    if (!is.null(p))
      return(c(i, p+1))
  }
  return(NULL)
}


#' Plots a 2D tecplot structured zone.
#'
#' Debuggin utilitity that plots a 2D tecplot structured zone and
#' identifies each cell in the grid.
#'
#' @param z Tecplot zone.
#' @param add Add to an existing plot?
#' @param add.text Plot the indices of each cell?
#' @param cex Text font size multiplier.
#' @param outline Draw the outline of the zone?.
#' @export
plotZone <- function(z, add=FALSE, add.text=TRUE, cex=1, outline=FALSE){
  vx <- z$data[,'x']
  vy <- z$data[,'y']

  dim(vx) <- c(z$I, z$J)
  dim(vy) <- c(z$I, z$J)
  if(!add) plot(vx, vy, ty='n')


  if (!outline){
    for (j in 1:z$J) lines(vx[,j], vy[,j])
    for (i in 1:z$I) lines(vx[i,], vy[i,])

    if (add.text)
      for (j in 1:(z$J-1))
        for (i in 1:(z$I-1)){
          xx <- c(vx[i,j], vx[i+1,j], vx[i+1,j+1], vx[i,j+1])
          yy <- c(vy[i,j], vy[i+1,j], vy[i+1,j+1], vy[i,j+1])
          text(mean(xx), mean(yy), paste('(', i, ',', j, ')', sep=''), cex=cex)
        }
  }else{

    lines(vx[,1], vy[,1])
    lines(vx[, z$J], vy[, z$J])
    lines(vx[1,], vy[1,])
    lines(vx[z$I,], vy[z$I,])
  }
}


#' Returns zone coordinate array.
#'
#' This function returns the coordinate arrays of a zone.
#'
#' @param z Tecplot zone.
#' @return List containing the x and y coordinates the the zone array.
#' @return List of zone coordinates in array format.
zoneCoords <- function(z){

  xv <- z$data[,'x']
  yv <- z$data[,'y']

  dim(xv) <- c(z$I, z$J)
  dim(yv) <- c(z$I, z$J)

  return(list(x=xv, y=yv))
}


#' Return the quadrilateral corresponding to an index.
#'
#' Given a 2D tecplot structured zone and index of a cell,
#' This function returns the x and y coordinates of the vertices
#' of the polygon that corresponds to the cell.
#'
#' @param z Tecplot 2D structured zone.
#' @param i First index.
#' @param j Second index.
#' @return x and y vertices of the quadrilateral.
#' @export
zonePolygon <- function(z, i, j){

  p <- zoneCoords(z)
  xx <- p$x
  yy <- p$y
  pts <- cbind(x=c(xx[i,j], xx[i+1,j], xx[i+1,j+1], xx[i,j+1]),
               y=c(yy[i,j], yy[i+1,j], yy[i+1,j+1], yy[i,j+1]))

  return(pts)
  
}


#' Find the index of a quadrilateral inside a structured zone.
#'
#' A structured zone is characterized by two indexes i and j.
#' This function returns the index of the quadrilateral inside
#' a structured zone.
#'
#' @param I Maximum size of the first index.
#' @param J Maximum size of the second index.
#' @param i First index.
#' @param j Second index.
#' @return Index of the polygon corresponding to i and j.
polygonIndex <- function(I, J, i, j)
  i + J * (j-1)
  

  
  
#' Return interpolation coefficients for quadrilateral.
#'
#' Find bilinear interpolation coefficients of a given
#' quadrilateral characterized by ist vertices.
#'
#' @param x X vertices of the quadrilateral.
#' @param y Y vertices of the quadrilateral.
#' @return Shape function coeficients.
#' @export
interpolateZone <- function(x, y){

  mat <- matrix(c(1, x[1], y[1], x[1]*y[1],
                  1, x[2], y[2], x[2]*y[2],
                  1, x[3], y[3], x[3]*y[3],
                  1, x[4], y[4], x[4]*y[4]), nr=4, nc=4, byrow=TRUE)

  imat <- solve(mat)
  coefs <- imat %*% diag(4)

  return(function(x, y) matrix(c(1, x, y, x*y), nr=1, nc=4) %*% coefs)
  
  
}


#' Sets up interpolation structures for a mesh.
#'
#' Given a mesh and a sequence of points, this function
#' sets up interpolation structures so that mesh values can
#' be easily interpolated on these points.
#'
#' @param zlst List of 2D structured zones.
#' @param x Vector with x coordinates of the points.
#' @param y Vector with y coordinates of the points.
#' @return Interpolation structure for each point.
#' @export
interpPoints <- function(zlst, x, y){

  npts <- length(x)
  
  zones <- lapply(1:npts, function(i){print(i); searchZones(zlst, x[i], y[i])})

  pnull <- sapply(zones, function(x) is.null(x))
  x <- x[!pnull]
  y <- y[!pnull]
  zones <- sapply(zones[!pnull], function(x) x)
  zlst2 <- zlst[zones[1,]]
  
  nz <- dim(zones)[2]
  fun <- function(i){
    z <- zones[,i]
    zz <- zlst2[[i]]
    polygonIndex(zz$I, zz$J, c(z[2], z[2]+1, z[2]+1, z[2]),
                 c(z[3], z[3], z[3]+1, z[3]+1))
  }
  
  ip <- sapply(1:nz, fun)

  fun2 <- function(i){
    k <- ip[,i]
    phi <- interpolateZone(zlst2[[i]]$data[k,'x'], zlst2[[i]]$data[k,'y'])(x[i], y[i])
    names(phi) <- paste('phi', 1:4, sep='')
    idx <- ip[,i]
    names(idx) <- paste('i', 1:4, sep='')
    return(c(element=zones[1,i], idx, phi, i=zones[2,i], j=zones[3,i]))
  }
  
                                                                                 
  return(sapply(1:nz, fun2))
  
  
  #phi <- lapply(1:nz, function(i){
  #  pts <- zlst2[[i]]$data[ip[[i]]


   #                        p[interpolateZone(zlst2$da[[[,'x'], p[,'y']))

  return(sapply(1:nz, function(i) phi[[i]](x[i], y[i])))
  
                                             
                                                
}


#' Interpolate a mesh.
#'
#' This function uses the interpolation data
#' structure created with \code{\link{interpPoints}}
#' to interpolate the values of a mesh made up of
#' 2D tecplot structured zones.
#'
#' @param zlst Tecplot mesh to be interpolated.
#' @param phi Interpolation data structure created with \code{\link{interpPoints}}
#' @return Interpolated values (every variable in the mesh.
#' @export
applyInterp <- function(zlst, phi){

  nvars <- length(zlst[[1]]$header)
  npts <- dim(phi)[2]

  d <- matrix(NA, nr=nvars, nc=npts)

  for (i in 1:npts){
    e <- phi[1,i]
    idx <- phi[2:5,i]
    p <- matrix(phi[6:9,i], nr=1, nc=4)
    vars <- zlst[[e]]$data[idx,]
    d[,i] <- p %*% vars
  }
  rownames(d) <- zlst[[1]]$header
  
  return(d)
}


