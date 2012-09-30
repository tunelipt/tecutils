#' Calculate distance from points.
#'
#' To be able to draw cylinders easily, this
#' function calculates the distance from
#' a set of points.
#'
#' @param tec Tecplot element.
#' @param xc Vector with x center of the cylinders.
#' @param yc Vector with y center of the cylinders should have the same length as xc.
#' @return Element with new fields: distance from each cylinder center.
#' @export
addRadius <- function(tec, xc=0, yc=0){

  if (is.null(xc)) return(tec)

  np <- length(xc)
  nn <- dim(tec$data)[1]
  
  x <- tec$data[,'x']
  y <- tec$data[,'y']

  rr <- matrix(0.0, nr=nn, nc=np)
  for (i in 1:np)
    rr[,i] <- sqrt( (x-xc[i])**2 + (y-yc[i])**2 )
  if (np==1)
    colnames(rr) <- 'r'
  else
    colnames(rr) <- paste('r', 1:np, sep='')
  tec$data <- cbind(tec$data, rr)
  tec$header <- c(tec$header, colnames(rr))
  return(tec)
}

#' Extends a tecplot element from 2D to  3D.
#'
#' Given a 2D field, this function extends the field
#' from 2D to 3D.
#'
#' @param tec Tecplot element.
#' @param nz Number of planes along z direction.
#' @param LZ Length of the domain in z direction.
#' @return 3D tecplot element.
#' @export
tecplot2dTo3d <- function(tec, nz=5, LZ=1){

  I <- tec$I
  J <- tec$J

  K <- nz
  nxy <- I*J

  zz <- seq(0, LZ, len=nz)
  z <- rep(zz, each=nxy)

  idx <- rep(1:nxy,nz)

  d <- tec$data[idx,]
  tec$data <- cbind(d, z=z)
  tec$header <- c(tec$header, 'z')
  tec$K <- nz

  return(tec)

  
}

#' Repeats a 3D field along z direction.
#'
#' When a 3D field is periodic in the z direction,
#' sometimes, for visualization purposes it is interesting
#' to work with a longer field. This function repeats
#' the original field to extend it accordingly.
#'
#' @param tec Tecplot 3D periodic element.
#' @param n Number of times the field should be repeated.
#' @return Extended 3D field.
#' @export
repeatTecZ <- function(tec, n=2){

  I <- tec$I
  J <- tec$J
  K <- tec$K
  if (K==1) return(tec)
  nn <- dim(tec$data)[1]
  
  nxy <- I*J
  
  nz <- K-1
  z1 <- tec$data[,'z'][1:(nz*nxy)]
  dlast <- tec$data[(nxy*nz+1):nn,]
  zlast <- tec$data[,'z'][(nxy*nz+1):nn]

  zmin <- min(z1)
  zmax <- max(zlast)

  dz <- zmax - zmin

  
  d1 <- tec$data[1:(nz*nxy),]
  d <- d1
  if (n > 1)
    for (i in 2:n)
      d <- rbind(d, d1)
  d <- rbind(d, dlast)
  
  z <- z1

  if (n > 1)
    for (i in 2:n)
      z <- c(z, z1+(i-1)*dz)
  z <- c(z, zlast + (n-1)*dz)

  d[,'z'] <- z

  tec$data <- d
  tec$K <- nz + 1

  return(tec)
}


