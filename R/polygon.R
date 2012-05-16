

pnpoly <- function(xv, yv, x, y)
  .Call('Rpoly_pnpoly', xv, yv, x, y)

pointInZone <- function(z, x, y)

  .Call('Rpoly_find_zone', z$data[,'x'], z$data[,'y'], z$I, z$J, x, y)
searchZones <- function(mesh, x, y){

  n <- length(mesh)
  for (i in 1:n){
    p <- pointInZone(mesh[[i]], x, y)
    if (!is.null(p))
      return(c(i, p+1))
  }
  return(NULL)
}


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

zoneCoords <- function(z){

  xv <- z$data[,'x']
  yv <- z$data[,'y']

  dim(xv) <- c(z$I, z$J)
  dim(yv) <- c(z$I, z$J)

  return(list(x=xv, y=yv))
}


  
zonePolygon <- function(z, i, j){

  p <- zoneCoords(z)
  xx <- p$x
  yy <- p$y
  pts <- cbind(x=c(xx[i,j], xx[i+1,j], xx[i+1,j+1], xx[i,j+1]),
               y=c(yy[i,j], yy[i+1,j], yy[i+1,j+1], yy[i,j+1]))

  return(pts)
  
}


polygonIndex <- function(I, J, i, j)
  i + J * (j-1)
  

  
  


interpolateZone <- function(x, y){

  mat <- matrix(c(1, x[1], y[1], x[1]*y[1],
                  1, x[2], y[2], x[2]*y[2],
                  1, x[3], y[3], x[3]*y[3],
                  1, x[4], y[4], x[4]*y[4]), nr=4, nc=4, byrow=TRUE)

  imat <- solve(mat)
  coefs <- imat %*% diag(4)

  return(function(x, y) matrix(c(1, x, y, x*y), nr=1, nc=4) %*% coefs)
  
  
}


  
interpPoints <- function(zlst, x, y){

  npts <- length(x)
  
  zones <- lapply(1:npts, function(i) searchZones(zlst, x[i], y[i]))

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


