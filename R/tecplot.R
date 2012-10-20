
#' Parses tecplot header.
#' 
#' This function parses a tecplot file header (the format given by nektar)
#' It returns a list containing the labels of the variables, the number of
#' variables, the position of var x, y and z. If z is not present, it is a
#' 2d file, else a 3d file
#'
#' @param header Tecplot header characterizing the variables.
#' @return Character vector with variable names.
parseHeader <- function(header){
  # This function parses the main header

  header <- strsplit(header, "=")[[1]][2]

  labs <- strsplit(header, "[ ,\t:;]+")[[1]]

  labs <- labs[labs != ""]
  labs <- strwrap(gsub('"', '', labs))
  
  # Find whether x, y, z is present

  return(labs)
  
}


#' Parse a zone header.
#'
#' This function parses a zone header. It returns a list containing the
#' the zone name, and the values of parameters I,J,K,F and the number of
#' data lines.
#'
#' @param zheader Character string with zone header.
#' @param header Character vector with variable names.
#' @return Function that reads the zone data.
parseZHeader <- function(zheader, header){
  elem <- strsplit(zheader, ",")[[1]]
  n <- length(elem)
  getField <- function(sec){
    pair <- strsplit(sec, '=')[[1]]
    pair[1] <- strwrap(pair[1])
    return(pair)
  }
  params <- list()
  for (i in 2:n){
    pair <- getField(elem[i])
    params[[pair[1]]] <- pair[2]
  }

  I <- NULL
  J <- 1
  K <- 1
  F <- NULL
  fields <- names(params)
  for (f in fields){
    p <- params[[f]]
    if (f=='I')
      I <- as.integer(p)
    else if (f=='J')
      J <- as.integer(p)
    else if (f=='K')
      K <- as.integer(p)
    else if (f == 'F')
      F <- strwrap(p)
  }
  if (is.null(I))
    stop('At least parameter I should be specified!')
  
  if (!is.null(F) && F != 'POINT')
    stop('I can only read zones with F=POINT')

  n <- I*J*K

  
  readZone <- function(con){
    nhdr <- length(header)
    what <- list()
    for (i in 1:nhdr)
      what[[i]] <- double(0)
    
    d <- scan(file=con, what=what, nmax=n)
    
    data <- matrix(0, n, nhdr)
    for (i in 1:nhdr)
      data[,i] <- d[[i]]
    colnames(data) <- header
    

    return(list(data=data, I=I, J=J, K=K, header=header))
  }

  return(readZone)
}



#' Reads a tecplot file with a single zone.
#'
#' Reads a tecplot strutured grid file with a single zone.
#'
#' @param fname File name
#' @return  Tecplot plot zone data.
#' @export
readTecplot1 <- function(fname){

  f <- file(fname, open='r')
  
  repeat{
    line <- readLines(f,n=1)
    m <- grep("^[ ]*VARIABLES[ ]*=", line)
    if (length(m) != 0) break
  }

  vars <- parseHeader(line)

  repeat{
    line <- readLines(f, n=1)
    m <- grep("^[ ]*ZONE", line)
    if (length(m) != 0) break
  }
  
  readZone <- parseZHeader(line, vars)

  zone <- readZone(f)
  close(f)

  return(zone)
}

#' Reads a tecplot structured grid file .
#'
#' Reads a tecplot structured grid file with one or more zones.
#'
#' @param fname Tecplot file name.
#' @return List of zone data.
#' @export
readTecplot <- function(fname){

  f <- file(fname, open='r')
  on.exit(close(f))

  repeat{
    line <- readLines(f,n=1)
    m <- grep("^[ ]*VARIABLES[ ]*=", line)
    if (length(m) != 0) break
  }

  vars <- parseHeader(line)
   zlst <- list()
  zc <- 0
  while(TRUE){
    repeat{
      line <- readLines(f, n=1)
      if (length(line)==0) return(zlst)
      m <- grep("^[ ]*ZONE", line)
      if (length(m) != 0) break
    }
    zc <- zc + 1
    readZone <- parseZHeader(line, vars)
    
    zlst[[zc]] <- readZone(f)
  }
  return(zlst)
}

#' Returns a list of variables from a tecplot zone.
#'
#' This function returns a list of arrays of each variable
#' in a tecplot zone.
#'
#' @param d tecplot structured zone.
#' @return List of variable arrays.
#' @export
transfTec <- function(d){

  data <- list()
  h <- d$header
  I <- d$I
  J <- d$J
  K <- d$K
  nvars <- length(h)
  for (i in 1:nvars){
    v <- h[i]
    data[[v]] <- d$data[,i]
    if (K > 1)
      ndim <- c(I, J, K)
    else if (J > 1)
      ndim <- c(I, J)
    else
      ndim <- I

    dim(data[[v]]) <- ndim

  }

  return(data)
}



      
      
    
#' Reads a list of tecplot files.
#'
#' This function loads a list of files
#' which have the same structure. and returns
#' a data structure with all the data.
#'
#' @param files File names of structured meshes with same structure.
#' @return List with all the data loaded.
#' @export
readTecFiles <- function(files){

  n <- length(files)

  z <- readTecplot(files[1])


  nvars <- length(z$header)
  I <- z$I
  J <- z$J
  K <- z$K

  np <- I*J*K

  data <- list()
  for (i in 1:nvars){
    data[[i]] <- matrix(0, np, n)
    data[[i]][,1] <- z$data[,i]
  }

  for (i in 2:n){
    z <- readTecplot(files[i])
    for (j in 1:nvars)
      data[[j]][,i] <- z$data[,j]
  }

  names(data) <- z$header
  return(list(data=data, I=I, J=J, K=K))
  

}



statsPiv <- function(files){

  data <- readTecFiles(files)
  d <- data$data
  
  # Velocidade média:

  Um <- rowMeans(d$Vx)
  Wm <- rowMeans(d$Vy)
  
  sU <- apply(d$Vx, 1, sd)
  sW <- apply(d$Vy, 1, sd)

  k <- (sU^2 + sW^2) / 2
  u <- d$Vx
  w <- d$Vy
  nt <- dim(u)[2]
  
  for (i in 1:nt)
    u[,i] <- u[,i] - Um
  for (i in 1:nt)
    w[,i] <- w[,i] - Wm

  uw <- u*w
  uw <- rowMeans(uw)
  
  d <- data.frame(x=d$x[,1], y=d$y[,1], U=Um, W=Wm, sU=sU, sW=sW, kturb=k, uw=uw)

  return(list(data=d, I=data$I, J=data$J))
    
}


writeTecplot1 <- function(fname, d){
  f <- file(fname, open='w')
  cat("TITLE =", 'VEL_STATS\n', file=f)
  cat("VARIABLES =", d$header, '\n', file=f)
  if (!is.null(d$K) && d$K > 1)
    cat('ZONE T="helideck", I=', d$I, ', J=', d$J, ', K=', d$K, '\n', file=f)
  else
    cat('ZONE T="helideck", I=', d$I, ', J=', d$J, '\n', file=f)
  write.table(d$data, row.names=FALSE, col.names=FALSE, file=f)

  close(f)

}


#' Write a tecplot structured mesh to a file.
#'
#' Creates a tecplot structured mesh file.
#'
#' @param fname Name of the file to be created.
#' @param tec List of zone data.
#' @param tectitle Title of the tecplot file (optional).
#' @export
writeTecplot <- function(fname, tec, tectitle=NULL){
  f <- file(fname, open='w')
  if (!is.null(tectitle))
    cat("TITLE =", tectitle, '\n', file=f)

  
  cat("VARIABLES =", tec[[1]]$header, '\n', file=f)
  nelems <- length(tec)
  for (e in 1:nelems){
    d <- tec[[e]]
    if (!is.null(d$K) && d$K > 1)
      cat('ZONE T="Element', e, '", I=', d$I, ', J=', d$J, ', K=', d$K, '\n', file=f)
    else
      cat('ZONE T="Element', e, '", I=', d$I, ', J=', d$J, '\n', file=f)
    write.table(d$data, row.names=FALSE, col.names=FALSE, file=f)
  }
  close(f)

}


  
joinTec <- function(teclst, z=NULL, zvar="z"){

  
  # This function joins different planes
  K <- length(teclst)
  
  if (is.null(z)) z <- 0:(K-1)

  if (teclst[[1]]$K != 1) stop("The files are already 3D")

  I <- teclst[[1]]$I
  J <- teclst[[1]]$J
  NIJ <- I*J
  
  
  header=c(zvar, teclst[[1]]$header)
  data <- cbind(rep(z[1], NIJ), teclst[[1]]$data)

  if (K > 1)
    for (n in 2:K)
      data <- rbind(data, cbind(rep(z[n], NIJ), teclst[[n]]$data))

  return(list(data=data, I=I, J=J, K=K, header=header))
}




  

  


  
  
  
    
