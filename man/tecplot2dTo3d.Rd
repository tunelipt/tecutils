\name{tecplot2dTo3d}
\alias{tecplot2dTo3d}
\title{Extends a tecplot element from 2D to  3D.}
\usage{
  tecplot2dTo3d(tec, nz = 5, LZ = 1)
}
\arguments{
  \item{tec}{Tecplot element.}

  \item{nz}{Number of planes along z direction.}

  \item{LZ}{Length of the domain in z direction.}
}
\value{
  3D tecplot element.
}
\description{
  Given a 2D field, this function extends the field from 2D
  to 3D.
}

