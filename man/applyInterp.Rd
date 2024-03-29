% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/polygon.R
\name{applyInterp}
\alias{applyInterp}
\title{Interpolate a mesh.}
\usage{
applyInterp(zlst, phi)
}
\arguments{
\item{zlst}{Tecplot mesh to be interpolated.}

\item{phi}{Interpolation data structure created with \code{\link{interpPoints}}}
}
\value{
Interpolated values (every variable in the mesh.
}
\description{
This function uses the interpolation data
structure created with \code{\link{interpPoints}}
to interpolate the values of a mesh made up of
2D tecplot structured zones.
}
