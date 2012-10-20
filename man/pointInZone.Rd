\name{pointInZone}
\alias{pointInZone}
\title{Determine if point is inside a grid.}
\usage{
  pointInZone(z, x, y)
}
\arguments{
  \item{z}{Tecplot 2D structured zone.}

  \item{x}{X coordinate of the point.}

  \item{y}{Y coordinate of the point.}
}
\value{
  Indices (i,j) of the grid where the point is located or
  NULL.
}
\description{
  Find out whether a point is inside a grid. The grid is
  defined as a tecplot 2D structured zone. A structured
  grid is determined by an a 2D array of points where index
  I runs faster that index J. This function searches each
  quadrilateral that makes up the grid and determines in
  which one the point is. If found, it returns the
  corresponding (i,j) index. Else return NULL.
}

