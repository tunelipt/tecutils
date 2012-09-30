\name{addRadius}
\alias{addRadius}
\title{Calculate distance from points.}
\usage{
  addRadius(tec, xc = 0, yc = 0)
}
\arguments{
  \item{tec}{Tecplot element.}

  \item{xc}{Vector with x center of the cylinders.}

  \item{yc}{Vector with y center of the cylinders should
  have the same length as xc.}
}
\value{
  Element with new fields: distance from each cylinder
  center.
}
\description{
  To be able to draw cylinders easily, this function
  calculates the distance from a set of points.
}

