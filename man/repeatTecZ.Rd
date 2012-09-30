\name{repeatTecZ}
\alias{repeatTecZ}
\title{Repeats a 3D field along z direction.}
\usage{
  repeatTecZ(tec, n = 2)
}
\arguments{
  \item{tec}{Tecplot 3D periodic element.}

  \item{n}{Number of times the field should be repeated.}
}
\value{
  Extended 3D field.
}
\description{
  When a 3D field is periodic in the z direction,
  sometimes, for visualization purposes it is interesting
  to work with a longer field. This function repeats the
  original field to extend it accordingly.
}

