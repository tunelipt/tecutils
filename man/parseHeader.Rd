\name{parseHeader}
\alias{parseHeader}
\title{Parses tecplot header.}
\usage{
  parseHeader(header)
}
\arguments{
  \item{header}{Tecplot header characterizing the
  variables.}
}
\value{
  Character vector with variable names.
}
\description{
  This function parses a tecplot file header (the format
  given by nektar) It returns a list containing the labels
  of the variables, the number of variables, the position
  of var x, y and z. If z is not present, it is a 2d file,
  else a 3d file
}

