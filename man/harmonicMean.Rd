\name{harmonicMean}
\alias{harmonicMean}
\alias{harmonicMean}

\title{MulCom Harmonic Mean}

\description{
  Computes harmonic means across groups replicate 
  Should not be called directly
}
\usage{
harmonicMean(index)
}
\arguments{
  \item{index}{a numeric vector with the groups labels of the samples. 0 are the control samples. Number must be progressive}
}

\value{
  a numeric vector 
}
\details{
  \code{harmonicMean} calculates harmonic means across groups replicate for the estimation of Mulcom Test
}

\references{\email{claudio.isella@ircc.it}}

\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
