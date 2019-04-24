\name{mulSSE}
\alias{mulSSE}

\alias{mulSSE}

\title{MulCom Sum of Square Error}

\description{
  Computes sum of square errors for all the experimental points in the datasets
  Should not be called directly
}
\usage{
mulSSE(vec, index)
}
\arguments{
  \item{vec}{a numeric vector with data measurements}
  \item{index}{a numeric vector with the labels of the samples. 0 are the control samples. 
     number should be progressive}
}
\value{
  \item{vec}{a numeric vector with data measurements}
  \item{index}{a numeric vector with the labels of the samples. 0 are the control samples.
     number must be progressive}
}
\details{
  \code{mulSSE} An internal function that should not be called directly. It
    calculates sum of square error in the groups defined in the index class
    vector.
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
