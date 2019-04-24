\name{mulMSE}
\alias{mulMSE}

\alias{mulMSE}

\title{MulCom Mean Square Error}

\description{
  Computes Mean Square Error for all the experimental points in the datasets in respect to control. should not be called directly
}
\usage{
mulMSE(vector, index, tmp = vector())
}
\arguments{
  \item{vector}{a numeric vector with data mesurements}
  \item{index}{a numeric vector with the labels of the samples. 0 are the control samples. 
     number must be progressive}
  \item{tmp}{a vector}
}
\value{
  \item{vector}{a numeric vector with data measurements}
  \item{index}{a numeric vector with the labels of the samples. 0 are the control samples.
     number must be progressive}
  \item{tmp}{a vector}
}
\details{
   \code{mulMSE} An internal function that should not be called directly. It
     calculates within group means square error for the values defined in the x vector
     according to the index class
     vector
}



\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
