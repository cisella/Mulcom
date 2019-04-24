\name{mulDELTA}
\alias{mulDELTA}
\title{MulCom Delta}
\description{
  Computes Delta for all the experimental points in the datasets in respect to control
  Should not be called directly
}
\usage{
mulDELTA(vector, index)
}
\arguments{
  \item{vector}{vector: numeric vector with data measurements}
  \item{index}{a numeric vector with the labels of the samples. 0 are the control samples. 
     number must be progressive}
}
\value{
  \item{vector}{a numeric vector with data measurements}
  \item{index}{a numeric vector with the labels of the samples. 0 are the control samples.
     number must be progressive}
}
\details{
   \code{mulDELTA} An internal function that should not be called directly. It 
     calculates differential expression in the groups defined in the index class 
     vector, in respect to the 0 groups
}
\examples{
data(benchVign)
mulcom_delta <- mulDELTA(exprs(Affy[1,]),Affy$Groups)

}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
