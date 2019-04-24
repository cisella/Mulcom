\name{mulPermC}
\alias{mulPermC}


\title{MulCom Permutation}

\description{
  R pipe to C function not called directly by  user that reiterate MulCom Test on permutated data to perform Monte Carlo simulation
}
\usage{
mulPermC(eset, index, means, mse, n, m, nump, ngroups, reference)
}
\arguments{
  \item{eset}{An an \code{\link[affy:AffyBatch-class]{AffyBatch}} object, each row of must correspond to a variable and each column to a sample.}
  \item{index}{a numeric vector of length ncol(data) with the labels of the samples. 0 are the reference samples.}
  \item{means}{entry for the means output.}
  \item{mse}{entry for the mean square errors output}
  \item{n}{number of rows in obext of class eset}
  \item{m}{number of columns}
  \item{nump}{number of permutation to perform}
  \item{ngroups}{a number corresponding to the number of groups in the analysis.}
  \item{reference}{reference for the comparisons. typically it is 0}
}
\details{
   \code{mulPerm} 
}

\examples{
data(benchVign)
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
