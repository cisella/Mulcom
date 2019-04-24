\name{mulPerm}
\alias{mulPerm}


\title{MulCom Permutation}

\description{
  Reiterate MulCom Test on permutated data to perform Montecarlo simulation
}
\usage{
mulPerm(eset, index, np, seed, segm = "F")
}
\arguments{
  \item{eset}{An an \code{\link[affy:AffyBatch-class]{AffyBatch}} object, each row of must correspond to a variable and each column to a sample.}
  \item{index}{a numeric vector of length ncol(data) with the labels of the samples. 0 are the reference samples.}
  \item{np}{a numeric values indicating the number of permutation to perform. It is set as default to 10}
  \item{seed}{set the seed of the permutaton, default is 1}
  \item{segm}{a default set to F. This parametheres requires to be setted to avoid segmentation fault of C subroutin in the case of very large datasets.}
}
\details{
   \code{mulPerm} 
}

\examples{
data(benchVign)
mulcom_perm <- mulPerm(Affy, Affy$Groups, 10,2)
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
