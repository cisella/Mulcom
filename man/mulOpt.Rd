\name{mulOpt}
\alias{mulOpt}

\title{MulCom optimization}

\description{
  The function systematically performs the calculation of significant genes and corresponding FDR for all the combination of given list of m and t values.
}
\usage{
mulOpt(Mulcom_P, vm, vt) 
}
\arguments{
  \item{Mulcom_P}{an object of class Mulcom_P}
  \item{vm}{a vector of m values to test}
  \item{vt}{a vector of t values to test}
}
\details{
  \code{mulOpt} The function systematically performs the calculation of significant genes and corresponding FDR for all the combination of given list of m and t values.
}

\examples{
data(benchVign)
mulcom_perm <- mulPerm(Affy, Affy$Groups, 10, 7)
mulcom_opt <- mulOpt(mulcom_perm, seq(0.1, 0.5, 0.1), seq(1, 3, 0.1))
}

\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
