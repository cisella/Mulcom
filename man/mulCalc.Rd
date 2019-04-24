\name{mulCalc}
\alias{mulCalc}

\alias{mulCalc}

\title{MulCom Calculation}

\description{Calculates MulCom test score for given m and t parameters}
\usage{
mulCalc(Mulcom_P, m, t)
}
\arguments{
  \item{Mulcom_P}{an object of class MULCOM}
  \item{m}{m: a numeric value corresponding to log 2 ratio correction for MulCom Test}
  \item{t}{t: a numeric value corresponding to T values for MulCom Test}
}
\details{
  \code{mulCalc} Calculate the Mulcom Score with m and t defined by the user
  \describe{
     \item{\code{Mulcom_P}:}{an object of class MULCOM_P}
  }
  \describe{
     \item{\code{m}:}{a number corresponding to log 2 ratio correction for MulCom Test}
  }
  \describe{
     \item{\code{t}:}{a number corresponding to T values for MulCom Test}
  }
}
\examples{
data(benchVign)
mulcom_scores <- mulScores(Affy, Affy$Groups)
mulcom_calc <- mulCalc(mulcom_scores, 0.2, 2)
}



\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
