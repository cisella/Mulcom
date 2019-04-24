\name{mulCAND}
\alias{mulCAND}

\title{Identify the Mulcom candidate feature selection}

\description{Identify the Mulcom candidate feature selection by the m and T defined by the user}
\usage{
mulCAND(eset, Mulcom_P, m, t, ese = "T")
}
\arguments{
  \item{eset}{an \code{\link[affy:AffyBatch-class]{AffyBatch}}}
  \item{Mulcom_P}{an object of class MULCOM}
  \item{m}{m: a numeric vector corresponding to log 2 ratio correction}
  \item{t}{t: a numeric vector corresponding to the MulCom T values}
  \item{ese}{True or False}
}
\details{
  \code{mulCAND} Identify the Mulcom candidate feature selection by the m and T defined by the user
}

\examples{
data(benchVign)
mulcom_perm <- mulPerm(Affy, Affy$Groups, 10,2)
mulcom_cand <- mulCAND(Affy, mulcom_perm, 0.2, 2)
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
