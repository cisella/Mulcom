\name{mulFSG}
\alias{mulFSG}

\title{MulCom False Significant Genes}

\description{Calculate the False Significant Genes for m and t defined by the user}
\usage{
mulFSG(Mulcom_P, m, t)
}
\arguments{
  \item{Mulcom_P}{an object of class MULCOM}
  \item{m}{m: a numeric value corresponding to log 2 ratio correction for MulCom Test}
  \item{t}{t: a numeric value corresponding to t values for MulCom Test}
}
\details{
  \code{mulFDR} evaluate the False Significant genes on the Mulcom_P object 
  according to specific m and t parameters. For each permutation 
  it is calculated the number of positive genes. An estimation of the false 
  called genes is evaluated with the median for each experimental subgroups
}

\examples{
data(benchVign)
mulcom_perm <- mulPerm(Affy, Affy$Groups, 10, 7)
mulcom_fsg <- mulFSG(mulcom_perm, 0.2, 2)
}




\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
