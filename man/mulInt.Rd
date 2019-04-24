\name{mulInt}
\alias{mulInt}

\title{generates a consensus matrix from list of genes}

\description{generates a consensus matrix from list of genes}
\usage{
mulInt(...)
}
\arguments{
  \item{...}{the function requires vector files as imputs}
}
\details{
  \code{mulCAND} generates a consensus matrix from list of genes
}

\examples{
data(benchVign)
mulcom_perm <- mulPerm(Affy, Affy$Groups, 10,2)
mulcom_opt <- mulOpt(mulcom_perm, vm = seq(0,0.5, 0.1), vt = seq(1,3, 0.1))

h1_opt <- mulParOpt(mulcom_perm, mulcom_opt, ind = 1, th = 0.05)
h2_opt <- mulParOpt(mulcom_perm, mulcom_opt, ind = 1, th = 0.05)

int <- mulInt(h1_opt, h2_opt)

}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
