\name{mulOptPars}
\alias{mulOptPars}

\title{MulCom Parameter Optimization} 

\description{
   Function to optimize Mulcom parameter for maximim nuber of genes with a user defined FDR}
\usage{
   mulOptPars(opt, ind, ths) 
}
\arguments{
  \item{opt}{an MulCom optimization object}
  \item{ind}{index corresponding to the comparison}
  \item{ths}{a threshold for the FDR optimization, default is 0.05}
}
\details{
  \code{mulOptPars} MulCom optimization function to identify best parameters
}
\examples{
data(benchVign)
mulcom_perm <- mulPerm(Affy, Affy$Groups, 10, 7)
#mulcom_opt <- mulOpt(mulcom_perm, seq(0.1, 0.5, 0.1), seq(1, 3, 0.1))
#optThs <- mulOptPars(mulcom_opt, 1, 0.05)
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
