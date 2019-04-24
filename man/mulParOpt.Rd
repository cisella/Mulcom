\name{mulParOpt}
\alias{mulParOpt}

\title{MulCom Parameters Optimization}

\description{MulCom parameter optimization function to identify best combination of t and m providing maximum number of genes at a given FDR}
\usage{
   mulParOpt(perm, M.Opt, ind, th, image = "T")
}
\arguments{
  \item{perm}{a object with permutated MulCom Scores}
  \item{M.Opt}{an MulCom optimization object}
  \item{ind}{index corresponding to the comparison to plot}
  \item{th}{a threshold for the FDR plot}
  \item{image}{default = "T", indicates is print the MulCom optimization plot}
}
\details{
  \code{mulParOpt}     The function mulParOpt is designed to identify the optimal m and t values
combination leading to the maximum number of differentially regulated genes
satisfying an user define FDR threshold.
In case of equal number of genes, the combination of m and
t with the lower FDR will be prioritized. In case of both identical number of
genes and FDR, the function will chose the highest t. The function optionally
will define a graphical output to visually inspect the performance of the test at
given m and t parameters for a certain comparison.
 
}


\examples{
   data(benchVign)
   mulcom_perm <- mulPerm(Affy, Affy$Groups, 10,2)
   mulcom_opt <- mulOpt(mulcom_perm, vm=seq(0.1, 0.5, 0.1), vt=seq(1, 3,1))
   mulParOpt(mulcom_perm, mulcom_opt, 1, 0.05)
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
