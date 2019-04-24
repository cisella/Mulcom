\name{mulOptPlot}
\alias{mulOptPlot}

\alias{mulOptPlot}

\title{MulCom optimization Plot}

\description{MulCom optimization Plot to identify best configuration paramters}
\usage{
   mulOptPlot(M.Opt, ind, th, smooth = "NO") 
}
\arguments{
  \item{M.Opt}{an MulCom optimization object}
  \item{ind}{index corresponding to the comparison to plot}
  \item{th}{a threshold for the FDR plot}
  \item{smooth}{indicates whether the FDR plot will show a significant threshold or will be continuous.}
}
\value{a numeric vector}
\details{
  \code{mulOptPlot} MulCom optimization Plot
}


\examples{
   data(benchVign)
   mulcom_perm <- mulPerm(Affy, Affy$Groups, 10,2)
   mulcom_opt <- mulOpt(mulcom_perm, vm=seq(0.1, 0.5, 0.1), vt=seq(1, 3,1))
   mulOptPlot(mulcom_opt, 1, 0.05)
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
