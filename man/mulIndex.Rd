\name{mulIndex}
\alias{mulIndex}


\title{Mulcom Index for Monte Carlo Simlation}

\description{
  Random assebly of the groups indices for Monte Carlo Simulation 
}
\usage{
mulIndex(index, np, seed)
}
\arguments{
  \item{index}{ the vector with the groups of analysis, must be numeric and 0 correspond to the reference.
}
  \item{np}{ number of permutation in the simulation 
}
  \item{seed}{  seed for permtations
}
}
\value{
   A matrix with all indices permutations
}
\details{
     'mulIndex' generates random index for the function mulPerm. it is not directly
     called by the user.
}
\examples{
data(benchVign)
mulcom_scores <- mulIndex(Affy$Groups, 5, 7)
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
