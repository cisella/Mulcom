\name{MULCOM_P-class}
\docType{class}
\alias{class:MULCOM_P}
\alias{MULCOM_P-class}
\alias{MULCOM_P}
\title{Class MulCom Permutation}
\description{This is a class representation MulCom test scores permutation
}
\section{Objects from the Class}{
  Objects can be created using the function \code{\link{mulScores}}
  on \code{ExpressionSet}.
}
\section{Slots}{
  \describe{
    \item{\code{FC}:}{Object of class \code{numeric} representing delta between
    all experimental groups and the reference groups}
    \item{\code{MSE_Corrected}:}{Object of class \code{numeric} representing the
    MulCom test estimation of mean square error as described in the formula of
    the Dunnett's t-test}
    \item{\code{FCp}:}{Object of class \code{numeric} representing delta between
    all experimental groups and the reference groups in permutated data}
    \item{\code{MSE_Correctedp}:}{Object of class \code{numeric} representing the
    MulCom test estimation of mean square error as described in the formula of
    the Dunnett's t-test in permutated data}
  }
}
\examples{
data(benchVign)
mulcom_scores <- mulScores(Affy, Affy$Groups)
}


\author{ Claudio Isella }

\keyword{classes}
