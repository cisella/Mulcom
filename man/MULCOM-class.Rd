\name{MULCOM-class}
\docType{class}
\alias{class:MULCOM}
\alias{MULCOM-class}
\alias{MULCOM}
\title{Class MulCom}
\description{This is a class representation MulCom test scores
}
\section{Objects from the Class}{
  Objects can be created using the function \code{\link{mulScores}}
  on \code{ExpressionSet}.
}
\section{Slots}{
  \describe{
    \item{\code{FC}:}{Object of class \code{numeric} representing difference between
    all experimental groups and the reference groups}
    \item{\code{HM}:}{Object of class \code{numeric} representing the harmonic means in all subgroups
    }
    \item{\code{MSE_Corrected}:}{Object of class \code{numeric} representing the
    MulCom test estimation of mean square error as described in the formula of
    the Dunnett's t-test
    }
  }
}
\examples{
data(benchVign)
mulcom_scores <- mulScores(Affy, Affy$Groups)
}

\author{ Claudio Isella }

\keyword{classes}
