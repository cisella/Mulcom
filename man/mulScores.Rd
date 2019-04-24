\name{mulScores}
\alias{mulScores}


\title{MulCom Score Calculation}

\description{
  Computes the scores for the MulCom test. The function 
  calculates the numerator and the denominator of the test
  without the parameters m and t
}
\usage{
mulScores(eset, index)
}
\arguments{
  \item{eset}{An an \code{\link[affy:AffyBatch-class]{AffyBatch}} object, each row
          of must correspond to a variable and each column to a sample.
}
  \item{index}{a numeric vector of length ncol(data) with the labels of the samples. 0 are the reference samples. 
}
}
\value{
  An Object of class MULCOM from Mulcom package
}
\details{
     'mulScore' computes the scores for the MulCom test for multiple point profile.
     The Mulcom test is designed to compare each experimental mean with the control mean
     and it is derived from the "Dunnett's test".  Dunnett's test controls the Experiment-wise Error Rate and is more powerful
     than tests designed to compare each mean with each other mean. The test is
     conducted by computing a modified t-test between each experimental group and
     the control group. 
    
}
\examples{
data(benchVign)
mulcom_scores <- mulScores(Affy, Affy$Groups)
}


\author{Claudio Isella, \email{claudio.isella@ircc.it}}
\keyword{MulCom}
