\name{ses.moecalc}
\alias{ses.moecalc}
\title{
  Constructor function for Standard Error Class
}
\description{
  Constructs a standard error object, consisting of standard errors and
  a difference matrix. Used in margin of error calculations, see
  \code{\link{moecalc}}.
}
\usage{
ses.moecalc(ses, ses.diffs)
}
\arguments{
  \item{ses}{
    numeric, vector of standard errors.
}
  \item{ses.diffs}{
    matrix of standard error differences.
}
}
\value{
  An object of class \code{ses.moecalc}.
}
\author{
  David Banks.
}

\seealso{
  \code{\link{moecalc}}, \code{\link{multicomp}}
}
