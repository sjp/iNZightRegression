\name{qqplotArray}
\alias{qqplotArray}
\title{
  QQ Plot Array
}
\description{
  Produces an array of QQ Plots to compare against the QQ plot of a
  fitted linear model.
}
\usage{
qqplotArray(x, n = 7)
}
\arguments{
  \item{x}{
    an \code{lm} or \code{svyglm} object.
}
  \item{n}{
    the number of additional QQ plots to display beside the QQ plot of
    \code{x}.
}
}
\details{
  The QQ plot of the model \code{x} appears in the top-left
  position. For each of the other QQ plots, the fitted values of
  \code{x} are taken and normal random errors are added to these. The
  normal random errors have standard error equal to the estimated
  residual standard error of \code{x}. A model is then fitted to this
  altered data and a QQ plot is produced.
}
\value{
None.
}
\author{
  David Banks.
}


\seealso{
  \code{\link{histogramArray}}
}
\examples{
fit <- lm(Volume ~ Height + Girth, data = trees)
qqplotArray(fit)
}
