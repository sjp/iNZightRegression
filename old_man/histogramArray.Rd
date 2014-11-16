\name{histogramArray}
\alias{histogramArray}
\title{
  Histogram Array
}
\description{
  Produces an array of histograms to compare against the histogram of
  residuals for a fitted linear model.
}
\usage{
histogramArray(x, n = 7)
}
\arguments{
  \item{x}{
    an \code{lm} or \code{svyglm} object.
  }
  \item{n}{
    the number of additional histograms to plot alongside the original.
  }
}
\details{
  The histogram of the model \code{x} appears in the top-left
  position. For each of the other histograms, the fitted values of
  \code{x} are taken and normal random errors are added to these. The
  normal residual standard errors have standard error equal to the
  estimated residual standard error of \code{x}. A model is then fitted
  to this altered data and a histogram is produced.
}
\value{
  None.
}
\author{
  David Banks
}

\seealso{
  \code{\link{iNZightQQplot}}
}