\name{histogramArray}
\alias{histogramArray}
\title{
  Histogram Array
}
\description{
  Produces an array of histograms to compare against the histogram of
  the residuals of a model.
}
\usage{
histogramArray(x, n = 7)
}
\arguments{
  \item{x}{
    An \code{lm} object.
  }
  \item{n}{
    The number of additional histograms to plot alongside the original
    histogram.
  }
}
\details{
  The histogram of the model \code{x} appears in the (1,1) position. For
  each of the other histograms, the fitted values of \code{x} are taken
  and normal random errors are added to these. The normal random errors
  have standard error equal to the estimated residual standard error of
  \code{x}. A model is then fitted to this altered data and a histogram
  is produced.
}
\value{
  None.
}
\author{
  David Banks
}
