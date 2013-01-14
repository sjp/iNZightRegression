\name{histogramArray}
\alias{histogramArray}
\title{
  Histogram Array
}
\description{
Produces an array of histograms to compare against the histogram of the residuals of a model. 
}
\usage{
histogramArray(x, n = 7)
}
\arguments{
  \item{x}{
An lm object
}
  \item{n}{
The number of additional histograms to plot alongside the original histogram.
}
}
\details{
The histogram of the model x appears in the (1,1) position. For each of the other histograms, the fitted values of
x are taken and normal random errors are added to these (the normal random errors have standard error equal to
the estimated residual standard error of x). Then a model is fitted to this altered data and a histogram
is produced. 
}
\value{
  None.
}
\author{
  David Banks
}
