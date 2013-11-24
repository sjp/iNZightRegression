\name{allPartialResPlots}
\alias{allPartialResPlots}
\title{
  Draw multiple partial residual plots
}
\description{
  This function draws partial residual plots for all continuous
  explanatory variables in a given model. Clicking the device
  displays the next plot in the sequence.
}
\usage{
allPartialResPlots(fit)
}
\arguments{
  \item{fit}{
    an \code{lm}, \code{glm} or \code{svyglm} object.
}
}
\value{
  None.
}
\author{
  David Banks, Tom Elliott.
}

\seealso{
  \code{\link{partialResPlot}}
}
