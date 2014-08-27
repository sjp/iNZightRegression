\name{partialResPlot}
\alias{partialResPlot}
\title{
  Partial residual plots
}
\description{
  Draws partial residual plots, given a fitted model. Also plots a
  smooth through the points, a straight line representing the linear
  trend we expect to see, and a next of bootstrap smooths.
}
\usage{
partialResPlot(fit, varname, showBootstraps = nrow(fit$model) >= 30)
}
\arguments{
  \item{fit}{
    an \code{lm}, \code{glm} or \code{svyglm} object.
}
  \item{varname}{
    character, the name of a variable that is shown in the partial
    residual plot. Must refer to a continuous variable.
}
  \item{showBootstraps}{
    logical, if \code{TRUE}, show bootstrap smoothers. By default, only
    shown for sample size of at least 30.
}
}
\value{
  None
}
\author{
  David Banks, Tom Elliott.
}

\seealso{
  \code{\link{allPartialResPlots}}
}