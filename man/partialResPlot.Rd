\name{partialResPlot}
\alias{partialResPlot}
\title{
  Partial residual plots
}
\description{
  Draws partial residual plots given a fitted model. Also plots a smooth
  through the points, a straight line representing the linear trend we
  expect to see, and a nest of bootstrap smooths.
}
\usage{
partialResPlot(fit, varname, showBootstraps = nrow(fit$model) >= 30)
}
\arguments{
  \item{fit}{
    An lm or glm object
  }
  \item{varname}{
    Character vector. The name of the variable that is shown in the partial residual
    plot. Must refer to a continuous variable.
  }
  \item{showBootstraps}{
    A logical value. Whether or not to show bootstrap smoothers. This will
    be \code{TRUE} by default for models where there are at least 30
    observations.
  }
}
\value{
  None.
}
\author{
  David Banks
}
\examples{
fit <- lm(Volume ~ Girth + Height, data = trees)
partialResPlot(fit, "Girth")
}
