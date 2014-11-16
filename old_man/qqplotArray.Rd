\name{iNZightQQplot}
\alias{iNZightQQplot}
\title{
  iNZight QQ Plot
}
\description{
  Produces a sample of QQ-plots based on the fitted values, overlaid by
  a QQ-plot of the original data.
}
\usage{
iNZightQQplot(x, n = 5)
}
\arguments{
  \item{x}{
    an \code{lm} or \code{svyglm} object (with \code{family = "Gaussian"}.
}
  \item{n}{
    the number of sampled QQ plots to produce beneath the QQ plot of
    \code{x}.
}
}
\details{
  Multiple bootstrap models are generated from the fitted values of
  the model, each with different random normal errors with standard 
  error equal to the estimated residual standard error from the
  original model. These are plotted, and then overlaid by the QQ plot
  from the original data.

  This plot can be used to assess the assumption of normality in the
  residuals for a linear regression model.
}
\value{
None.
}
\author{
  David Banks, Tom Elliott.
}


\seealso{
  \code{\link{histogramArray}}
}
\examples{
fit <- lm(Volume ~ Height + Girth, data = trees)
iNZightQQplot(fit)
}
