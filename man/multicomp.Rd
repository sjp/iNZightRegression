\name{multicomp}
\alias{multicomp}
\title{
  Multiple Comparisons Between Factor Levels
}
\description{
  This is a method for \code{moecalc} objects. Most other \code{moecalc}
  methods are concerned with how each factor level differs from the
  baseline. This does not show the significance of differences between
  non-baseline levels of a factor.

  Currently the p-values are not adjusted. As a result, more comparisons
  are likely to be significantly different when they shouldn't be.
}
\usage{
multicomp(x, ...)
}
\arguments{
  \item{x}{
    an \code{moecalc} object.
  }
  \item{\dots}{
    additional parameters to be passed to the function (currently
    ignored).
  }
}
\value{
  A \code{multicomp} object, primarily for automatic pretty printing of
  the resulting matrix.
}
\author{
  Simon Potter.
}
