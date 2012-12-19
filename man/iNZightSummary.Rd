\name{iNZightSummary}
\alias{iNZightSummary}
\title{
  Informative summary information for linear models
}
\description{
  This function aims to improve upon the base R summary output.
  More information is provided, and this function provides both creates
  and returns a summary object as well as printing it.
}
\usage{
iNZightSummary(x,
               method = "standard",
               reorder.factors = TRUE,
               digits = max(3, getOption("digits") - 3),
               symbolic.cor = x$symbolic.cor,
               signif.stars = getOption("show.signif.stars"), ...)
}
\arguments{
  \item{x}{
    an object of class \code{"lm"}, usually, a result of a call to
    \code{\link{lm}}.
  }
  \item{method}{
    One of either \code{"standard"} or \code{"bootstrap"}. If
    \code{"bootstrap"}, then construct bootstrapped estimates and
    standard errors. Otherwise, use the standard estimates.
  }
  \item{reorder.factors}{
    If there are factors present in the model, and \code{TRUE} is given
    to this argument, then the most common level of the factor is set to
    be the baseline.
  }
  \item{digits}{
    the number of significant digits to use when printing.
  }
  \item{symbolic.cor}{
    logical. If \code{TRUE}, print the correlations in
    a symbolic form (see \code{\link{symnum}}) rather than as numbers.
  }
  \item{signif.stars}{
    logical. If \code{TRUE}, \sQuote{significance stars}
    are printed for each coefficient.
  }
  \item{\dots}{
    further arguments passed to or from other methods.
  }
}
\details{
  This summary function provides more information in the following ways:

  Factor headers are now given. The base level for a factor is also
  listed with an estimate of 0. This is to make it clear what the base
  level of a factor is, rather than attempting to work out by deduction
  from what has already been printed.

  The p-value of a factor is now given, this is not simply the output
  from the \code{\link{anova}} function. The reason is because
  \code{\link{anova}} works sequentially, instead we use
  \code{\link[car]{Anova}} to calculate the p-value based off Type III
  sums of squares.

  Each level of a factor is indented by 2 characters for its label and
  its p-value to distinguish between a factor and a level of a factor.

  The labels for each level of an interaction is now just the levels of
  the factor (separated by a \code{.}), rather than being prepended with
  the factor name also.
}
\value{
  An object of class \code{summary.lm}.
}
\author{
  Simon Potter.
}
\note{
  If any level is not observed in a factor, no p-values will be printed
  on all factors. This is because we cannot calculate Type III sums of
  squares if that is the case.
}

\seealso{
  The model fitting function \code{\link{lm}}, \code{\link{summary}}.

  Function \code{\link{coef}} will extract the matrix of coefficients
  with standard errors, t-statistics and p-values.

  To calculate p-values for factors, use \code{\link[car]{Anova}} with
  type three sums of squares.
}
