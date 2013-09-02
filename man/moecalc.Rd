\name{moecalc}
\alias{moecalc}
\alias{ses.moecalc}
\title{
  Margin of Error Calculations
}
\description{
  TODO: DESCRIBE BETTER.
}
\usage{
moecalc(x, factorname = NULL, levelnames = NULL,
        coef.idx = NULL, est = NULL, ci = NULL, base = TRUE,
        basename = "base", conf.level = 1.96)
ses.moecalc(ses, ses.diffs)
}
\arguments{
  \item{x}{
    A model object (i.e. \code{lm}) or an \code{ses.moecalc} object.
  }
  \item{factorname}{
    Character vector. The name of the factor. Can be an interaction.
  }
  \item{levelnames}{
    The names of each level of the factor.
  }
  \item{coef.idx}{
    The coefficient index.
  }
  \item{est}{
    Estimate.
  }
  \item{ci}{
    The relevant confidence interval, only use when the length of
    \code{est} is 2.
  }
  \item{base}{
    Whether a baseline level is set. Defaults to \code{TRUE}.
  }
  \item{basename}{
    Label name to apply to baseline level, if \code{base} is
    \code{TRUE}.
  }
  \item{conf.level}{
    Confidence level for both error bars and confidence intervals.
  }
  \item{ses}{
    A numeric vector of standard errors.
  }
  \item{ses.diffs}{
    A matrix of standard error differences.
  }
}
\value{
  An \code{moecalc} object. Which has the following methods:
  \code{plot}, \code{print}, \code{summary}.

  \code{ses.moecalc} produces an object of class \code{"ses.moecalc"}.
}
\author{
  Danny Chang, David Banks
}
