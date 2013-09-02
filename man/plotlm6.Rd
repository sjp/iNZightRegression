\name{plotlm6}
\alias{plotlm6}
\title{
  Extended Plot Diagnostics for (g)lm models
}
\description{

  These plots are an extension to the original plots provided by
  \code{plot.lm}.

  Six plots (selectable by \code{which}) are currently available: a plot
  of residuals against fitted values, a Scale-Location plot of
  \eqn{\sqrt{| residuals |}}{sqrt(| residuals |)} against fitted values,
  a plot of residuals against leverages, a plot of Cook's distances
  versus row labels, a Normal Q-Q plot and histogram.

  Also provided is a summary plot which shows all diagnostic plots
  arranged in a 2 by 3 grid. By default, this is shown first, then each
  of the individual plots in turn.

}
\usage{
plotlm6(x,
        which = 1:6,
        panel = if (add.smooth) panel.smooth else points,
        sub.caption = NULL,
        main = "",
        ask = prod(par("mfcol")) < length(which) &&
              dev.interactive(),
        ...,
        id.n = 3,
        labels.id = names(residuals(x)),
        cex.id = 0.75,
        qqline = TRUE,
        cook.levels = c(0.5, 1),
        add.smooth = getOption("add.smooth"),
        label.pos = c(4, 2),
        cex.caption = 1,
        showBootstraps = nrow(x$model) >= 30)
}
\arguments{
  \item{x}{
    \code{lm} object, typically the result of \code{\link{lm}} or
    \code{\link{glm}}. Can also take \code{\link[survey]{svyglm}}
    objects.
  }
  \item{which}{
    If a subset of the plots is required, specify a subset of the
    numbers \code{1:6}. \code{7} will produce a summary plot showing all
    of the plots arranged in a grid. \code{1:6} will show the summary
    plot followed by each of the single plots one by one.
  }
  \item{panel}{
    panel function. The useful alternative to \code{\link{points}},
    \code{\link{panel.smooth}} can be chosen by \code{add.smooth = TRUE}.
  }
  \item{sub.caption}{
    common title---above the figures if there are more than one; used as
    \code{sub} (s.\code{\link{title}}) otherwise.  If \code{NULL}, as by
    default, a possible abbreviated version of \code{deparse(x$call)} is
    used.
  }
  \item{main}{
    title to each plot---in addition to \code{caption}.
  }
  \item{ask}{
    logical; if \code{TRUE}, the user is \emph{ask}ed before each plot,
    see \code{\link{par}(ask=.)}. Ignored when only one plot is being
    shown.
  }
  \item{\dots}{
    other parameters to be passed through to plotting functions.
  }
  \item{id.n}{
    number of points to be labelled in each plot, starting with the most
    extreme.
  }
  \item{labels.id}{
    vector of labels, from which the labels for extreme points will be
    chosen. \code{NULL} uses observation numbers.
  }
  \item{cex.id}{
    magnification of point labels.
  }
  \item{qqline}{
    logical indicating if a \code{\link{qqline}()} should be added to
    the normal Q-Q plot.
  }
  \item{cook.levels}{
    levels of Cook's distance at which to draw contours.
  }
  \item{add.smooth}{
    logical indicating if a smoother should be added to most plots; see
    also \code{panel} above.
  }
  \item{label.pos}{
    positioning of labels, for the left half and right half of the graph
    respectively, for plots 1-3.
  }
  \item{cex.caption}{
    controls the size of \code{caption}.
  }
  \item{showBootstraps}{
    logical; if \code{TRUE}, then bootstrap loess smooths are shown in
    the first 4 plots. By default, these are only shown for sample sizes
    greater than 30.
  }
}
\details{
  For the residuals vs fitted values plot, we add bootstrapped smoothers
  to illustrate variance. The smoother is also added to the
  Scale-Location plot too.

  The Normal Q-Q and histogram plots are taken from the \code{normcheck}
  function in the \code{s20x} package.
}
\author{
  Simon Potter, David Banks. Original authors of \code{plot.lm} are John
  Maindonald and Martin Maechler.
}

\seealso{
  \code{\link{plot.lm}}
}
