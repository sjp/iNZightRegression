multicomp <- function(x, ...) {
  UseMethod("multicomp")
}

multicomp.moecalc <- function(x, ...) {
  if (! is.null(x$est)) {
    cols <- c("Estimate", "Lower", "Upper", "p-value (unadj.)")
    levelnames <- x$xlevels[[1]]
    k <- length(levelnames)
    nr <- k * (k - 1) / 2
    result.matrix <- matrix(0, nrow = nr, ncol = length(cols))
    row <- 1
    names <- character(nr)
    df <- ifelse(isGlm(x$fit), x$fit$df.null, x$fit$df)

    for (i in 1:(k - 1)) {
      for (j in (i + 1):k) {
        est <- - x$est.diffs[j, i]
        bounds <- est + c(-1, 1) * x$moe.diffs[j, i]
        # TODO: Adjust p-values for multiple comparisons using Tukey or Bonf
        pval <- pt(abs(est / x$ses.diffs[j, i]), df = df, lower.tail = FALSE)
        result.matrix[row, ] <- c(est, bounds, pval)
        names[row] <- paste(levelnames[i], " - ", levelnames[j])
        row <- row + 1
      }
    }
    rownames(result.matrix) <- names
    colnames(result.matrix) <- cols
    class(result.matrix) <- "multicomp"
    result.matrix
  } else {
    cat("Errbars\n")
    print(x$ErrBars)
  }
}

print.multicomp <- function(x, ...) {
  printCoefmat(x, P.values = TRUE, has.Pvalue = TRUE, ...)
  invisible(x)
}
