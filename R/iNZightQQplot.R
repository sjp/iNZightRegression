iNZightQQplot <- function(x, n = 5) {
    
  # ------------------------------------------------------------------ #
  # Generates n (= 5) parameteric bootstrap samples with normal errors
  # from the fitted values. These are plotted and overlaid by the qq
  # values from the original data set.
  # ------------------------------------------------------------------ #
    
  # Assumes lm model (gaussian errors only)
    if (isGlm(x))
        if (x$family$family != 'gaussian')
            stop('histrogramArray only works with linear models.')
    
  # Calculate the QQ values for the fitted model
    dropInf <- function(x, h) {
        if (any(isInf <- h >= 1)) {
            warnings("Not plotting observations with leverage one:\n ",
                     paste(which(isInf), collapse = ", "), call. = FALSE)
            x[isInf] <- NaN
        }
        x
    }
    qqVals <- function(x) {
        hii <- lm.influence(x, do.coef = FALSE)$hat
        r <- residuals(x)
        s <- sqrt(deviance(x) / df.residual(x))
        rs <- dropInf(r / (s * sqrt(1 - hii)), hii)
        qq <- normCheck(rs, plot = FALSE)
        qq
    }
    qq <- qqVals(x)

  # Generate n bootstrap samples with normal errors
    n.obs <- nrow(x$model)
    bootstrapSample <- bootstrapData(x, sample(1:n.obs, replace = TRUE))
    response <- rownames(attr(x$terms, "factors"))[1]
    newcall <- modifyModelCall(x)

    qqList <- vector("list", length = n)
    sd <- ifelse(isGlm(x),
                 sqrt(1 / (n.obs - ncol(x$model)) *
                      sum((x$residuals)^2)),
                 summary(x)$sigma)
    
    for (i in 1:n) {
        rNormal <- rnorm(n.obs, sd = sd)
        bootstrapSample[, response] <- x$fitted.values + rNormal
        newlm <- eval(newcall)
        qqList[[i]] <- qqVals(newlm)
    }

  # Set up a plotting window
    xlim <- range(qq$x)
    ylim <- range(sapply(qqList, function(q) q$y))

    plot.new()
    plot.window(xlim = xlim, ylim = ylim)
    axis(1)
    axis(2)
    box()
    title(xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
          main = "")
    abline(c(0, 1), lty = 2)

  # Plot the n random qq values
    for (i in 1:n)
        points(qqList[[i]], pch = 4, cex = 0.8,
               col = hcl(i/n * 360, 80, 50))

  # Overlay the true values
    points(qq, pch = 1, cex = 0.8, lwd = 2,
           col = hcl(240, 80, 0))

  # Add a legend
    legend("topleft", c("Original Data", "Sampled Normal Errors"),
           pch = c(1, 4), pt.cex = 0.8, col = "black",
           bty = "n")
}
  

