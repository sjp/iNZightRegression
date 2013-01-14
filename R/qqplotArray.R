qqplotArray = function(x, n = 7) {
  ## Assumes lm model (gaussian errors only)
  n = min(n, 11)
  nRows = 2
  nCols = ceiling(round(n + 1) / nRows)
  opar = par(mfrow = c(2, nCols))
  on.exit(par(opar))

  dropInf <- function(x, h) {
    if (any(isInf <- h >= 1)) {
      warning("Not plotting observations with leverage one:\n  ",
              paste(which(isInf), collapse = ", "), call. = FALSE)
      x[isInf] <- NaN
    }
    x
  }
  hii = lm.influence(x, do.coef = FALSE)$hat
  r = residuals(x)
  s = sqrt(deviance(x)/df.residual(x))
  rs = dropInf(r/(s * sqrt(1 - hii)), hii)
  qq = normCheck(rs, plot = FALSE)
  ylims = range(qq$y)
     
  newdat = x$model
  response = rownames(attr(x$terms, "factors"))[1]
  newcall = modifyModelCall(x, "newdat")
  
  n.obs = nrow(x$model)
  rsList = list()
  for (i in 1:n) {
    rNormal = rnorm(n.obs, sd = summary(x)$sigma)
    newdat[, response] = x$fitted.values + rNormal
    newlm = eval(parse(text = newcall))
    r = residuals(newlm)
    s = sqrt(deviance(newlm) / df.residual(newlm))
    hii <- lm.influence(newlm, do.coef = FALSE)$hat
    rsList[[i]] = dropInf(r/(s * sqrt(1 - hii)), hii)
    qq = normCheck(rsList[[i]], plot = FALSE)
    ylims = range(c(ylims, qq$y), na.rm = TRUE)
  }

  normCheck(rs, xlab = "Theoretical Quantiles",
            ylab = "Sample Quantiles", ylim = ylims)
  mtext("Original data", 3, font = 2, col = "navy", line = 1)
  
  for (i in 1:n) {
    normCheck(rsList[[i]], xlab = "Theoretical Quantiles",
                   ylab = "Sample Quantiles", ylim = ylims,
                   shapiroWilk = FALSE)
    mtext(paste("Normal errors: sample", i), 3, font = 2,
          col = "navy", line = 1)
  }
}