histogramArray = function(x, n = 7) {
  if (isGlm(x))
      if (x$family$family != 'gaussian')
          stop('histrogramArray only works with linear models.')

  ## Assumes lm model (gaussian errors only)
  n = min(n, 11)
  nRows = 2
  nCols = ceiling(round(n + 1) / nRows)
  opar = par(mfrow = c(2, nCols))
  on.exit(par(opar))

  r = residuals(x)
  h = hist(r, plot = FALSE)
  xlab <- "Residuals"
  mx <- mean(r)
  sx <- sd(r)
  rx <- range(r)
  xmin <- min(rx[1], mx - 3.5 * sx, h$breaks[1])
  xmax <- max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)])

  bootstrapSample <- bootstrapData(x, 1:nrow(x$model))
  response = rownames(attr(x$terms, "factors"))[1]
  newcall = modifyModelCall(x)#, "newdat")

  n.obs = nrow(x$model)
  resList = list()
  breaksList = list()
  for (i in 1:n) {
    sd <- ifelse(isGlm(x),
                 sqrt(1 / (nrow(x$model) - ncol(x$model)) *
                      sum((x$residuals)^2)),
                 summary(x)$sigma)
    rNormal = rnorm(n.obs, sd = sd)
    bootstrapSample[, response] = x$fitted.values + rNormal
    newlm = eval(newcall)
    r = residuals(newlm)
    resList[[i]] = r
    h = hist(r, plot = FALSE)
    breaksList[[i]]  = h$breaks
    mx = mean(r)
    sx = sd(r)
    rx = range(r)
    xmin <- min(xmin, min(rx[1], mx - 3.5 * sx, h$breaks[1]))
    xmax <- max(xmax, max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)]))

  }
  binWidth = diff(breaksList[[1]][1:2])
  breaks = seq(binWidth * floor(xmin / binWidth), binWidth * ceiling(xmax / binWidth),
               by = binWidth)

  ## Need to define histograms again without drawing, to find max density
  ymax = 0
  for (i in 1:n) {
    r = resList[[i]]
    h = hist(r, plot = FALSE, breaks = breaks)
    ymax = max(ymax, max(h$density, dnorm(mean(r), mean(r), sd(r))) * 1.05)
  }
  oh = hist(residuals(x), breaks = breaks, plot = FALSE)
  r = residuals(x)
  maxOh = max(oh$density, dnorm(mean(r), mean(r), sd(r)))
  ymax = max(ymax, maxOh)

  ## Original data histogram of residuals
  hist(residuals(x), breaks = breaks, prob = TRUE, ylim = c(0, ymax),
       xlim = c(xmin, xmax), xlab = xlab, col = hcl(240, 80, 80), main = NULL)
  mtext("Original data", 3, font = 2, col = "navy", line = 1)
  box()
  x1 <- seq(xmin, xmax, length = 100)
  y1 <- dnorm(x1, mx, sx)
  lines(x1, y1, lwd = 1.5, lty = 3)

  ## Normal error sample histograms
  for (i in 1:n) {
    hist(resList[[i]], breaks = breaks, prob = TRUE, ylim = c(0, ymax),
         xlim = c(xmin, xmax), xlab = xlab,
         col = hcl(240, 80, 30), main = NULL)
    mtext(paste("Normal errors: sample", i), 3, font = 2,
          col = "navy", line = 1)
    box()
    x1 <- seq(xmin, xmax, length = 100)
    y1 <- dnorm(x1, mx, sx)
    lines(x1, y1, lwd = 1.5, lty = 3)
  }
}
