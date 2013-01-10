histogramArray = function(x, n = 7) {
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
  ymax <- max(h$density, dnorm(mx, mx, sx)) * 1.05
  hist(r, prob = TRUE, ylim = c(0, ymax), xlim = c(xmin, xmax),
       xlab = xlab, col = "light blue", main = NULL)
  mtext("Original data", 3, font = 2, col = "navy", line = 1)
  box()
  x1 <- seq(xmin, xmax, length = 100)
  y1 <- dnorm(x1, mx, sx)
  lines(x1, y1, lwd = 1.5, lty = 3)
  
  newdat = x$model
  response = rownames(attr(x$terms, "factors"))[1]
  newcall = modifyModelCall(x, "newdat")
  
  n.obs = nrow(x$model)
  for (i in 1:n) {
    rNormal = rnorm(n.obs, sd = summary(x)$sigma)
    newdat[, response] = x$fitted.values + rNormal
    newlm = eval(parse(text = newcall))
    r = residuals(newlm)
    h = hist(r, plot = FALSE)
    mx = mean(r)
    sx = sd(r)
    rx = range(r)
    xmin <- min(rx[1], mx - 3.5 * sx, h$breaks[1])
    xmax <- max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)])
    ymax <- max(h$density, dnorm(mx, mx, sx)) * 1.05
    hist(r, prob = TRUE, ylim = c(0, ymax), xlim = c(xmin, xmax),
         xlab = xlab, col = "light blue", main = NULL)
    mtext(paste("Normal errors: sample", i), 3, font = 2,
          col = "navy", line = 1)
    box()
    x1 <- seq(xmin, xmax, length = 100)
    y1 <- dnorm(x1, mx, sx)
    lines(x1, y1, lwd = 1.5, lty = 3)
  }
}