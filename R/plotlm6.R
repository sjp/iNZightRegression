

plotlm6 = function (x, which = 1:6,
    panel = if (add.smooth) panel.smooth else points, sub.caption = NULL,
    main = "", ask = prod(par("mfcol")) < length(which) && dev.interactive(),
    ..., id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75,
    qqline = TRUE, cook.levels = c(0.5, 1), add.smooth = getOption("add.smooth"),
    label.pos = c(4, 2), cex.caption = 1)
{
    smColour = "orangered"      # colour of data loess line
    bsmColour = "lightgreen"    # colour of bootstrap loess lines

    dropInf <- function(x, h) {
        if (any(isInf <- h >= 1)) {
            warning("Not plotting observations with leverage one:\n  ",
                paste(which(isInf), collapse = ", "), call. = FALSE)
            x[isInf] <- NaN
        }
        x
    }
    if (!inherits(x, "lm"))
        stop("use only with \"lm\" objects")

    if (!is.numeric(which) || any(which < 1) || any(which > 7))
        stop("'which' must be in 1:7")

    # Are we only showing the summary plot?
    if (7 %in% which) {
        onlyShowAll <- TRUE
        which <- 1:6
    } else {
        onlyShowAll <- FALSE
    }

    isGlm <- inherits(x, "glm")
    show <- rep(FALSE, 6)
    show[which] <- TRUE
    r <- residuals(x)
    yh <- predict(x)
    w <- weights(x)
    if (!is.null(w)) {
        wind <- w != 0
        r <- r[wind]
        yh <- yh[wind]
        w <- w[wind]
        labels.id <- labels.id[wind]
    }
    n <- length(r)
    if (any(show[2:6])) {
        s <- if (inherits(x, "rlm"))
            x$s
        else if (isGlm)
            sqrt(summary(x)$dispersion)
        else sqrt(deviance(x)/df.residual(x))
        hii <- lm.influence(x, do.coef = FALSE)$hat
        if (any(show[3L:6L])) {
            cook <- if (isGlm)
                cooks.distance(x)
            else cooks.distance(x, sd = s, res = r)
        }
    }
    if (any(show[c(2,5)])) {
        ylab23 <- if (isGlm)
            "Std. deviance resid."
        else "Standardized residuals"
        r.w <- if (is.null(w))
            r
        else sqrt(w) * r
        rs <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
    }
    if (show[3]) {
        r.hat <- range(hii, na.rm = TRUE)
        isConst.hat <- all(r.hat == 0) || diff(r.hat) < 1e-10 *
            mean(hii, na.rm = TRUE)
    }
    if (any(show[1:2]))
        l.fit <- if (isGlm)
            "Predicted values"
        else "Fitted values"
    if (is.null(id.n))
        id.n <- 0
    else {
        id.n <- as.integer(id.n)
        if (id.n < 0 || id.n > n)
            stop(gettextf("'id.n' must be in {1,..,%d}", n),
                domain = NA)
    }
    if (id.n > 0) {
        if (is.null(labels.id))
            labels.id <- paste(1:n)
        iid <- 1:id.n
        show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
        if (any(show[c(2,5)]))
            show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]
        text.id <- function(x, y, ind, adj.x = TRUE) {
            labpos <- if (adj.x)
                label.pos[1 + as.numeric(x > mean(range(x)))]
            else 3
            text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE,
                pos = labpos, offset = 0.25)
        }
    }

    caption = list("Residuals vs Fitted", "Scale-Location",
                   "Residuals vs Leverage","Cook's distance",
                   "Normal Q-Q", "Histogram")
    getCaption <- function(k) {
        if (length(caption) < k)
            NA_character_
        else
            as.graphicsAnnot(caption[[k]])
    }
    if (is.null(sub.caption)) {
        cal <- x$call
        if (!is.na(m.f <- match("formula", names(cal)))) {
            cal <- cal[c(1, m.f)]
            names(cal)[2] <- ""
        }
        cc <- deparse(cal, 80)
        nc <- nchar(cc[1], "c")
        abbr <- length(cc) > 1 || nc > 75
        sub.caption <- if (abbr)
            paste(substr(cc[1], 1, min(75, nc)), "...")
        else cc[1]
    }

    one.fig <- length(which) == 1 || onlyShowAll
    if (ask) {
        oask <- devAskNewPage(! one.fig)
        on.exit(devAskNewPage(oask))
    } else {
        oask <- devAskNewPage(FALSE)
        on.exit(devAskNewPage(oask))
    }
    
    bsModels = bootstrapModels(x)
    nBootstraps = length(bsModels)
    ## New bootstrapped values (bs suffix stands for bootstrap)
    rbs = rsbs = yhbs = sbs = hiibs = vector("list", nBootstraps)
    for (i in 1:nBootstraps) {
        rbs[[i]] = residuals(bsModels[[i]])
        yhbs[[i]] = fitted(bsModels[[i]])
        sbs[[i]] = if (inherits(x, "rlm"))
                     bsModels[[i]]$s
                   else if (isGlm)
                     sqrt(summary(bsModels[[i]])$dispersion)
                   else sqrt(deviance(bsModels[[i]])/df.residual(bsModels[[i]]))
        if (any(show[2:6]))
            hiibs[[i]] <- lm.influence(bsModels[[i]], do.coef = FALSE)$hat
        if (any(show[2:3]))
            rsbs[[i]] <- dropInf(rbs[[i]]/(sbs[[i]] * sqrt(1 - hiibs[[i]])), hiibs[[i]])
    }

    # If we want to show all of the plots, assume "all" is the seventh plot
    showAllPlots = all(show)

    # Ensure par is not globally modified
    origpar = par(mfrow = c(1, 1))
    on.exit(par(origpar))

    for (plotNum in 1:7) {
        if (showAllPlots & plotNum == 1) {
            # We are showing all plots
            showPlot = rep(TRUE, 6)
            par(mfrow = c(2, 3))
        } else {
            # If we are only showing the summary plot
            # skip any specific plot
            if (onlyShowAll)
                next

            # Just show a specific plot
            showPlot = rep(FALSE, 6)
            # See if we need to plot the "current" plot
            showPlot[plotNum - 1] = show[plotNum - 1]
            par(mfrow = c(1, 1))
        }

        if (showPlot[1]) {
            ylim <- range(r, na.rm = TRUE)
            if (id.n > 0)
                ylim <- extendrange(r = ylim, f = 0.08)
            dev.hold()
            plot(yh, r, xlab = l.fit, ylab = "Residuals", main = main,
                ylim = ylim, ...)

            ### Draw bootstrap sample loess lines
            for (i in 1:nBootstraps) {
                bsm = loess(rbs[[i]] ~ yhbs[[i]])
                bsmOrd = order(bsm$x)
                lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd], col = bsmColour)
            }
            ### Draw loess line for original data set
            sm = loess(r ~ yh)
            smOrd = order(sm$x)
            lines(sm$x[smOrd], sm$fitted[smOrd], col = smColour, lwd = 2)

            if (one.fig)
                title(sub = sub.caption, ...)
            mtext(getCaption(1), 3, 0.25, cex = cex.caption)
            if (id.n > 0) {
                y.id <- r[show.r]
                y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
                text.id(yh[show.r], y.id, show.r)
            }
            abline(h = 0, lty = 3, col = "gray")
            dev.flush()
        }
        if (showPlot[2]) {
            sqrtabsr <- sqrt(abs(rs))
            ylim <- c(0, max(sqrtabsr, na.rm = TRUE))
            yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))))
            yhn0 <- if (is.null(w))
                yh
            else yh[w != 0]
            dev.hold()
            plot(yhn0, sqrtabsr, xlab = l.fit, ylab = yl, main = main,
                ylim = ylim, ...)

            ### Draw bootstrap sample loess lines
            for (i in 1:nBootstraps) {
                bsm = loess(sqrt(abs(rsbs[[i]])) ~ yhbs[[i]])
                bsmOrd = order(bsm$x)
                lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd], col = bsmColour)
            }
            ### Draw loess line for original data set
            sm = loess(sqrtabsr ~ yhn0)
            smOrd = order(sm$x)
            lines(sm$x[smOrd], sm$fitted[smOrd], col = smColour, lwd = 2)

            if (one.fig)
                title(sub = sub.caption, ...)
            mtext(getCaption(2), 3, 0.25, cex = cex.caption)
            if (id.n > 0)
                text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
            dev.flush()
        }
        if (showPlot[3]) {
            ylab5 <- if (isGlm)
                "Std. Pearson resid."
            else "Standardized residuals"
            r.w <- residuals(x, "pearson")
            if (!is.null(w))
                r.w <- r.w[wind]
            rsp <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
            
            ## bootstrap rsp
            rspbs = vector("list", nBootstraps)
            for (i in 1:nBootstraps) {
                pearsonResid = residuals(bsModels[[i]], "pearson")
                rspbs[[i]] = dropInf(pearsonResid / (sbs[[i]] * sqrt(1 - hiibs[[i]])), hiibs[[i]])
            }
            
            ylim <- range(rsp, na.rm = TRUE)
            if (id.n > 0) {
                ylim <- extendrange(r = ylim, f = 0.08)
                show.rsp <- order(-cook)[iid]
            }
            do.plot <- TRUE

            xx <- hii
            xx[xx >= 1] <- NA
            dev.hold()
            plot(xx, rsp, xlim = c(0, max(xx, na.rm = TRUE)),
                ylim = ylim, main = main, xlab = "Leverage",
                ylab = ylab5, ...)
            ## Bootstrap smooths
            for (i in 1:nBootstraps) {
              xxbs = hiibs[[i]]
              xxbs[xxbs >= 1] = NA
              bsm = loess(rspbs[[i]] ~ xxbs)
              bsmOrd = order(bsm$x)
              lines(bsm$x[bsmOrd], bsm$fitted[bsmOrd], col = bsmColour)
            }
            ## Original data smooth
            sm = loess(rsp ~ xx)
            smOrd = order(sm$x)
            lines(sm$x[smOrd], sm$fitted[smOrd], col = smColour, lwd = 2)
            
            if (one.fig)
                title(sub = sub.caption, ...)
            if (length(cook.levels)) {
                p <- length(coef(x))
                usr <- par("usr")
                hh <- seq.int(min(r.hat[1L], r.hat[2L]/100),
                  usr[2L], length.out = 101)
                for (crit in cook.levels) {
                  cl.h <- sqrt(crit * p * (1 - hh)/hh)
                  lines(hh, cl.h, lty = 2, col = 2)
                  lines(hh, -cl.h, lty = 2, col = 2)
                }
                legend("bottomleft", legend = "Cook's distance",
                  lty = 2, col = 2, bty = "n")
                xmax <- min(0.99, usr[2L])
                ymult <- sqrt(p * (1 - xmax)/xmax)
                aty <- c(-sqrt(rev(cook.levels)) * ymult, sqrt(cook.levels) *
                  ymult)
                axis(4, at = aty, labels = paste(c(rev(cook.levels),
                  cook.levels)), mgp = c(0.25, 0.25, 0), las = 2,
                  tck = 0, cex.axis = cex.id, col.axis = 2)
            }
            dev.flush()

            if (do.plot) {
                mtext(getCaption(3), 3, 0.25, cex = cex.caption)
                if (id.n > 0) {
                    y.id <- rsp[show.rsp]
                    y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
                    text.id(xx[show.rsp], y.id, show.rsp)
                }
            }
        }
        if (showPlot[4]) {
            ## cooks distance
            cdx <- cooks.distance(x)
            show.mx <- order(-cdx)[1:3]
            dev.hold()
            plot(1:length(cdx), cdx, type = "h", main = main,
                 xlab = "observation number", ylab = "cook's distance")
            mtext(getCaption(4), 3, 0.25, cex = cex.caption)
            text(show.mx, cdx[show.mx] + 0.4 * 0.75 * strheight(" "), show.mx)
            dev.flush()
        }
        if (showPlot[5]) {
            ylim <- range(rs, na.rm = TRUE)
            ylim[2] <- ylim[2] + diff(ylim) * 0.075
            dev.hold()
            qq <- normCheck(rs, main = main, ylab = ylab23, ylim = ylim, ...)
            if (one.fig)
                title(sub = sub.caption, ...)
            mtext(getCaption(5), 3, 0.25, cex = cex.caption)
            if (id.n > 0)
                text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
            dev.flush()
        }
        if (showPlot[6]) {
            ## Histogram
            h <- hist(r, plot = FALSE)
            xlab <- "Residuals"
            mx <- mean(r)
            sx <- sd(r)
            rx <- range(r)
            xmin <- min(rx[1], mx - 3.5 * sx, h$breaks[1])
            xmax <- max(rx[2], mx + 3.5 * sx, h$breaks[length(h$breaks)])
            ymax <- max(h$density, dnorm(mx, mx, sx)) * 1.05
            dev.hold()
            hist(r, prob = TRUE, ylim = c(0, ymax), xlim = c(xmin, xmax),
                 xlab = xlab, col = "light blue",
                 main = main)
            mtext(getCaption(6), 3, 0.25, cex = cex.caption)
            box()
            x1 <- seq(xmin, xmax, length = 100)
            y1 <- dnorm(x1, mx, sx)
            lines(x1, y1, lwd = 1.5, lty = 3)
            dev.flush()
        }
    }

    if (!one.fig && par("oma")[3] >= 1)
        mtext(sub.caption, outer = TRUE, cex = 1.25)

    invisible()
}

# A modified version of the s20x::normcheck default method
normCheck = function (x, col = NULL, shapiroWilk = TRUE, plot = TRUE, ...) {
    # Note: this is a bit nasty, consider rewriting
    # If 'main' has been passed in, use it, otherwise leave as empty
    moreargs <- list(...)
    qqp =
      if ("main" %in% names(moreargs))
        qqnorm(x, plot.it = plot, ...)
      else
        qqnorm(x, plot.it = plot, main = "", ...)

    if (plot) {
      mx <- mean(x)
      sx <- sd(x)
      dev.hold()
      abline(c(mx, sx), col = "gray50")
      if (shapiroWilk) {
        stest <- shapiro.test(x)
        txt <- paste("Shapiro-Wilk normality test", "\n", "W = ",
                     round(stest$statistic, 4), "\n", "P-value = ",
                     round(stest$p.value, 3), sep = "")
        text(sort(qqp$x)[2], 0.99 * max(qqp$y, na.rm = TRUE), txt,
             adj = c(0, 1))
      }
      dev.flush()
    }
    qqp
}
