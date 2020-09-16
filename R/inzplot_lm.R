#' Diagnostic Plots for Regression Models
#'
#' @section Plot types:
#' There are several plot types available:
#' * residual versus fitted
#' * scale-location
#' * residual versus leverage
#' * Cook's distance
#' * normal Q-Q
#' * histogram array
#'
#' @param x a regression model
#' @param which the type of plot to draw
#' @param show.bootstraps logical, if `TRUE` bootstrap smoothers will be shown
#' @param label.id integer for the number of extreme points to label (with row id)
#' @param col.smooth the colour of smoothers
#' @param col.bs the colour of bootstrap (smoothers)
#' @param cook.levels levels of the Cook's distance at which to draw contours.
#' @param col.cook the colour of Cook's distance contours
#' @param ... additional arguments
#' @param env the environment for evaluating things (e.g., bootstraps)
#' @return A ggplot object
#'
#' @md
#' @import ggplot2
#' @import ggtext
#' @import ggrepel
#' @importFrom magrittr "%>%"
#' @author Tom Elliott
#' @export
#' @examples
#' iris_fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
#' inzplot(iris_fit)
inzplot.lm <- function(x,
                       which = c(
                           "residual",
                           "scale",
                           "leverage",
                           "cooks",
                           "normal",
                           "hist"
                        ),
                       show.bootstraps = TRUE,
                       label.id = 3L,
                       col.smooth = "orangered",
                       col.bs = "lightgreen",
                       cook.levels = c(0.5, 1),
                       col.cook = "pink",
                       ...,
                       env = parent.frame()
                       ) {

    # instead, just loop over `which` and patchwork:: them together
    which <- match.arg(which)

    p <- switch(which,
        "residual" = ,
        "scale" = ,
        "leverage" = .inzplot_lm_scatter(x, which, show.bootstraps,
            label.id, col.smooth, col.bs, cook.levels, col.cook,
            ...,
            env = env
        )
    )

    grDevices::dev.hold()
    on.exit(grDevices::dev.flush())
    suppressWarnings(print(p))

    invisible(p)
}

.inzplot_lm_scatter <- function(x, which, show.bootstraps, label.id,
                                col.smooth, col.bs,
                                cook.levels, col.cook,
                                ...,
                                env) {
    HEX_THRESHOLD <- 1e5

    d_fun <- function(x, which, label.id = 0L, is.bs = FALSE) {
        # point labels
        if (label.id > 0L)
            iid <- seq_len(label.id)

        labs <- character(nrow(x$model))
        switch(which,
            "residual" = {
                r <- residuals(x)
                if (label.id > 0L) {
                    l <- sort.list(abs(r), decreasing = TRUE)[iid]
                    labs[l] <- names(r)[l]
                }
                data.frame(x = predict(x), y = r, lab = labs)
            },
            "scale" = {
                r <- residuals(x)
                s <- sqrt(deviance(x) / df.residual(x))
                hii <- lm.influence(x, do.coef = FALSE)$hat
                rs <- dropInf(r / (s * sqrt(1 - hii)), hii)
                if (label.id > 0L) {
                    l <- sort.list(abs(r), decreasing = TRUE)[iid]
                    labs[l] <- names(r)[l]
                }
                data.frame(x = predict(x), y = sqrt(abs(rs)), lab = labs)
            },
            "leverage" = {
                rp <- residuals(x, "pearson")
                hii <- lm.influence(x, do.coef = FALSE)$hat
                s <- sqrt(deviance(x) / df.residual(x))
                rsp <- dropInf(rp / (s * sqrt(1 - hii)), hii)
                xx <- ifelse(hii > 1, NA, hii)
                cook <- cooks.distance(x, sd = s, res = residuals(x))
                if (label.id > 0L) {
                    l <- order(-cook)[iid]
                    labs[l] <- names(rp)[l]
                }
                d <- data.frame(x = xx, y = rsp, lab = labs)

                if (!is.bs && length(cook.levels)) {
                    r.hat <- range(hii, na.rm = TRUE)
                    attr(d, "r.hat") <- r.hat
                }
                d
            }
        )
    }
    d <- d_fun(x, which, label.id = label.id)

    if (show.bootstraps)
        bs.fits <- generate_bootstraps(x, env)

    title <- switch(which,
        "residual" = "Residuals vs Fitted Values",
        "scale" = "Scale-location plot",
        "leverage" = "Residuals vs Leverage"
    )
    if (show.bootstraps) {
        title <- sprintf(
            "**%s** with <span style='color:%s'>fitted</span> and <span style='color:%s'>bootstrap</span> smoothers",
            title,
            col.smooth,
            col.bs
        )
    } else {
        title <- sprintf(
            "**%s** with fitted smoother",
            title
        )
    }
    if (which == "leverage" && !is.null(attr(d, "r.hat"))) {
        title <- sprintf("%s, and <span style='color:%s'>Cook's contours</span>",
            title,
            col.cook
        )
    }

    USE_HEX <- nrow(d) > HEX_THRESHOLD
    XL <- extendrange(range(d$x, na.rm = TRUE))
    YL <- extendrange(range(d$y, na.rm = TRUE), f = 0.08)

    p <- ggplot(d, aes_(~x, ~y))

    yax2 <- NULL
    if (which == "leverage" && !is.null(attr(d, "r.hat"))) {
        px <- length(coef(x))
        r.hat <- attr(d, "r.hat")
        hh <- seq.int(min(r.hat[1L], r.hat[2L] / 100), XL[2],
            length.out = 101)

        yax2 <- numeric()
        for (crit in cook.levels) {
            cl.h <- sqrt(crit * px * (1 - hh) / hh)
            yax2 <- c(yax2,
                structure(cl.h[length(cl.h)] * c(1, -1), .Names = c(crit, crit))
            )
            dx <- data.frame(x = hh, y = cl.h)
            p <- p +
                geom_path(lty = 2, col = col.cook,
                    data = dx, na.rm = TRUE) +
                geom_path(aes(y = -y), lty = 2, col = col.cook,
                    data = dx, na.rm = TRUE) +
                geom_vline(xintercept = XL[2])
        }
        yax2 <- yax2[dplyr::between(yax2, YL[1], YL[2])]
    }

    if (USE_HEX) {
        p <- p + geom_hex() +
            scale_fill_gradient(low = "gray80", high = "black") +
            labs(fill = "Count")
    } else {
        p <- p + geom_point()
    }

    p <- p +
        geom_hline(yintercept = 0, lty = 3) +
        scale_x_continuous(
            switch(which,
                "residual" = "Fitted values",
                "scale" = "Fitted values",
                "leverage" = "Leverage"
            )
        ) +
        scale_y_continuous(
            switch(which,
                "residual" = "Residuals",
                "scale" = expression(sqrt(abs("Standardized residuals"))),
                "leverage" = "Residuals"
            ),
            sec.axis =
                if (!is.null(yax2) && length(yax2)) {
                    sec_axis(
                        trans = ~.,
                        name = NULL,
                        breaks = yax2,
                        labels = names(yax2)
                    )
                } else waiver()
        ) +
        ggtitle(
            title,
            subtitle = sprintf("Linear model: %s",
                utils::capture.output(x$call$formula)
            )
        ) +
        theme_classic() +
        theme(
            plot.title.position = "plot",
            plot.title = element_markdown()
        ) +
        coord_cartesian(xlim = XL, ylim = YL, expand = FALSE)

    if (label.id > 0L && !is.null(d$lab)) {
        p <- p +
            geom_text_repel(aes_(label = ~lab),
                data = d[d$lab != "", ]
            )
    }

    if (show.bootstraps) {
        for (i in seq_along(bs.fits)) {
            ds <- d_fun(bs.fits[[i]], which, is.bs = TRUE)
            p <- p +
                geom_smooth(
                    data = ds,
                    method = "loess",
                    formula = y ~ x,
                    colour = col.bs,
                    se = FALSE,
                    na.rm = TRUE
                )
        }
    }

    p <- p +
        geom_smooth(
            method = "loess",
            formula = y ~ x,
            colour = col.smooth,
            se = FALSE,
            na.rm = TRUE
        )

    p
}

dropInf <- function(x, h) {
    if (any(isInf <- h >= 1)) {
        #warning("Not plotting observations with leverage one:\n  ",
        #        paste(which(isInf), collapse = ", "), call. = FALSE)
        x[isInf] <- NaN
    }
    x
}
