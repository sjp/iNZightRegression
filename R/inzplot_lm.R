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
                       ...,
                       env = parent.frame()
                       ) {

    # instead, just loop over `which` and patchwork:: them together
    which <- match.arg(which)

    p <- switch(which,
        "residual" = ,
        "scale" = ,
        "leverage" = .inzplot_lm_scatter(x, which, show.bootstraps, label.id, col.smooth, col.bs, ..., env = env)
    )

    grDevices::dev.hold()
    on.exit(grDevices::dev.flush())
    print(p)

    invisible(p)
}

.inzplot_lm_scatter <- function(x, which, show.bootstraps, label.id, col.smooth, col.bs, ..., env) {
    d_fun <- function(x, which, label.id = 0L) {
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
                data.frame(x = xx, y = rsp, lab = labs)
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

    p <- ggplot(d, aes_(~x, ~y)) +
        geom_point() +
        geom_hline(yintercept = 0, lty = 3) +
        scale_x_continuous(
            switch(which,
                "residual" = "Fitted values",
                "scale" = "Fitted values",
                "leverage" = "Leverage"
            ),
            limits = range(d$x)
        ) +
        scale_y_continuous(
            switch(which,
                "residual" = "Residuals",
                "scale" = expression(sqrt(abs("Standardized residuals"))),
                "leverage" = "Residuals"
            )
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
        )

    if (label.id > 0L && !is.null(d$lab)) {
        p <- p +
            geom_text_repel(aes_(label = ~lab),
                data = d[d$lab != "", ]
            )
    }

    if (show.bootstraps) {
        for (i in seq_along(bs.fits)) {
            ds <- d_fun(bs.fits[[i]], which)
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
