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
#' @param col.smooth the colour of smoothers
#' @param col.bs the colour of bootstrap (smoothers)
#' @param ... additional arguments
#' @param env the environment for evaluating things (e.g., bootstraps)
#' @return A ggplot object
#'
#' @md
#' @import ggplot2
#' @import ggtext
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
        "leverage" = .inzplot_lm_scatter(x, which, show.bootstraps, col.smooth, col.bs, ..., env = env)
    )

    grDevices::dev.hold()
    on.exit(grDevices::dev.flush())
    print(p)

    invisible(p)
}

.inzplot_lm_scatter <- function(x, which, show.bootstraps, col.smooth, col.bs, ..., env) {
    d_fun <- function(x, which) {
        switch(which,
            "residual" =
                data.frame(x = predict(x), y = residuals(x)),
            "scale" = {
                r <- residuals(x)
                s <- sqrt(deviance(x) / df.residual(x))
                hii <- lm.influence(x, do.coef = FALSE)$hat
                rs <- dropInf(r / (s * sqrt(1 - hii)), hii)
                data.frame(x = predict(x), y = sqrt(abs(rs)))
            },
            "leverage" = {
                r.w <- residuals(x, "pearson")
                hii <- lm.influence(x, do.coef = FALSE)$hat
                s <- sqrt(deviance(x) / df.residual(x))
                rsp <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
                xx <- ifelse(hii > 1, NA, hii)
                data.frame(x = xx, y = rsp)
            }
        )
    }
    d <- d_fun(x, which)

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
            subtitle = sprintf("%s", utils::capture.output(x$call))
        ) +
        theme(
            plot.title.position = "plot",
            plot.title = element_markdown()
        )


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
