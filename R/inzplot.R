#' inzplot method
#'
#' @importFrom iNZightPlots inzplot
#' @name inzplot
#' @rdname inzplot.lm
#' @export
NULL

#' Diagnostic Plots for Regression Models
#'
#' @section Plot types:
#' There are several plot types available:
#' * residual versus fitted
#' * scale-location
#' * residual versus leverage
#' * Cook's distance
#' * normal Q-Q
#' * histogram
#'
#' @param x a regression model
#' @param which the type of plot to draw
#' @param show.bootstraps logical, if `TRUE` bootstrap smoothers will be shown
#' @param ... additional arguments
#' @param env the environment for evaluating things (e.g., bootstraps)
#' @return A ggplot object
#'
#' @md
#' @import ggplot2
#' @importFrom magrittr "%>%"
#' @author Tom Elliott
#' @export
#' @examples
#' iris_fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)
#' inzplot(iris_fit)
inzplot.lm <- function(x,
                       which = c("residual", "scale-location", "leverage", "cooks", "normal", "hist"),
                       show.bootstraps = TRUE,

                       ...,
                       env = parent.frame()
                       ) {

    d <- data.frame(r = residuals(x), yh = predict(x))

    if (show.bootstraps) {
        data <- eval(x$call$data, envir = env)
        bs.call <- x$call
        bs.call$data <- as.name("bs.data")
        bs.fits <- vector("list", 30L)
        for (i in seq_along(bs.fits)) {
            bs.data <- data %>% dplyr::slice_sample(n = nrow(data), replace = TRUE)
            bs.fits[[i]] <- eval(bs.call)
        }
    }

    p <- ggplot(d, aes_(~yh, ~r)) +
        geom_point() +
        geom_hline(yintercept = 0, lty = 3) +
        scale_x_continuous(
            "Fitted values",
            limits = range(d$yh)
        ) +
        scale_y_continuous(
            "Residuals"
        ) +
        ggtitle("Plot of residuals versus fitted values",
            subtitle = sprintf("for the model %s", utils::capture.output(x$call))
        )


    if (show.bootstraps) {
        for (i in seq_along(bs.fits)) {
            ds <- data.frame(r = residuals(bs.fits[[i]]), yh = predict(bs.fits[[i]]))
            p <- p +
                geom_smooth(
                    data = ds,
                    method = "loess",
                    formula = y ~ x,
                    colour = "lightgreen",
                    se = FALSE,
                    na.rm = TRUE
                )
        }
    }

    p <- p +
        geom_smooth(
            method = "loess",
            formula = y ~ x,
            colour = "orangered",
            se = FALSE,
            na.rm = TRUE
        )

    p
}
