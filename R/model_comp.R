#' Compare regression models using AIC, BIC, etc
#' 
#' Obtain a quick model comparison matrix for a selection of models
#' @param x a regression model (lm, glm, svyglm, ...)
#' @param ... other models
#' @return an matrix object
#' @author Tom Elliott
#' @export
compare_models <- function(x, ...) {
    # x should be a model
    xclass <- class(x)
    model.list <- c(list(x), list(...))
    if (length(model.list) > 1) {
        if (any(!sapply(model.list, function(z) all(class(z) == xclass))))
            stop("Models must be of the same type")
    }

    AIC <- AIC(x, ...)
    BIC <- BIC(x, ...)

    if (length(model.list) > 1) {
        df <- AIC$df
        AIC <- AIC$AIC
        BIC <- BIC$BIC
    }
    else df <- attr(stats4::logLik(x), "df")

    mat <- cbind(df, AIC, BIC)
    structure(mat,
        .Dimnames = list(
            Model = as.character(match.call())[-1],
            colnames(mat)
        )
    )
}
