#' Compare regression models using AIC, BIC, etc
#' 
#' Obtain a quick model comparison matrix for a selection of models
#' @param x a regression model (lm, glm, svyglm, ...)
#' @param ... other models
#' @return an `inzmodelcomp` object
#' @author Tom Elliott
#' @export
compare_models <- function(x, ...) {
    # x should be a model
    xclass <- class(x)
    model.list <- c(list(x), list(...))
    if (length(model.list) > 1) {
        if (any(!sapply(model.list[[-1]], function(z) all(class(z) == xclass))))
            stop("Models must be of the same type")
    }

    model.list
}

#' @export
print.inzmodelcomp <- function(x, ...) {

}
