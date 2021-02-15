#' inzsummary method
#'
#' @importFrom iNZightPlots inzsummary
#' @name inzsummary
#' @rdname inzsummary.lm
#' @export
NULL

#' Informative Summary Information for Regression Models
#'
#' Displays a summary of a regression model fitted using `lm`, `glm`, or `survey::svyglm`.
#'
#' @param x an object of class \code{"lm"}, \code{"glm"} or \code{"svyglm"}, usually the result of a call to the corresponding function.
#' @param method the type of inference to perform: "standard" uses Normal theory, "bootstrap" uses bootstrap resampling.
#' @param reorder.factors if `FALSE` (default), factor order is left unchanged, if `TRUE`, the most common level is set as the baseline. Alternatively can be a character value of a level to use as the baseline.
#' @param digits the number of significant digits to display in results
#' @param symbolic.cor logical, if \code{TRUE}, print the correlations in a symbolic form (see \code{\link{symnum}}), rather than as numbers.
#' @param signif.stars logical, if \code{TRUE}, \sQuote{significance stars} are printed for each coefficient.
#' @param exclude a character vector of names of variables to be excluded from the summary output (i.e., confounding variables).
#' @param exponentiate.ci logical, if \code{TRUE}, the exponential of the confidence intervals will be printed if appropriate (log/logit link or log transformed response)
#' @param env the environment for evaluating things (e.g., bootstraps)
#'
#' @param ... additional arguments, ignored.
#'
#' @return Summary information is printed to the console.
#'
#' @author Tom Elliott
#' @md
#' @export
inzsummary.lm <- function(x,
                          method = c("standard", "bootstrap"),
                          reorder.factors = FALSE,
                          digits = max(3, getOption("digits") - 3),
                          symbolic.cor = x$symbolic.cor,
                          signif.stars= getOption("show.signif.stars"),
                          exclude = NULL,
                          exponentiate.ci = FALSE,
                          ...,
                          env = parent.frame()) {

    method <- match.arg(method)


    ## section 1: model information

    # response:
    cat(glue::glue("Linear Model for '{attr(x$model, 'names')[1]}'"), "\n\n")

    # confounding variables/adjusted for:


    ## section 2: coefficient matrix
    # print(coef_matrix(x, signif.stars = signif.stars, exclude = exclude))

    # names -> factors split into levels; overall factor p-value
    # estimates
    # standard errors
    # test statistic (t-value)
    # p-value
    # p-value significance stars
    # confidence interval of estimate (lower, upper)

    # things to worry about:
    # * intercept (yes/no)
    # * numeric vars - one row
    # * categorical vars - one row per level + 1
    # * interactions - n*m + 1 (either can be numeric)

    # significance codes (stars)

    ## section 3: errors, df, R-squared, etc (model dependent)

}
