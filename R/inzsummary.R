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
#' @param reorder.factors if `FALSE` (default), factor order is left unchanged, if `TRUE`, the most common level is set as the baseline.
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
    if (!is.null(exclude)) {
        cat('The model has been adjusted for the',
            'following confounder(s):\n', sep = ' ')
        cat('\t')
        cat(exclude, sep = ', ')
        cat('\n\n')
    }

    ## section 2: coefficient matrix
    print(coef_matrix(x, method = method, signif.stars = signif.stars, exclude = exclude))

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

coef_matrix <- function(x, method, signif.stars, exclude) {
    z <- summary(x)
    # coef <- z$coefficients
    # if (signif.stars) {
    #     Signif <- symnum(pv, corr = FALSE, na = FALSE,
    #         cutpoints = c(0,  .001,.01,.05, .1, 1),
    #         symbols   =  c("***","**","*","."," "))
    # }

    coefs <- coefficients(x)
    err <- sqrt(diag(vcov(x)))
    tval <- coefs / err
    pval <- pt(tval, x$df, lower.tail = FALSE) * 2
    ci <- confint(x)
    mat <- cbind(
        Estimate = coefs,
        `Std. Error` = err,
        `t value` = tval,
        `p value` = pval,
        lower = ci[,1],
        upper = ci[,2]
    )

    class(mat) <- "inzcoefmat"
    return(mat)

    terms <- terms(z)

    # Intercept:
    intercept <- if (attr(terms, "intercept") == 1) coef["(Intercept)", ] else NULL

    # Now terms: should be one group of row(s) per column of `terms`
    tf <- attr(terms, "factors")
    terms <- sapply(colnames(tf),
        function(term) {

            c(estimate = NA, error = NA, tvalue = NA, pvalue = NA, NA, NA)
        }
    )
    print(intercept)
# print(t(terms))
    # rbind(intercept, t(terms))
}

print.inzcoefmat <- function(x, digits = 3, ...) {
    # assume (for now) that everything is there, we just need to format the column values
    # cols 1-3,5-6 formatted normally; col 4 formatted as p-value

    mat <- apply(x, 2, function(col) {
        format(col, digits = digits, scientific = FALSE)
    })
    mat <- matrix(mat, ncol = ncol(x))
    mat[,4] <- format.pval(x[,4], digits = digits)

    mat <- rbind(colnames(x), mat)
    mat <- matrix(
        apply(mat, 2, function(col) format(col, justify = "right")),
        nrow = nrow(mat)
    )

    mat <- cbind(c("", rownames(x)), mat)
    mat <- matrix(
        apply(mat, 2, function(col) format(col, justify = "left")),
        nrow = nrow(mat)
    )

    mat <- apply(mat, 1,
        function(x) paste0("   ", paste(x, collapse = "   "))
    )

    cat(mat, sep = "\n")
}
