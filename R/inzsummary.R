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
    coef_matrix(x, method = method, signif.stars = signif.stars, exclude = exclude)

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
    invisible(NULL)
}

coef_matrix <- function(x, method, signif.stars, exclude) {
    z <- summary(x)

    coef <- z$coefficients
    if (any(aliased <- z$aliased)) {
        cn <- names(aliased)
        coef <- matrix(NA, length(aliased), 4, dimnames = list(cn, colnames(coef)))
        coef[!aliased, ] <- z$coefficients
    }
    coefs <- coef[,1,drop=FALSE]
    err <- coef[,2,drop=FALSE]
    tval <- coef[,3,drop=FALSE]
    pval <- coef[,4,drop=FALSE]
    ci <- confint(x)

    mat <- cbind(
        est = coefs,
        err = err,
        tval = tval,
        pval = pval,
        low = ci[,1],
        upp = ci[,2]
    )

    terms <- terms(z)

    # Now terms: should be one group of row(s) per column of `terms`
    dc <- attr(terms, "dataClasses")
    tf <- attr(terms, "factors")
    aov <- anova(x)
    coef_mat <- lapply(colnames(tf),
        function(term) {
            if (sum(tf[,term]) == 1) {
                # just a single variable
                if (dc[term] == "numeric")
                    return(list(names = term, rows = mat[term,,drop=FALSE]))
                # else is a factor; factor row + baseline + other rows from `mat`
                lvls <- x$xlevels[[term]]
                rows <- rbind(
                    c(NA, NA, NA, aov[term,"Pr(>F)"], NA, NA),
                    c(0, NA, NA, NA, NA, NA),
                    mat[paste0(term, lvls[-1]), , drop = FALSE]
                )
                return(
                    list(
                        names = c(term, paste0("  ", lvls)),
                        rows = rows
                    )
                )
            } else {
                # an interaction
                vars <- strsplit(term, ":")[[1]]
                lvls <- lapply(vars,
                    function(v) if (dc[v] == "numeric") "" else x$xlevels[[v]][-1]
                )
                names(lvls) <- vars
                labels <- lapply(vars, function(v) paste0(v, lvls[[v]]))
                labels <- do.call(expand.grid, labels)
                labels <- apply(as.matrix(labels), 1, paste, collapse = ":")
                rows <- mat[labels, , drop = FALSE]

                names <- do.call(expand.grid, lvls)
                names <- paste0("  ", apply(as.matrix(names), 1, paste, collapse = ":"))

                names <- c(term, names)
                rows <- rbind(c(NA, NA, NA, aov[term,"Pr(>F)"], NA, NA), rows)
                return(list(names = names, rows = rows))
            }
        }
    )
    names <- do.call(c, lapply(coef_mat, function(x) x$names))
    coef_mat <- do.call(rbind, lapply(coef_mat, function(x) x$rows))

    # Intercept:
    if (attr(terms, "intercept") == 1) {
        coef_mat <- rbind(mat["(Intercept)", , drop=FALSE], coef_mat)
        names <- c("Intercept", names)
    }
    rownames(coef_mat) <- names
    colnames(coef_mat) <- c("Estimate", "Std. error", "t value", "p value", "lower", "upper")

    class(coef_mat) <- "inzcoefmat"
    cat("Coefficients:\n")
    print(coef_mat, signif.stars = signif.stars)
}

print.inzcoefmat <- function(x, digits = 3, signif.stars = TRUE, ...) {
    # assume (for now) that everything is there, we just need to format the column values
    # cols 1-3,5-6 formatted normally; col 4 formatted as p-value

    if (signif.stars) {
        pval <- x[, 4]
        Signif <- symnum(pval, corr = FALSE, na = FALSE,
            cutpoints = c(0,  .001,.01,.05, .1, 1),
            symbols   =  c("***","**","*","."," "))
    }

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
    mat <- gsub("NA", "-", mat)

    mat <- cbind(mat[,1:4], c("", as.character(Signif)), mat[,5:6])

    mat <- cbind(c("", rownames(x)), mat)
    mat <- matrix(
        apply(mat, 2, function(col) format(col, justify = "left")),
        nrow = nrow(mat)
    )

    mat <- apply(mat, 1,
        function(x) paste0("   ", paste(x, collapse = "   "))
    )

    cat(mat, sep = "\n")
    if (signif.stars)
        cat("---\nSignif. codes: ", attr(Signif, "legend"), "\n")
}
