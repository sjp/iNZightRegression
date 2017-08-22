isGlm <- function(fit)
    inherits(fit, 'glm')

isSurvey <- function(fit)
    inherits(fit, 'svyglm')



errorPlot <- function(x = "Unable to draw plot") {
    ##plot.new()
    ##plot.window(0:1, 0:1)
    u <- par()$usr
    rect(u[1], u[3], u[2], u[4], col = "white")
    text(mean(u[1:2]), mean(u[3:4]), x, cex = 1.5)
}
tryOrErrorPlot <- function(expr, x) {
    tryCatch(expr, error = function(e) {
        errorPlot(x)
        print(e)
    })
}


##' A modified `poly()` function that allows for missing values.
##'
##' Credit goes to whoever posted this online first (google search if you must find it!)
##' @title Polynomial Matrix
##' @param x variable to convert to matrix
##' @param degree degree of polynomial
##' @param coefs pass to poly() function
##' @param raw pass to poly() function
##' @param ... more arguments for the poly() function
##' @return a matrix, with NAs in the missing rows
##' @author Tom Elliott
##' @export
Poly <- function(x, degree = 1, coefs = NULL, raw = FALSE, ...) {
    notNA<-!is.na(x)
    answer<-poly(x[notNA], degree=degree, coefs=coefs, raw=raw, ...)
    THEMATRIX<-matrix(NA, nrow=length(x), ncol=degree)
    THEMATRIX[notNA,]<-answer
    attributes(THEMATRIX)[c('degree', 'coefs', 'class')]<- attributes(answer)[c('degree', 'coefs', 'class')]
    THEMATRIX
}
