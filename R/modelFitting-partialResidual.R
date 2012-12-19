partialResPlot = function(fit, varname) {

    xVarterms = attr(fit$terms, "term.labels")
    xVarnames = xVarterms[ ! grepl(":", xVarterms)]
    if (! varname %in% xVarnames)
        stop(paste(varname, "is not an explanatory variable in the given model"))
    xVartypes = attr(fit$terms, "dataClasses")
    varType = xVartypes[varname]
    if (varType %in% c("factor", "ordered"))
        stop(paste(varname, "is a factor variable"))

    yname = dimnames(attr(fit$terms, "factors"))[[1]][1]
    r = fit$residuals
    Bi = fit$coefficients[varname]
    Xi = fit$model[, varname]
    Yi = r + Bi*Xi

    ## Plot the data points
    plot(Xi, Yi, xlab = varname,
         ylab = "Y - estimated effects of other variables",
         main = paste("Partial residual plot for", varname)) 
    
    ## Plot bootstrap smooths
    bsm = bootstrapModels(fit)
    for (j in seq_along(bsm)) {
        bsm_r = bsm[[j]]$residuals
        bsm_Bi = bsm[[j]]$coefficients[varname]
        bsm_Xi = bsm[[j]]$model[, varname]
        bsm_sm = lowess(bsm_Xi, bsm_r + bsm_Bi * bsm_Xi)
        lines(bsm_sm$x, bsm_sm$y, col = "lightgreen")
    }
        
    ## Plot linear trend we are modelling
    xlims = c(min(Xi), max(Xi))    
    lines(xlims, Bi * xlims, lty = "dashed", col = "blue", lwd = 2)
    
    ## Plot original data smooth
    sm = lowess(Xi, Yi)
    lines(sm$x, sm$y, col = "red", lwd = 2)
}

allPartialResPlots = function(fit) {
    promptSetting = devAskNewPage(TRUE)
    xVarterms = attr(fit$terms, "term.labels")
    xVarnames = xVarterms[ ! grepl(":", xVarterms)]
    xVartypes = attr(fit$terms, "dataClasses")
    for (v in xVarnames)
        if (! xVartypes[v] %in% c("factor", "ordered")) partialResPlot(fit, v)
   devAskNewPage(promptSetting)
}
