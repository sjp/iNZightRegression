partialResPlot = function(fit, varname, showBootstraps = nrow(fit$model) >= 30) {

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

    if (showBootstraps) {
        ## Plot bootstrap smooths
        bsm = bootstrapModels(fit)
        for (j in seq_along(bsm)) {
            bsm_r = bsm[[j]]$residuals
            bsm_Bi = bsm[[j]]$coefficients[varname]
            bsm_Xi = bsm[[j]]$model[, varname]
            bsm_sm = loess(bsm_r + bsm_Bi * bsm_Xi ~ bsm_Xi)
            bsm_smOrd = order(bsm_sm$x)
            bsm_smx = bsm_sm$x[bsm_smOrd]
            bsm_smy = bsm_sm$fitted[bsm_smOrd]
            lines(bsm_smx, bsm_smy, col = "lightgreen")
        }
    }

    ## Plot linear trend we are modelling
    xlims = range(Xi)
    lines(xlims, Bi * xlims, lty = "dashed", col = "blue", lwd = 2)

    ## Plot original data smooth
    sm = loess(Yi ~ Xi)
    smOrd = order(sm$x)
    smx = sm$x[smOrd]
    smy = sm$fitted[smOrd]
    lines(smx, smy, col= "orangered", lwd = 2)
}

allPartialResPlots = function(fit) {
    promptSetting = devAskNewPage(TRUE)
    xVarterms = attr(fit$terms, "term.labels")
    xVarnames = xVarterms[ ! grepl(":", xVarterms)]
    xVartypes = attr(fit$terms, "dataClasses")
    for (v in xVarnames)
        if (! xVartypes[v] %in% c("factor", "ordered"))
            partialResPlot(fit, v)
   devAskNewPage(promptSetting)
}
