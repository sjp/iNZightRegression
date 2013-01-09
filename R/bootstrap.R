bootstrapModels = function(fit, nBootstraps = 30) {
    ### Variables for adding bootstrap lowess lines
    nr = nrow(fit$model)
    # Call needs to remove data = ...
    modifiedCall = modifyModelCall(fit, "bootstrapSample")
    
    listOfModels = vector("list", nBootstraps)
    for (i in 1:nBootstraps) {
        bootstrapSample = fit$model[sample(1:nr, replace = TRUE), ]
        listOfModels[[i]] = eval(parse(text = modifiedCall))
    }
    
    invisible(listOfModels)
}

modifyModelCall = function(fit, newDataName) {
  callParts = as.character(fit$call)
  callFormula = callParts[grepl("~", callParts)]
  modifiedCall = paste(callParts[1], "(", callFormula, ", data = ", newDataName, ")", sep = "")
  modifiedCall
}