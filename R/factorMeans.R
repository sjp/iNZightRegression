
factorMeans = function(fit) {
  varTypes = attr(fit$terms, "dataClasses")
  isFactor = varTypes %in% c("factor", "ordered")
  factorNames = names(varTypes)[isFactor]
  yvar = names(varTypes[1])
  yvals = fit$model[,yvar]
  termLabels = attr(fit$terms, "term.labels")
  interactions = termLabels[grep(":", termLabels)]
  interactingVars = strsplit(interactions, ":")
  if (any(sapply(interactingVars, length) > 2))
    stop("Interactions involving more than 2 factors are not supported")
  nonInteractions = factorNames
  for (i in seq_along(interactions))
    nonInteractions = nonInteractions[! nonInteractions %in% interactingVars[[i]]]

  res = list()
  for (i in seq_along(nonInteractions)) {
    f = nonInteractions[i]
    facvals = structure(list(fit$model[,f]), names = f)
    facmeans = aggregate(yvals, facvals, mean)
    res[[f]] = structure(facmeans$x, names = levels(facmeans[,f]))
  }
  for (i in seq_along(interactions)) {
    f = interactingVars[[i]]
    facvals = structure(as.list(fit$model[,f]), names = f)
    facmeans = aggregate(yvals, facvals, mean)
    meanNames = paste(facmeans[,f[1]], gsub(" ", "", facmeans[,f[2]]), sep = ".")
    res[[interactions[i]]] = structure(facmeans$x, names = meanNames)
  }
  res
}



## Adjusted means for the levels of a factor: Sets values of continuous
## variables equal to their means and sets other factors equal to their
## baseline values

adjustedMeans = function(fit) {
  varTypes = attr(fit$terms, "dataClasses")[-1]
  isFactor = varTypes %in% c("factor", "ordered")
  factorNames = names(varTypes)[isFactor]
  numericNames = names(varTypes)[!isFactor]
  numericMeans = sapply(fit$model[,numericNames, drop = FALSE], mean)

  termLabels = attr(fit$terms, "term.labels")
  interactions = termLabels[grep(":", termLabels)]
  interactingVars = strsplit(interactions, ":")
  if (any(sapply(interactingVars, length) > 2))
    stop("Interactions involving more than 2 factors are not supported")
  nonInteractions = factorNames
  for (i in seq_along(interactions))
    nonInteractions = nonInteractions[! nonInteractions %in% interactingVars[[i]]]


  res = list()
  for (i in seq_along(nonInteractions)) {
    f = nonInteractions[i]
    facLevels = levels(fit$model[,f])
    otherVars = names(varTypes)[names(varTypes) != f]
    otherVarsList = list()
    for (i in seq_along(otherVars)) {
      val = ifelse(otherVars[i] %in% factorNames,
                   levels(fit$model[,otherVars[i]])[1],
                   numericMeans[otherVars[i]])
      otherVarsList[[otherVars[i]]] = val
    }
    adjMean = structure(numeric(length(facLevels)), names = facLevels)
    for (flev in facLevels) {
      otherVarsList[[f]] = flev
      adjMean[flev] = predict(fit, data.frame(otherVarsList))
    }
    res[[f]] = adjMean
  }

  for (i in seq_along(interactions)) {
    f = interactingVars[[i]]
    facCombinations = expand.grid(levels(fit$model[,f[1]]), levels(fit$model[,f[2]]))

    ###################

    ## More to do here!

    ###################
 
  }


  res
}

