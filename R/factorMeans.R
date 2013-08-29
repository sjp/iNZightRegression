
factorMeans = function(fit) {
  varTypes = attr(fit$terms, "dataClasses")
  isFactor = varTypes %in% c("factor", "ordered")
  factorNames = names(varTypes)[isFactor]
  if (any(isFactor)) {
      yvar = names(varTypes[1])
      factorNames = factorNames[factorNames != yvar]
      yvals = if (isGlm(fit)) fit$y else fit$model[,yvar]
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
      return(res)
  }
  else cat('There are no factors in this model.\n')
}



## Adjusted means for the levels of a factor: Sets values of continuous
## variables equal to their means and sets other factors equal to their
## baseline values

adjustedMeans = function(fit) {
  varTypes = attr(fit$terms, "dataClasses")[-1]
  isFactor = varTypes %in% c("factor", "ordered")
  if (any(isFactor)) {
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
          nonInteractions =
              nonInteractions[! nonInteractions %in% interactingVars[[i]]]

      res = list()
      for (i in seq_along(nonInteractions)) {
          f = nonInteractions[i]
          facLevels = levels(fit$model[,f])
          xVars = names(varTypes)[names(varTypes) != f]
          xVarsList = list()
          for (j in seq_along(xVars)) {
              if (xVars[j] %in% factorNames) {
                  val = factor(levels(fit$model[,xVars[j]])[1])
                  levels(val) <- levels(fit$model[, xVars[[j]]])
              } else {
                  val = numericMeans[xVars[j]]
              }
              xVarsList[[xVars[j]]] = val
          }
          adjMean = structure(numeric(length(facLevels)), names = facLevels)
          for (flev in facLevels) {
              xVarsList[[f]] = factor(flev, levels = facLevels)
              adjMean[flev] = predict(fit, data.frame(xVarsList))
          }
          res[[f]] = adjMean
      }

      for (i in seq_along(interactions)) {
          f = interactingVars[[i]]
          facCombinations = expand.grid(levels(fit$model[,f[1]]),
              levels(fit$model[,f[2]]))
          nCombinations = nrow(facCombinations)
          combinationNames = paste(facCombinations[,1], facCombinations[,2],
              sep = ".")
          xVars = names(varTypes)[!names(varTypes) %in% f]
          xVarsList = list()
          for (i in seq_along(xVars)) {
              val = ifelse(xVars[i] %in% factorNames,
                  levels(fit$model[,xVars[i]])[1],
                  numericMeans[xVars[i]])
              xVarsList[[xVars[i]]] = val
          }

          adjMean = structure(numeric(nCombinations), names = combinationNames)
          for (i in 1:nCombinations) {
              for (j in 1:2)
                  xVarsList[[f[j]]] = facCombinations[i, j]
              adjMean[combinationNames[i]] = predict(fit, data.frame(xVarsList))
          }
          ord = order(names(adjMean))
          adjMean = adjMean[names(adjMean)[ord]]

          res[[paste(f, collapse = ":")]] = adjMean
      }

      ## If GLM, then inverse-link the results
      res <- if (isGlm(fit)) lapply(res, fit$family$linkinv) else res
      return(res)
  }
  else {
      cat('There are no factors in this model.\n')
  }
}

