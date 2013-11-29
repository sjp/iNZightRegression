bootstrapModels <- function(fit, nBootstraps = 30) {
    ### Variables for adding bootstrap lowess lines
    nr = nrow(fit$model)
    # Call needs to remove data = ...
    modifiedCall = modifyModelCall(fit, "bootstrapSample")

    # rename the weights column in the model:
    if ('(weights)' %in% names(fit$model)) {
        Wt <- fit$weights
        fit <- renameWeights(fit)
    } else {
        Wt <- rep(1, nrow(fit$model))
    }
    
    listOfModels = vector("list", nBootstraps)
    i = 1
    while (i <= nBootstraps) {
        bootstrapID <- sample(1:nr, replace = TRUE, prob = Wt)
        bootstrapSample <- bootstrapData(fit, bootstrapID)
        mod <- suppressWarnings(eval(parse(text = modifiedCall)))
        if (isGlm(fit)) {
            if (mod$conv) {
                listOfModels[[i]] <- mod
                i <- i + 1
            }
        } else {
            listOfModels[[i]] = mod
            i <- i + 1
        }
    }

    invisible(listOfModels)
}

modifyModelCall <- function(fit, newDataName) {
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)

    ## First piece is the function name:
    fn <- callValues[1]

    ## lm and glm have data part that needs to be changed,
    ## while survey objects have a design argument:
    dataK <- ifelse(isSurvey(fit), 'design', 'data')

    ## The other parts need to all be kept:
    o <- !callNames %in% c('', 'design', 'data')
    other <- paste(callNames[o], ' = ', callValues[o], ', ', sep = '',
                   collapse = '')

    ## Paste everything together:
    modifiedCall <- paste(fn, '(', other, dataK, ' = ',
                          newDataName, ')', sep = '')

    modifiedCall
}

bootstrapData <- function(fit, id)
    UseMethod("bootstrapData")

bootstrapData.lm <- function(fit, id) {
    out <- fit$model[id, ]
}

bootstrapData.glm <- function(fit, id) {
    ## Issue with GLMs defined as count/total ~ x

    ## (weights) will already have been renamed to total,
    ## so just convert count/total to count
    if (grepl('/', colnames(fit$model)[1])) {
      # In this case, need to do some complicated stuff ...
        stop('Beta development version cannot *yet* support binomial bootstrapping.')
    }
    
    out <- fit$model[id, ]
    out
}

bootstrapData.svyglm <- function(fit, id) {
  # To do: account for sample design when doing bootstrap resample.
    
    cat('NOTE: Bootstrapping for survey models is still in beta.\n')
    cat('-------------------------------------------------------\n\n')
    
    ## Survey glm: bootstrap the data in the design,
    ## then recreate the design object and return.
    data <- fit$survey.design$variables  # a data.frame
    newData <- data[id, ]
    
    designCall <- fit$survey.design$call
    desNames <- names(designCall)
    desVals <- as.character(designCall)
    xargs <- !desNames %in% 'data'
    
    o <- !desNames %in% c('', 'data')
    args <- paste(desNames[o], ' = ', desVals[o], ', ',
                  sep = '', collapse = '')
    newCall <- paste(desVals[1], '(', args,
                     'data = newData)', sep = '')
    
    out <- eval(parse(text = newCall))
    
    out
}



renameWeights <- function(fit) {
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    wtName <- callValues[callNames == 'weights']

    names(fit$model)[names(fit$model) == '(weights)'] <- wtName
    fit
}
