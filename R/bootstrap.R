bootstrapModels <- function(fit, nBootstraps = 30) {
    ### Variables for adding bootstrap lowess lines
    nr = nrow(fit$model)
    # Call needs to remove data = ...
    modifiedCall = modifyModelCall(fit, "bootstrapSample")

    listOfModels = vector("list", nBootstraps)
    i = 1
    while (i <= nBootstraps) {
        bootstrapID <- sample(1:nr, replace = TRUE)
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

bootstrapData <-
    function(fit, id) {
        if (isSurvey(fit)) {
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

        } else {
            ## Simply bootstrap the data.

            out <- fit$model[id, ]
        }

     ## Issue with GLMs defined as count/total ~ x
        if ('(weights)' %in% names(out)) {
            match <-grepl('/', names(out)) 
            if (any(match)) {
                p <- unlist(strsplit(names(out)[match], '/'))
                out[p[1]] <- out[, 1] * out[, '(weights)']
                out[p[2]] <- out[, '(weights)']
            } else {
                stop('I\'m not sure what you are doing ...')
            }
        }
        out
    }
