bootstrapModels <- function(fit, nBootstraps = 30) {
    if (isSurvey(fit)) {
        #cat('NOTE: Bootstrapping for survey models is still in beta.\n')
        #cat('-------------------------------------------------------\n\n')
        warning('Bootstrapping for survey glms is still under development.')
    }
    ### Variables for adding bootstrap lowess lines
    nr = nrow(fit$model)
    # Call needs to remove data = ...
    modifiedCall = modifyModelCall(fit, "bootstrapSample")
    
    listOfModels = vector("list", nBootstraps)
    #i = 1
    #while (i <= nBootstraps) {
    #    bootstrapID <- sample(1:nr, replace = TRUE)#, prob = Wt)
    #    bootstrapSample <- bootstrapData(fit, bootstrapID)
    #    mod <- suppressWarnings(eval(parse(text = modifiedCall)))
    #    if (isGlm(fit)) {
    #        if (mod$conv) {
    #            listOfModels[[i]] <- mod
    #            i <- i + 1
    #        }
    #    } else {
    #        listOfModels[[i]] = mod
    #        i <- i + 1
    #    }
    #}

    bootstrapID <- replicate(nBootstraps, sample(1:nr, replace = TRUE))
    listOfModels <- invisible(lapply(1:nBootstraps,
        function(i) {
            conv <- FALSE
            while (!conv) {
                bootstrapSample <- bootstrapData(fit, bootstrapID[, i])
                mod <- suppressWarnings(eval(parse(text = modifiedCall)))
                if (isGlm(fit)) {
                    if (mod$conv) {
                        #listOfModels[[i]] <<- mod
                        conv <- TRUE
                    }
                } else {
                    #listOfModels[[i]] <<- mod
                    conv <- TRUE
                }
            }
            mod
        }))
    
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
    if ('weights' %in% names(fit$call))
        fit <- renameWeights(fit)
    out <- fit$model[id, ]
}

bootstrapData.glm <- function(fit, id) {
    ## Issue with GLMs defined as count/total ~ x

    ## (weights) will already have been renamed to total,
    ## so just convert count/total to count
    if (grepl('/', colnames(fit$model)[1])) {
      # In this case, need to do some complicated stuff ...
        mod <- fit$model

      # Get the names of the counts and totals:
        response <- strsplit(names(mod), '/')[[1]]
        total <- mod[, '(weights)']

      # Bootstrap the number of succese based on La Place ...
        bs.tot <- total#rpois(nrow(mod), total)
        mod[, response[1]] <- rbinom(nrow(mod),
                                     bs.tot,
                                     #(mod[, 1] * total + 1) / (total + 2))
                                     (mod[, 1] * total) / (total))
        mod[, response[2]] <- bs.tot
        out <- mod
    } else {
        out <- fit$model[id, ]
    }
    
    out
}

bootstrapData.svyglm <- function(fit, id) {
  # To do: account for sample design when doing bootstrap resample.
    
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
