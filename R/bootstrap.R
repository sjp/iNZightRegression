bootstrapModels <- function(fit, nBootstraps = 30, env = parent.frame()) {
    if (isSurvey(fit))
        warning('Bootstrapping for survey glms is still under development.')

  # Variables for adding bootstrap lowess lines
    # nr = nrow(fit$model)
    # modifiedCall <- modifyModelCall(fit)
    # bootstrapID <- replicate(nBootstraps, sample(1:nr, replace = TRUE))
    # listOfModels <- invisible(lapply(1:nBootstraps,
    #     function(i) {
    #         conv <- FALSE
    #         while (!conv) {
    #             bootstrapSample <- bootstrapData(fit, bootstrapID[, i])
    #             mod <- suppressWarnings(eval(modifiedCall))
    #             if (isGlm(fit)) {
    #                 if (mod$conv)
    #                     conv <- TRUE
    #             } else
    #             conv <- TRUE
    #         }
    #         mod
    #     }))

    bs.fits <- replicate(nBootstraps, {
        conv <- FALSE
        while (!conv) {
            bsfit <- suppressWarnings({
                call <- update(fit, subset = sample(nrow(fit$model), replace = TRUE), evaluate = FALSE)
                eval(call, envir = env)
            })
            conv <- !isGlm(fit) || bsfit$conv
        }
        bsfit
    }, simplify = FALSE)

    invisible(bs.fits)
}

bootstrapData <- function(fit, id)
    UseMethod("bootstrapData")

bootstrapData.lm <- function(fit, id) {
    return(fit$model[id, ])
    ## Try just use the data set in the R session ...
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    dataName <-callValues[callNames == 'data']
    bsData <- eval(parse(text = dataName))[id, ]
    bsData
}

bootstrapData.glm <- function(fit, id) {
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    dataName <-callValues[callNames == 'data']
    bsData <- eval(parse(text = dataName))

    if (grepl('/', colnames(fit$model)[1])) {
      # In this case, resample the number of successes
        mod <- fit$model
        response <- strsplit(names(mod), '/')[[1]]
        y <- bsData[, response[1]]
        n <- bsData[, response[2]]
        p <- ifelse(n == 0, 0, y / n)

        bsData[, response[1]] <- rbinom(nrow(bsData), n, p)
    } else {
      # otherwise, simply resample the data
        bsData <- bsData[id, ]
    }

    bsData
}

bootstrapData.svyglm <- function(fit, id) {
  # To do: account for sample design when doing bootstrap resample.

  # Returns a bootstrapped survey design object

  # First, get the survey design name, then the dataset name:
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    designName <- callValues[callNames == 'design']

    des <- eval(parse(text = designName))
    descall <- des$call
    descallValues <- as.character(descall)
    descallNames <- names(descall)
    dataName <- descallValues[descallNames == 'data']

  # Rebuild design call with new data:
    bsData <- eval(parse(text = dataName))[id, ]

    o <- !descallNames %in% c('', 'data')
    other <- paste(descallNames[o], ' = ', descallValues[o], ', ',
                   sep = '', collapse = '')

    newcall <- paste('svydesign(', other, 'data = bsData)', sep = '')
    bsDesign <- eval(parse(text = newcall))
}



modifyModelCall <- function(fit) {
    if (isSurvey(fit)) {
        modifiedCall <- update(fit, . ~ .,
                               design = eval(parse(text = "bootstrapSample")),
                               evaluate = FALSE)
    } else {
        modifiedCall <- update(fit, . ~ .,
                               data = eval(parse(text = "bootstrapSample")),
                               evaluate = FALSE)
    }

    modifiedCall
}


modelDataName <- function(fit) {
    call <- fit$call
    callValues <- as.character(call)
    callNames <- names(call)
    dataName <-callValues[callNames == 'data']
    dataName
}
