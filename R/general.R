isGlm <- function(fit)
    inherits(fit, 'glm')

isSurvey <- function(fit)
    inherits(fit, 'svyglm')
