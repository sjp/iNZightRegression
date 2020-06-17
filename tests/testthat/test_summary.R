context("iNZightSummary")
options(width = 100)

fit <- lm(Sepal.Length ~ Sepal.Width + Species + Petal.Length, data = iris)
test_that("Confidence limits included in summary output", {
    o <- capture.output(iNZightSummary(fit))
    cind <- grep("^Coefficients:", o)
    expect_equal(
        scan(text = gsub("%", "", o[cind+1]), what = character(), quiet = TRUE),
        c("Estimate", "Std.", "Error", "t", "value", "p-value", "2.5", "97.5")
    )
})

dat <- data.frame(x = runif(100, 0, 1), stringsAsFactors = TRUE)
dat$y <- rbinom(100, 1, log(dat$x*2 + 1) / log(3))
test_that("Probit models supported", {
    fit.logit <- glm(y ~ x, family = binomial, data = dat)
    smry <- capture.output(iNZightSummary(fit.logit))
    expect_equal(sum(grepl("using the logit link", smry)), 1)
    fit.probit <- glm(y ~ x, family = binomial(link = "probit"), data = dat)
    smry <- capture.output(iNZightSummary(fit.probit))
    expect_equal(sum(grepl("using the probit link", smry)), 1)
    expect_equal(sum(grepl("Coefficients:", smry)), 1)
    expect_equal(sum(grepl("AIC", smry)), 1)
    expect_equal(sum(grepl("Fisher Scoring iterations", smry)), 1)
})

test_that("Interactions are handled", {
    fit1 <- lm(uptake ~ Type:Treatment, data = CO2)
    fit2 <- lm(uptake ~ conc + Type:Treatment, data = CO2)

    smry1 <- capture.output(iNZightSummary(fit1))
    smry2 <- capture.output(iNZightSummary(fit2))
    expect_match(smry1, "^Type:Treatment", all = FALSE)
    expect_match(smry2, "^Type:Treatment", all = FALSE)
})

test_that("Confounding variables are handled appropriately", {
    o <- capture.output(iNZightSummary(fit, exclude = "Species"))
    expect_match(
        o,
        "The model has been adjusted for the following confounder\\(s\\)",
        all = FALSE
    )
})

dat$y.pois <- rpois(100, 10)
test_that("Exponentiated estimates are provided where appropriate", {
    ## Logistic regression - odds ratios
    fit.logit <- glm(y ~ x, family = binomial, data = dat)
    smry <- capture.output(iNZightSummary(fit.logit))
    expect_match(smry, "Odds Ratio", all = FALSE)
    
    ## Log-transformed response
    fit.log <- lm(log(Sepal.Length) ~ Sepal.Width + Species + Petal.Length, data = iris)
    smry2 <- capture.output(iNZightSummary(fit.log))
    expect_match(smry2, "Estimate \\(exp\\)", all = FALSE)
    
    ## log link
    fit.pois <- glm(y ~ x, family = poisson, data = dat)
    smry3 <- capture.output(iNZightSummary(fit.pois))
    expect_match(smry3, "Estimate \\(exp\\)", all = FALSE)
})
