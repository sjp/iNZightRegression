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

dat <- data.frame(x = runif(100, 0, 1))
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

