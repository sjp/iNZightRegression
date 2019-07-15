context("iNZightSummary")

dat <- data.frame(x = runif(100, 0, 1))
dat$y <- rbinom(100, 1, log(dat$x*2 + 1) / log(3))

lm <- lm(y ~ x, data = dat)

test_that("Probit models supported", {
    lm.logit <- glm(y ~ x, family = binomial, data = dat)
    smry <- capture.output(iNZightSummary(lm.logit))
    expect_equal(sum(grepl("using the logit link", smry)), 1)
    lm.probit <- glm(y ~ x, family = binomial(link = "probit"), data = dat)
    smry <- capture.output(iNZightSummary(lm.probit))
    expect_equal(sum(grepl("using the probit link", smry)), 1)
    expect_equal(sum(grepl("Coefficients:", smry)), 1)
    expect_equal(sum(grepl("AIC", smry)), 1)
    expect_equal(sum(grepl("Fisher Scoring iterations", smry)), 1)
})

