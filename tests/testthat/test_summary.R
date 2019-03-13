context("iNZightSummary")

dat <- data.frame(x = runif(100, 0, 1))
dat$y <- rbinom(100, 1, log(dat$x*2 + 1) / log(3))

test_that("Probit models supported", {
    lm.probit <- glm(y ~ x, family = binomial(link = "probit"), data = dat)
    smry <- capture.output(iNZightSummary(lm.probit))
    expect_equal("Coefficients:", smry[1])
    expect_equal(sum(grepl("AIC", smry)), 1)
    expect_equal(sum(grepl("Fisher Scoring iterations", smry)), 1)
})
