context("iNZightSummary")

dat <- data.frame(x = runif(100, 0, 1))
dat$y <- rbinom(100, 1, log(dat$x*2 + 1) / log(3))

test_that("Probit models supported", {
    lm.probit <- glm(y ~ x, family = binomial(link = "probit"), data = dat)
    expect_output(iNZightSummary(lm.probit))
})
