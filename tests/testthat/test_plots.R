context("Model plots")

dat <- data.frame(x = runif(100, 0, 1))
dat$y <- rnorm(100, 20 + dat$x * 5, 5)


test_that("Plot works for basic lm", {
    lm0 <- lm(y ~ 1, data = dat)
    expect_silent(plotlm6(lm0, which = 1))
    expect_silent(plotlm6(lm0, which = 3))
    expect_silent(plotlm6(lm0, which = 4))
    expect_silent(plotlm6(lm0, which = 4))
    expect_silent(plotlm6(lm0, which = 5))
    expect_silent(plotlm6(lm0, which = 6))

    lm1 <- lm(y ~ x, data = dat)
    expect_silent(plotlm6(lm1, which = 2))
    expect_silent(plotlm6(lm1, which = 3))
    expect_silent(plotlm6(lm1, which = 3))
    expect_silent(plotlm6(lm1, which = 4))
    expect_silent(plotlm6(lm1, which = 5))
    expect_silent(plotlm6(lm1, which = 6))
})

dat <- data.frame(x = runif(100, 0, 1))
dat$y <- rbinom(100, 1, log(dat$x*2 + 1) / log(3))


test_that("Plot works for basic glm", {
    glm0 <- glm(y ~ 1, data = dat)
    expect_silent(plotlm6(glm0, which = 1))
    expect_silent(plotlm6(glm0, which = 2))
    expect_silent(plotlm6(glm0, which = 3))
    expect_silent(plotlm6(glm0, which = 4))
    expect_silent(plotlm6(glm0, which = 5))
    expect_silent(plotlm6(glm0, which = 6))

    glm1 <- glm(y ~ x, data = dat)
    expect_silent(plotlm6(glm1, which = 1))
    expect_silent(plotlm6(glm1, which = 2))
    expect_silent(plotlm6(glm1, which = 3))
    expect_silent(plotlm6(glm1, which = 4))
    expect_silent(plotlm6(glm1, which = 5))
    expect_silent(plotlm6(glm1, which = 6))
})


unlink("Rplots.pdf")
