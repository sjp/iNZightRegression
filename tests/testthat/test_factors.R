context("Factor output")

fit <- lm(Sepal.Length ~ Species, data = iris)
test_that("Factor comparisons computed correctly", {
    x <- factorComp(fit, "Species")
    expect_is(x, "inzfactorcomp")

    z <- TukeyHSD(aov(fit), "Species")
    expect_equal(x$estimate[c(1, 3:4)], as.numeric(z$Species[, 1]))
    expect_equal(x$p.value[c(1, 3:4)], as.numeric(z$Species[, 4]))
    expect_equal(x$ci[c(1, 5, 7)], as.numeric(z$Species[, 2]))
    expect_equal(x$ci[1 + c(1, 5, 7)], as.numeric(z$Species[, 3]))
})

test_that("Factor comparison matrix prints OK", {
    expect_success(print(factorComp(fit, "Species")))
})
