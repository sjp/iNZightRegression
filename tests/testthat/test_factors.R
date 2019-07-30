context("Factor output")

fit <- lm(Sepal.Length ~ Species, data = iris)
fit2 <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)
fit3 <- 

test_that("Factor comparisons computed correctly", {
    x <- factorComp(fit, "Species")
    expect_is(x, "inzfactorcomp")

    z <- summary(multcomp::glht(fit, linfct = mcp(Species = "Tukey")))
    expect_equal(x$estimate[c(1, 3:4)], as.numeric(z$test$coefficients))
    expect_equal(x$p.value[c(1, 3:4)], as.numeric(z$test$pvalues))
    expect_equal(
        round(x$ci[c(1, 5, 7)], 3),
        round(as.numeric(confint(z)$confint[, "lwr"]), 3)
    )
    expect_equal(
        round(x$ci[1 + c(1, 5, 7)], 3),
        round(as.numeric(confint(z)$confint[, "upr"]), 3)
    )

    expect_is(factorComp(fit2, "Species"), "inzfactorcomp")
    
    d <- data.frame(
        x = rnorm(1000),
        g1 = sample(c("A", "B"), 1000, replace = TRUE),
        g2 = sample(c("first", "second"), 1000, replace = TRUE)
    )
    expect_is(factorComp(lm(x ~ g1 + g2, data = d), "g1"), "inzfactorcomp")
    expect_is(factorComp(lm(x ~ g1 + g2, data = d), "g2"), "inzfactorcomp")
})

test_that("Factor comparison matrix prints OK", {
    expect_output(print(factorComp(fit, "Species")))
})


