context("Model comparison")

f1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
f2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

test_that("Single model output is correct", {
    expect_equal(compare_models(f1)[, "df"], 3)
    expect_equal(compare_models(f1)[, "AIC"], AIC(f1))
    expect_equal(compare_models(f1)[, "BIC"], BIC(f1))
})

test_that("Multiple model comparisons are correct", {
    expect_equal(compare_models(f1, f2)[, "df"], c(f1 = 3, f2 = 5))
    expect_equal(compare_models(f1, f2)[, "AIC"], c(f1 = AIC(f1), f2 = AIC(f2)))
    expect_equal(compare_models(f1, f2)[, "BIC"], c(f1 = BIC(f1), f2 = BIC(f2)))
})

