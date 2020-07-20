context("Model plot methods")

test_that("Linear regression model plots", {
    fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
    inzplot(fit)
})
