context("Model plot methods")

skip_if_offline()
cas <- iNZightTools::smart_read("https://www.stat.auckland.ac.nz/~wild/data/FutureLearn/Census%20at%20School-500.csv")
fit <- lm(height ~ armspan + gender + age + travel, data = cas)

# dev.new()
# dev.set(dev.next())
# plotlm6(fit, which = 6)

test_that("Linear regression model plots - residual vs fitted", {
    expect_silent(p <- inzplot(fit, which = "residual"))
    expect_is(p, "gg")
    expect_match(p$labels$title, "Residuals vs Fitted")
})

test_that("Linear regression model plots - scale-location", {
    expect_is(inzplot(fit, which = "scale"), "gg")
})

test_that("Linear regression model plots - residuals vs leverage", {
    expect_is(inzplot(fit, which = "leverage"), "gg")
})

test_that("Linear regression model plots - Cook's distance", {
    expect_is(inzplot(fit, which = "cooks"), "gg")
})

test_that("Linear regression model plots - Normal Q-Q", {
    expect_is(inzplot(fit, which = "normal"), "gg")
})

test_that("Linear regression model plots - Histogram", {
    expect_is(inzplot(fit, which = "hist"), "gg")
})

test_that("Linear regression model plots - summary grid", {
    expect_is(inzplot(fit), "patchwork")
})
