context("Bootstrapping regression models")

test_that("Simple bootstrap with dataframe works", {
    fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)

    expect_silent(bs <- bootstrapModels(fit, 10))
    expect_is(bs, "list")
    expect_equal(length(bs), 10)
    expect_true(all(sapply(bs, class) == "lm"))
    expect_false(identical(coef(fit), coef(bs[[1]])))

    fit <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
    expect_silent(bs <- bootstrapModels(fit, 10))
    expect_false(identical(coef(fit), coef(bs[[1]])))
})

test_that("Bootstrapping without database works", {
    x <- iris$Sepal.Width
    y <- iris$Sepal.Length
    fit <- lm(y ~ x)

    expect_silent(bs <- bootstrapModels(fit, 10))
    expect_false(identical(coef(fit), coef(bs[[1]])))

    z <- iris$Species
    fit <- lm(y ~ x + z)
    expect_silent(bs <- bootstrapModels(fit, 10))
    expect_false(identical(coef(fit), coef(bs[[1]])))
})

test_that("Bootstrapping GLMs works", {
    utils::data(anorexia, package = "MASS")
    fit <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
        family = gaussian, data = anorexia)
    expect_silent(bs <- bootstrapModels(fit, 10))
    expect_false(identical(coef(fit), coef(bs[[1]])))
    
})

require(survey)
test_that("Bootstrapping survey GLMs works", {
    data(api)
    dclus2 <- svydesign(id = ~dnum + snum, weights = ~pw, data = apiclus2)
    s <- svyglm(api00 ~ ell + meals + mobility, design = dclus2)
    expect_warning(bootstrapModels(s, 10), "not yet ready")
})
