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

test_that("GLM models work", {
    iris$big <- as.integer(iris$Sepal.Length > median(iris$Sepal.Length))
    g1 <- glm(big ~ Sepal.Width, data = iris, family = binomial)
    g2 <- glm(big ~ Sepal.Width + Species, data = iris, family = binomial)
    expect_equal(
        compare_models(g1, g2),
        structure(
            cbind(
                c(2, 4),
                c(AIC(g1), AIC(g2)),
                c(BIC(g1), BIC(g2))
            ),
            .Dimnames = list(Model = c("g1", "g2"), c("df", "AIC", "BIC"))
        )
    )
})

library(survey)
data(api)
test_that("Survey models work", {
    dclus2 <- svydesign(id = ~dnum + snum, weights = ~pw, data = apiclus2)
    s1 <- svyglm(api00 ~ ell, design = dclus2)
    s2 <- svyglm(api00 ~ ell + meals, design = dclus2)
    s3 <- svyglm(api00 ~ ell + meals + mobility, design = dclus2)
    expect_equal(
        compare_models(s1, s2, s3),
        AIC(s1, s2, s3)
    )
})

