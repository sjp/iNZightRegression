context("Model comparison")

f1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
f2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

# compare_models(f1)
# compare_models(f1, f2)

