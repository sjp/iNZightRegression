## This script contains tests to ensure iNZightRegression functions
## work for linear models.

library(iNZightRegression)

fit1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length +
           Petal.Width + Species, data = iris)
iNZightSummary(fit1, digits = 2)
iNZightSummary(fit1, method = 'bootstrap')
plotlm6(fit1, which = 7)
partialResPlot(fit1, 'Sepal.Width')
allPartialResPlots(fit1)
histogramArray(fit1)
qqplotArray(fit1)
fit1.moe <- moecalc(fit1, 'Species')
multicomp(fit1.moe)
plot(fit1.moe)
summary(fit1.moe)

fit2 <- lm(Volume ~ Height + Girth, data = trees)
trees <- within(trees, wt <- runif(nrow(trees), 1, 5))
fit3 <- lm(Volume ~ Height + Girth, data = trees,
           weights = wt)
iNZightSummary(fit2)
iNZightSummary(fit3)
iNZightSummary(fit3, method = 'bootstrap')
plotlm6(fit2, which = 7)
plotlm6(fit3, which = 7)
