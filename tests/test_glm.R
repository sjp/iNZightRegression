## This script contains tests to ensure iNZightRegression functions
## work for general linear models.

library(iNZightRegression)

age <- runif(100, 20, 80)
sex <- factor(sample(c('male', 'female'), 100, TRUE))
eta <- -8 + 0.13 * age + ifelse(sex == 'male', 0, 2)
win <- rbinom(100, 1, exp(eta) / (1 + exp(eta)))
fake <- data.frame(age, sex, win)

fit1 <- glm(win ~ age + sex, family = binomial, data = fake)
iNZightSummary(fit1, digits = 2)
iNZightSummary(fit1, method = 'bootstrap')
plotlm6(fit1, which = 7)
partialResPlot(fit1, 'age')
allPartialResPlots(fit1)
histogramArray(fit1)
qqplotArray(fit1)
fit1.moe <- moecalc(fit1, 'sex')
multicomp(fit1.moe)
plot(fit1.moe)
summary(fit1.moe)



dat <- as.data.frame(Titanic)
dat$Survived <- ifelse(dat$Survived == 'Yes', 1, 0)
data <- dat[dat$Survived == 0, ]
colnames(data)[5] <- "Died"
data$Survived <- dat$Freq[dat$Survived == 1]
data$Total <- rowSums(data[, 4:5])
titanic <- data
fit2 <- glm(Survived / Total ~ Class + Sex + Age,
            weights = Total, family = binomial, data = titanic)

iNZightSummary(fit2)
iNZightSummary(fit2, method = 'bootstrap')

plotlm6(fit2, which = 7)
plotlm6(fit2, which = 7, showBootstraps = TRUE)


