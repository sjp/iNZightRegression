## Test iNZightRegression functions:

library(iNZightRegression)
source('../iNZightTools/R/fitModel.R')
source('../iNZightTools/R/history.R')

e <- list(hist = character())

dat <- as.data.frame(Titanic)
dat$Survived <- ifelse(dat$Survived == 'Yes', 1, 0)
data <- dat[dat$Survived == 0, ]
colnames(data)[5] <- "Died"
data$Survived <- dat$Freq[dat$Survived == 1]
data$Total <- rowSums(data[, 4:5])

titanic <- data

summary(glm(Survived / Total ~ Class + Sex + Age, family = binomial,
            data = titanic, weights = Total) -> fit)

fit.model("Survived / Total", "Class + Age + Sex",
          family = "binomial", data = "titanic",
          weights = "Total") -> fit
cHist("iNZightSummary(fit)")

cHist("temp <- titanic")
cHist("temp$Sex <- relevel(titanic$Sex, ref = 'Female')")

fit.model("Survived / Total", "Class + Age + Sex",
          family = "binomial", data = "temp",
          weights = "Total") -> fit.relevel
cHist("iNZightSummary(fit.relevel)")

cHist("plotlm6(fit, which = 1, showBootstraps = TRUE)")
plotlm6(fit, which = 1:6)


library(survey)
data(api)

fit.model("api00", "ell + meals + mobility",
          data = "apistrat",
          svydes = c('id = ~1', 'strata = ~stype',
              'weights = ~pw', 'fpc = ~fpc')) -> fit2
iNZightSummary(fit2)

plotlm6(fit2)
