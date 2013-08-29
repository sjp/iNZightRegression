setwd('~/iNZight/iNZightRegression/')               # mac

mydata = read.csv('../regression/testing/course.csv')

for (file in list.files('R/'))
  source(paste('R/', file, sep = ''))

mod <- list(6)
mod[[1]] <- lm(Exam ~ Assign + Test + Gender + Attend, data = mydata)
mod[[2]] <- glm(Attend ~ Exam + Gender + Degree, family = 'binomial', data = mydata)
x <- runif(20, 0, 100)
f <- as.factor(rep(LETTERS[1:5], length(x) / 5))
counts <- rpois(length(x), 4 + x * 0.24 + c(A = 2, B = 5, C = 18, D = 1, E = 20)[f])
mod[[3]] <- glm(counts ~ x + f, family = poisson)
x <- runif(30, 0, 50)
p <- exp(-5 + 0.2 * x) / (1 + exp(-5 + 0.2 * x))
y <- ifelse(runif(length(x)) < p, 1, 0)
mod[[4]] <- glm(y ~ x, family = 'binomial')

library(survey)
data(api)
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
mod[[5]] <- svyglm(api00~ell+meals+mobility, design=dstrat)


data(nhanes)
nhanes <- within(nhanes, sex <-
                 as.factor(ifelse(RIAGENDR == 1, 'male', 'female')))
nhanes$race <- factor(nhanes$race)
levels(nhanes$race) <-
    c('hispanic', 'non-hispanic white', 'non-hispanic black', 'other')
design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR,
                    nest=TRUE, data=nhanes[!is.na(nhanes$HI_CHOL),])
mod[[6]] <- svyglm(HI_CHOL ~ race + agecat + sex, design = design,
               family = 'quasibinomial')

i <- 5  # change this
model <- mod[[i]]
model

## bootstrap.R
(bootstrapModels(model, 2))
modifyModelCall(model, 'bootdata')
head(bootstrapData(model, sample(1:nrow(model$model), replace = TRUE)))

## bootstrapCoef.R
summary(reorderFactors(mydata)) # orders them from most to least ...

## factorMeans.R
factorMeans(model)
adjustedMeans(model)

## general.R
isGlm(model)
isSurvey(model)

## hisogramArray.R
histogramArray(model)

## moecalc.R
moecalc(model, 'Gender') -> moe; moe  ## will need to change this depending on the model ..
print(moe)
summary(moe)
print(summary(moe))
plot(moe)

## moe-multicomp.R
multicomp(moe)
print(multicomp(moe))

## partialResidual.R
allPartialResPlots(model)

## plotlm6.R
plotlm6(model, which = 7) ## normCheck() is used in this

## qqplotArray.R
qqplotArray(model)

## summary.R
iNZightSummary(model)
iNZightSummary(model, method = 'bootstrap')

