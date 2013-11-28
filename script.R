setwd('~/iNZight/iNZightRegression/sandbox')
library(devtools)
unload('../')
load_all('../')

#source('../R/ses.moecalc.R')
#package.skeleton()

data(teach.df)
teach.df$method <- as.factor(teach.df$method)
fit <- lm(lang ~ IQ + method, data = teach.df)
iNZightSummary(fit, digits = 3)

data(body.df)
body.df$ow <- with(body.df,
                   ifelse(bodyim %in% c('slight.uw', 'right'),
                          0, 1))
fit.glm <- glm(ow ~ weight + height + age + ethnicity, data = body.df)
iNZightSummary(fit.glm, reorder.factors = TRUE,
               digits = 3, method = 'bootstrap')


library(iNZightRegression)
?histogramArray
