library(iNZightRegression)
library(s20x)
data(course.df)

names(course.df)

fit <- lm(Exam ~ Degree + Gender + Attend + Assign + Test + MC + Colour +
          Stage1 + Years.Since + Repeat, data = course.df)
iNZightSummary(fit)

step.fit <- step(fit, direction = 'both')

library(R330)
allpossregs(fit)
