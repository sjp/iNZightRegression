test <- data.frame(y = runif(10, 0, 10),
		   x = runif(10, 0, 100),
		   f = sample(c('a', 'b'), 10, TRUE))
model_1 <- lm(y ~ x + f, data = test)
iNZightSummary(model_1)

test2 <- test
test2$f <- relevel(test2$f, 'b')
model_2 <- lm(y ~ x + f, data = test2)
iNZightSummary(model_2)
