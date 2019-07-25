context("Factor output")

fit <- lm(Sepal.Length ~ Species, data = iris)

# test_that("Factor comparisons", {
    
# })

# summary(f1, contrasts = )

# fit <- lm(Sepal.Length ~ Sepal.Width + Species, 
#     data = iris,
#     contrasts = list(
#         Species = rbind(
#             c(0,0,-1),
#             c(1,0,0),
#             c(0,1,1)
#             # c(1, 0,0,0),
#             # c( 1,  0, 0),
#             # c( 0,  1,  1)
#         )
#     )
# )
# summary(fit)
# summary(f2)
