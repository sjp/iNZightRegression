dat <- as.data.frame(Titanic)
dat$Survived <- ifelse(dat$Survived == 'Yes', 1, 0)
data <- dat[dat$Survived == 0, ]
colnames(data)[5] <- "Died"
data$Survived <- dat$Freq[dat$Survived == 1]
data$Total <- rowSums(data[, 4:5])
titanic <- data
fit <- glm(Survived / Total ~ Class + Sex + Age,
           weights = Total, family = binomial, data = titanic)


## Bootstrapping GLM
mod <- fit$model
response <- strsplit(names(mod), '/')[[1]]
total <- mod[, '(weights)']
mod[, response[1]] <- rbinom(nrow(mod),
                             total,
                             (mod[, 1] * total + 1) / (total + 2))
mod[, response[2]] <- total
fit$model <- mod

fit.bs <- glm(Survived / Total ~ Class + Sex + Age,
              weights = Total, family = binomial, data = fit$model)

summary(fit)
summary(fit.bs)
