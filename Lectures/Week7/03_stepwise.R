## Forward and Backward Stepwise Selection

library(ISLR2)
Hitters <- na.omit(Hitters)

library(leaps)

# Perform best subset, forward, and backward selection

regfit_best_subset <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, nbest = 1)

regfit_fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19,
                         nbest = 1, method = "forward")

regfit_bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19,
                         nbest = 1, method = "backward")

regfit_fwd_sum <- summary(regfit_fwd)
regfit_bwd_sum <- summary(regfit_bwd)

################ Forward Selection ################

names(regfit_fwd_sum)

n <- dim(Hitters)[1]
p <- rowSums(regfit_fwd_sum$which)   # number of predictors + intercept

adjr2 <- regfit_fwd_sum$adjr2
cp <- regfit_fwd_sum$cp
rss <- regfit_fwd_sum$rss

AIC <- n * log(rss / n) + 2 * p
BIC <- n * log(rss / n) + p * log(n)

plot(AIC, type = "b", main = "Forward Selection - AIC")
plot(BIC, type = "b", main = "Forward Selection - BIC")
plot(cp, type = "b", main = "Forward Selection - Cp")
plot(adjr2, type = "b", main = "Forward Selection - Adjusted R^2")

################ Backward Selection ################


p <- rowSums(regfit_bwd_sum$which)   # number of predictors + intercept

adjr2 <- regfit_bwd_sum$adjr2
cp <- regfit_bwd_sum$cp
rss <- regfit_bwd_sum$rss

AIC <- n * log(rss / n) + 2 * p
BIC <- n * log(rss / n) + p * log(n)

plot(AIC, type = "b", main = "Backward Selection - AIC")
plot(BIC, type = "b", main = "Backward Selection - BIC")
plot(cp, type = "b", main = "Backward Selection - Cp")
plot(adjr2, type = "b", main = "Backward Selection - Adjusted R^2")
