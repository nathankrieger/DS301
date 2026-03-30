

# a

n = 100
X1 = seq(0,10,length.out = n)
X2 = runif(n)
error = rnorm(n,0,1)
Y = 2 + 3*X1 + 5*log(X2) + error



data = data.frame(Y, X1, X2)
x <- model.matrix(Y ~ X1 + X2, data = data)[, -1]

ridge_model <- glmnet(x, Y, alpha = 0, lambda = 2)

# b
B <- 1000

# Store bootstrap estimates of the intercept
beta0_boot <- numeric(B)
beta1_boot <- numeric(B)

for (b in 1:B) {
  boot_index <- sample(1:n, size = n, replace = TRUE)
  boot_sample <- data[boot_index, ]
  
  fit_boot <- lm(Y ~ X1 + X2, data = boot_sample)
  beta0_boot[b] <- fit_boot$coef[1]
  beta1_boot[b] <- fit_boot$coef[2]
}
# Bootstrap estimate of the standard error of beta_0
mean(beta0_boot)
mean(beta1_boot)


# c - e

#TODO


# 2

X3 = rnorm(n)
X4 = rnorm(n)

data <- data.frame(Y, X1, X2, X3, X4)
# a 
regfit_best_subset <- regsubsets(Y ~ ., data = data, nvmax = 4, nbest = 1)

regfit_sum <- summary(regfit_best_subset)

p <- rowSums(regfit_sum$which)   # number of predictors + intercept

adjr2 <- regfit_sum$adjr2
cp <- regfit_sum$cp
rss <- regfit_sum$rss

AIC <- n * log(rss / n) + 2 * p
BIC <- n * log(rss / n) + p * log(n)

plot(AIC, type = "b", main = "Forward Selection - AIC")
plot(BIC, type = "b", main = "Forward Selection - BIC")
plot(cp, type = "b", main = "Forward Selection - Cp")
plot(adjr2, type = "b", main = "Forward Selection - Adjusted R^2")

# b

# The best model size is 2.
optimal_size <- which.min(AIC)
final_model <- regsubsets(Y ~ ., data = data, nvmax = optimal_size, nbest = 1)

final_coefs <- coef(final_model, id = optimal_size)
final_coefs

# c

# No. The true model is not linear.

