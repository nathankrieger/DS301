
# 1
## a
n = 100
X1 = seq(0,10,length.out = n)
X2 = runif(n)
error = rnorm(n,0,1)
Y = 2 + 3*X1 + 5*log(X2) + error

df = data.frame(Y, X1, X2)

x = model.matrix(Y ~ ., data = df)

ridge <- glmnet(x, Y, alpha = 0, lambda = 2)

## b
beta0_hat <- numeric(1000)
beta1_hat <- numeric(1000)
beta2_hat <- numeric(1000)
for (i in 1:1000) {
  
  X1 = seq(0,10,length.out = n)
  X2 = runif(n)
  error = rnorm(n,0,1)
  Y = 2 + 3*X1 + 5*log(X2) + error
  
  df = data.frame(Y, X1, X2)
  
  x = model.matrix(Y ~ ., data = df)
  
  ridge <- glmnet(x, Y, alpha = 0, lambda = 2)
  
  coefs <- coef(ridge)
  
  beta0_hat[i] <- coefs[1,1]
  beta1_hat[i] <- coefs[3,1]
  beta2_hat[i] <- coefs[4,1]
  
}

mean_beta0_hat <- mean(beta0_hat)
mean_beta1_hat <- mean(beta1_hat)
mean_beta2_hat <- mean(beta2_hat)

beta0_bias <- mean_beta0_hat - 2
beta1_bias <- mean_beta1_hat - 3
beta2_bias <- mean_beta2_hat - 5

beta0_bias
beta1_bias
beta2_bias


# 2
## a
X3 = rnorm(n)
X4 = rnorm(n)

df = data.frame(Y, X1, X2, X3, X4)

x = model.matrix(Y ~ ., data = df)

regfit <- regsubsets(Y ~ ., data = df, nvmax = 4, nbest = 1)

regfit_sum <- summary(regfit)

rss <- regfit_sum$rss
p <- rowSums(regfit_sum$which)

n <- nrow(df)
aic <- n*log(rss / n) + 2*p


best_size <- which.min(aic)
plot(aic, type = "b", main = "aic")

final_model <- regsubsets(Y ~ ., data = df, nvmax = best_size)

final_coeffs <- coef(final_model, id = best_size)
