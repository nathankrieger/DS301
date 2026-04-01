n = 100
X1 = seq(0,10,length.out =n) #generates 100 equally spaced values from 0 to 10.
X2 = runif(n) #generates 100 uniform values.
error = rnorm(n,0,1)
Y = 2 + 3*X1 + 5*log(X2) + error

df = data.frame(Y, X1, X2)

x <- model.matrix(Y ~ ., data = df)[, -1]

ridge_model <- glmnet(x, Y, alpha = 0, lambda = 2)

summary(ridge_model)

coefs <- coef(ridge_model)

true_beta0 <- 2
  
true_beta1 <- 3
  
true_beta2 <- 5


beta0_hat <- coefs[1, 1]
beta1_hat <- coefs[2, 1]
beta2_hat <- coefs[3, 1]

beta0_bias <- abs(true_beta0 - beta0_hat)
beta0_bias
beta1_bias <- abs(true_beta1 - beta1_hat)
beta1_bias
beta2_bias <- abs(true_beta2 - beta2_hat)
beta2_bias