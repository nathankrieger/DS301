####### Nathan Krieger ########

###### DS 3010 Exam 1 ############
library(caret)

set.seed(123)
n <- 100
X <- seq(0, 40, length.out = n)
error <- rnorm(n, mean = 0, sd = 5)

# True relationship is cubic:
Y <- 50 + 1.2*X - 0.08*X^2 + 0.001*X^3 + error
df <- data.frame(X = X, Y = Y)

# ----------------------------
# 1)
# ----------------------------

m1 <- lm(Y ~ X, data = df)

##  (a)

X_pre <- 20

new <- data.frame(X = X_pre)

new

m1$fitted.values[c(20)]

predict(m1, newdata = new)




# 47.73149


## (b)


new_b <- data.frame(X = c(20))

predict(m1, newdata = new_b, interval = "confidence", level = 0.95)


## (c)

new_c <- data.frame(X = c(47.73149))
predict(m1, newdata = new_c, interval = "prediction", level = 0.95)

# ----------------------------
# 2)
# ----------------------------


library(ISLR2)
library(caret)
library(ggplot2)
#install.packages('caret')

k = 5 # Number of folds

max_degree <- 10
errors <- numeric(max_degree)

# Create folds
flds <- createFolds(df$Y, k, list = TRUE)
#flds[[1]]

for (d in 1:max_degree) {
  
  folds <- numeric(k)
  
  for(i in 1:k){
    # Get test indices for this fold
    test_index = flds[[i]]
    test = df[test_index, ]
    train = df[-test_index, ]
    
    m <- lm(Y ~ poly(X, d, raw = TRUE), data = train)
    
    predict <- predict(m, newdata = test)
    
    folds[i] = mean((test$Y - predict)^2)
    
  }
  
  errors[d] = mean(folds)
}

cv_results <- data.frame(Degree = 1:max_degree, CV_Error = errors)
print(cv_results)

ggplot(cv_results, aes(x = Degree, y = CV_Error)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  scale_x_continuous(breaks = 1:9) +
  labs(title = "5-Fold CV Error vs. Polynomial Degree",
       x = "Polynomial Degree",
       y = "Mean Squared Error (CV)") +
  theme_minimal()


## (b)

# degree 3

## (c)

# The reason degree 3 is preferred in this case is not only because it has by far the lowest CV error (see plot), 
# but also because we know that degree 3 is the true degree of the relationship.

# Choosing a larger CV mse would just mean choosing a worse model.


# ----------------------------
# 3) Extra credit 
# ----------------------------


library(ISLR2)
library(caret)
library(ggplot2)
#install.packages('caret')

k = 5 # Number of folds

max_degree <- 10
errors <- numeric(max_degree)
train_MSE_total <- numeric(max_degree)

# Create folds
flds <- createFolds(df$Y, k, list = TRUE)
#flds[[1]]

for (d in 1:max_degree) {
  
  folds <- numeric(k)
  train_mse_inner <- numeric(k)
  
  for(i in 1:k){
    # Get test indices for this fold
    test_index = flds[[i]]
    test = df[test_index, ]
    train = df[-test_index, ]
    
    m <- lm(Y ~ poly(X, d, raw = TRUE), data = train)
    
    trainMSE <- sum(m1$residuals^2)/nrow(train)
    
    train_mse_inner[i] = trainMSE
    
    predict <- predict(m, newdata = test)
    
    folds[i] = mean((test$Y - predict)^2)
    
  }
  
  
  # trainMSE <- sum(m1$residuals^2)/nrow(train_patients)
  
  errors[d] = mean(folds)
  
  train_MSE_total[d] = mean(train_mse_inner)
}



cv_results <- data.frame(Degree = 1:max_degree, CV_Error = errors, train_MSE = train_MSE_total)
print(cv_results)

ggplot(cv_results, aes(x = Degree, y = CV_Error, train_MSE)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  scale_x_continuous(breaks = 1:9) +
  labs(title = "5-Fold CV Error vs. Polynomial Degree",
       x = "Polynomial Degree",
       y = "Mean Squared Error (CV)") +
  theme_minimal()


## (b)

# As the model complexity increases, training MSE will decrease while test MSE will decrease until it reaches an 
# optimal fit and then begin to increase again as the model becomes overfitted to the training data.
