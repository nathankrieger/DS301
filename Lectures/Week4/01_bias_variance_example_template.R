
# polynomial models, seeing what degree is best (basic)

library(MASS)


plot(medv~lstat,data=Boston)


# Fit linear regression models with different polynomial degrees

# Linear Regression Model (Degree 1)
# Equation: medv = β0 + β1 * lstat
m1 <- lm(medv ~ lstat, data = Boston)
# Polynomial Regression Model (Degree 5)
# Equation: medv = β0 + β1 * lstat + β2 * lstat^2 
#                + β3 * lstat^3 + β4 * lstat^4 + β5 * lstat^5


m5 <- lm(medv ~ poly(lstat, degree = 5, raw = TRUE), data = Boston)

# Polynomial Regression Model (Degree 9)
# Equation: medv = β0 + β1 * lstat + β2 * lstat^2 + 
#                  β3 * lstat^3 + ... + β9 * lstat^9

m9 <- lm(medv ~ poly(lstat, degree = 6, raw = TRUE), data = Boston)

# Set the plotting layout to a single plot per figure
par(mfrow=c(1,1))

# Scatter plot of medv vs lstat with fitted values from model m1

plot(medv ~ lstat, data = Boston)



## m1$fitted values are the predicted values we obtain from the model m1

points(Boston$lstat, m1$fitted.values, col = 'red')

# Scatter plot of medv vs lstat with fitted values from model m5

points(Boston$lstat, m5$fitted.values, col = 'green')

## m5$fitted values are the predicted values we obtain from the model m5

# Scatter plot of medv vs lstat with fitted values from model m9

points(Boston$lstat, m9$fitted.values, col = 'blue')

## m9$fitted values are the predicted values we obtain from the model m9

set.seed(13)
# Get the number of observations in the dataset

n <- nrow(Boston)

# Randomly split the data into training (50%) and test (50%) sets

train_index <- sample(1:n, floor(n / 2), replace = FALSE)
train_boston <- Boston[train_index, ]

test_boston <- Boston[-train_index, ]


# Fit models with different polynomial degrees on the training set

M1 <- lm(medv ~ lstat, data = train_boston)
M5 <- lm(medv ~ poly(lstat, degree = 5, raw = TRUE), data = train_boston)
M9 <- lm(medv ~ poly(lstat, degree = 9, raw = TRUE), data = train_boston)

## training MSE from M1

train_MSE_M1 <- mean((M1$fitted.values - train_boston$medv)^2)
train_MSE_M1

# Predict values on the test set using model M1

Y_predict_test <- predict(M1, data = test_boston)

## testing MSE from M1
test_MSE_M1 <- mean((Y_predict_test - test_boston$medv)^2)
test_MSE_M1


###########################
#### In-class Activity ####
###########################
# Analyze the effect of model complexity on prediction accuracy 
#by fitting polynomial regression models of increasing degrees (1 to 9)
#to the training set and evaluating their performance 
#on both training and test sets.

# Response: medv
# Predictor: lstat

# STEP 1: Initialize Storage for MSE Values

train_MSE <- c()
test_MSE <- c()


train_index <- sample(1:n, floor(n / 2), replace = FALSE)
train_boston <- Boston[train_index, ]

test_boston <- Boston[-train_index, ]

# STEP 2: Loop through polynomial degrees from 1 to 9
# Inside loop: Fit model, calculate train_mse, test_mse

for (i in 1:9) {
  
  tmp <- lm(medv ~ poly(lstat, degree = i, raw = TRUE), data = train_boston)
  train_MSE[i] <- mean((tmp$fitted.values - train_boston$medv)^2)
  
  
  tmp2 <- lm(medv ~ poly(lstat, degree = i, raw = TRUE), data = test_boston)
  test_MSE[i] <- mean((tmp2$fitted.values - test_boston$medv)^2)
  
}

# Step 3: Plot Training MSE vs. Model Complexity (Polynomial Degree)

plot(train_MSE)

# Step 4: Plot Test MSE vs. Model Complexity (Polynomial Degree)

plot(test_MSE)
