
# LOOCV, PRESS, k-fold - comparing linear and quadratic for each

library(ISLR2)
# response: mpg
# predictor: horsepower


head(Auto)

plot(Auto$horsepower, Auto$mpg)

################################
### Validation Set Approach ####
################################

# Hint: reference 01_bias_variance_example_code

n <- nrow(Auto)

# Randomly split our data into a training set and testing set
# 50/50 split

train_index <- sample(1:n, floor(n / 2), replace = FALSE)

train_auto <- Auto[train_index, ]
test_auto <- Auto[-train_index, ]

# Fit M1: linear and M2: quadratic models (degree = 2) on the training data

M1 <- lm(mpg ~ horsepower, data = train_auto)

M2 <- lm(mpg ~ poly(horsepower, degreee = 2, raw = TRUE), data = train_auto)

# Plot the data and the fitted values from both models

plot(Auto$horsepower, Auto$mpg)

plot(mpg ~ horsepower, data = Auto) # The same as the method above

points(train_auto$horsepower, M1$fitted.values, col='red')
points(train_auto$horsepower, M2$fitted.values, col='green')

## Evaluate our model on our test set using predict()

m1_mpg_test = predict(M1, newdata = test_auto)

m2_mpg_test = predict(M2, newdata = test_auto)

# Calculate test MSE

validation_MSE_M1 = mean((test_auto$mpg - m1_mpg_test)^2)
validation_MSE_M1

validation_MSE_M2 = mean((test_auto$mpg - m2_mpg_test)^2)
validation_MSE_M2
# if we had chosen a different training set, 
# we would obtain slightly different errors on the test set.

##############
### LOOCV ### = leave one out cross validation
##############

MSE_M1 = MSE_M2= rep(0,n) #vector

for(i in 1:n){
  test = Auto[i, ]
  train = Auto[-i, ]
  
  M1 = lm(mpg~horsepower, data = train)
  M2 = lm(mpg~poly(horsepower,2), data = train)
  
  M1_mgp_test = predict(M1, newdata= test)
  M2_mgp_test = predict(M2, newdata= test)
  
  MSE_M1[i] = (test$mpg - M1_mgp_test)^2
  MSE_M2[i] = (test$mpg - M2_mgp_test)^2
}

LOOCV_MSE_M1 = mean(MSE_M1)
LOOCV_MSE_M2 = mean(MSE_M2)


###############
### PRESS #### = Predicted Residual Sum of Squares - LOOCV shortcut
###############

# Prediction Sum of Squares (PRESS) for linear models
M1 = lm(mpg~horsepower, data = train)

# leverage
leverage_M1 = hatvalues(M1)
press_residuals_M1 = M1$residuals / (1 - leverage_M1)
press_MSE_M1 = mean(press_residuals_M1^2)

# Prediction Sum of Squares (PRESS) for quadratic models
M2 <- lm(mpg ~ poly(horsepower, 2), data = train)

# leverage
leverage_M2 = hatvalues(M2)
press_residuals_M2 = M2$residuals / (1 - leverage_M2)
press_MSE_M2 = mean(press_residuals_M2^2)

#################
### K-fold CV ###
#################

k = 10 # Number of folds

#install.packages('caret')
library(caret)


# Initialize vectors to store MSE for each fold
MSE_M1 <- rep(0, k)
MSE_M2 <- rep(0, k)

# Create folds
flds <- createFolds(Auto$mpg, k, list = TRUE)
flds[[1]]

for(i in 1:k){
  # Get test indices for this fold
  test_index = flds[[i]]
  test = Auto[test_index, ]
  train = Auto[-test_index, ]
  
  M1 = lm(mpg~horsepower, data = train)
  M2 = lm(mpg~poly(horsepower,2), data = train)
  
  M1_mgp_test = predict(M1, newdata= test)
  M2_mgp_test = predict(M2, newdata= test)
  
  MSE_M1[i] = (test$mpg - M1_mgp_test)^2
  MSE_M2[i] = (test$mpg - M2_mgp_test)^2
  
}

k_fold_MSE_M1 = mean(MSE_M1)
k_fold_MSE_M2 = mean(MSE_M2)
  
  
################
# HW 4 Problem 3
################


