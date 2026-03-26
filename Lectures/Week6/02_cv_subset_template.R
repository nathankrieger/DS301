
########################################################
###### Combining CV with 'best' subset selection #######
########################################################
library(ISLR2)
library(leaps)

## the predict() function does not work on regsubsets objects, 

head(Hitters)
Hitters <- na.omit(Hitters)
dim(Hitters)

## Validation set approach:   
## Use training set to perform all aspects of model-fitting, including model selection
## If we use the full dataset to carry out model selection, 
# we are essentially cheating (seeing the data twice) 
# and the test errors will be overly optimistic 

n <- dim(Hitters)[1]

set.seed(10)

train_index <- sample(1:n,n/2,rep=FALSE)
train <- Hitters[train_index,]
test <- Hitters[-train_index,]

best_train <- regsubsets(Salary ~ ., data = train, nbest = 1, nvmax = 19)
  
# so we need to write our own code to obtain our predicted values:

# Convert the test dataset into a model matrix (needed for matrix multiplication)
# This creates a numeric matrix with an intercept column and all predictor variables
  
test_mse <- rep(NA, 19)
test_mat <- model.matrix(Salary ~ ., data = test)
# Loop through all 19 possible model sizes 
for (i in 1:19) {

  # Extract the coefficients for the model with 'i' predictors from the best subset selection
  coef_m <- coef(best_train, id = i)
  # Compute the predicted values for the test set using matrix multiplication
  # Multiply the selected predictors' values in 'test.mat' by their corresponding coefficients
  
  pred <- test_mat[ , names(coef_m)]%*%coef_m

  # Calculate MSE
  test_mse[i] <- mean((pred - test$Salary)^2)
}

test_mse



########################
## Final Model with Optimal Size
########################

# Identify the best model size based on the minimum testing error  
optimal_size <- which.min(test_mse)
optimal_size

# Train the final model using the optimal number of predictors
final_model <- regsubsets(Salary ~ ., data = Hitters, nvmax = optimal_size)

# Get the selected predictors for the final model
final_coeffs <- coef(final_model, id = optimal_size)


#########################################################
## We can extend this to k-fold CV. 
# This approach is somewhat involved because we must perform 
# best subset selection within each of the k training sets. 

## This approach is often used to determine model SIZE, 
# not which predictors will end up in the model. 
# Once we have determined the optimal model SIZE, 
# best subset selection is then performed on the full data set
# and the best model of size ____ is chosen. 

########################
## In-class Activity  ##
########################

########################
## K-Fold Cross-Validation with Best Subset Selection
########################

## 1. Adapt the code above to perform 10-fold CV using subset selection.
## Your code should involve TWO loops: 
## (1) one outer loop to iterate through the k-folds
## (2) one inner loop to iterate through the 19 candidate models selected by subset selection
       # similar to the for loop used in Validation Set Approach above

## Report the 10-fold CV error for each model size (19 in total). 
## What is the optimal model size based on 10-fold CV? Report that in your solutions. 

## 2. Once you determined the optimal model size, 
## perform best subset selection on the full data set 
## and report the final model with optimal size. 

## Work in groups to come up with a solution. 
## Copy and paste any relevant code on Discussion. 
## Please be sure to list all your group members names. 
## Only one group member needs to post on Discussion. 


###################
# Load necessary libraries

library(caret)

set.seed(10) 

# Number of folds for cross-validation
k = 10
flds = createFolds(Hitters$Salary, k = k, list = TRUE)  # Create folds

# Number of predictors (max 19 models)
max_models = 19

# Matrix to store cross-validation errors for each model size across all folds
cv_errors = matrix(NA, nrow = k, ncol = max_models)

# Outer loop: Iterate through each fold  
for (i in 1:k) {  
  # Get test indices for this fold
  test_index = flds[[i]]  
  # Split the dataset into training and validation sets for the current fold:
  test_data = Hitters[test_index, ]  # Validation set
  train_data = Hitters[-test_index, ]  # Training set
  
  # Perform Best Subset Selection on the training set
  regfit = regsubsets(Salary ~ ., data = train_data, nbest = 1, nvmax = max_models)
  
  # Inner loop: Iterate through each model size (1 to max_models)
  # Calculate cv error (test MSE) 
  for (j in 1:max_models) {  
    test_mat = model.matrix(Salary ~ ., data = test_data)
    coef_m = coef(regfit, id = j)    
    pred = test_mat[, names(coef_m)] %*% coef_m 
    cv_errors[i, j] = mean((test_data$Salary - pred)^2) 
  }
  
}

# Compute the average cross-validation error for each model size  
mean_cv_errors = colMeans(cv_errors)
print(mean_cv_errors)

# Identify the best model size based on the minimum cross-validation error  
optimal_size = which.min(mean_cv_errors)
print(paste("Optimal model size:", optimal_size))

# Plot cross-validation errors to visualize model performance  
plot(mean_cv_errors, type = "b")
abline(v = optimal_size, col = "red")


########################
## Final Model with Optimal Size
########################

# Train the final model using the optimal number of predictors
final_model <- regsubsets(Salary ~ ., data = Hitters, nvmax = optimal_size)

# Get the selected predictors for the final model
final_coeffs <- coef(final_model, id = optimal_size)
print("Final Model Coefficients:")
print(final_coeffs)
