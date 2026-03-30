


# alpha = 1 means lasso regression
# alpha = 0 is ridge




library(ISLR2)
library(glmnet)   # install.packages("glmnet") if needed

################################################
## Regularized Regression with glmnet()
################################################

########################
## Step 1: Prepare data
########################

Hitters <- na.omit(Hitters)
head(Hitters)

## glmnet() only takes numerical predictors.
## It cannot directly handle factor variables the same way lm() does.
## So we use model.matrix() to convert everything into the right format.
x <- model.matrix(Salary ~ ., data = Hitters)[, -1]

## Response variable
y <- Hitters$Salary

########################
## Step 2: Lambda grid
########################

## lambda controls how much shrinkage we apply
## larger lambda  -> more shrinkage
## smaller lambda -> less shrinkage
grid <- 10^seq(10, -2, length = 100)

########################
## Ridge Regression
########################

## In glmnet():
## alpha = 0 -> ridge regression
## alpha = 1 -> lasso regression

## Fit ridge regression over the full lambda grid
ridge_model <- glmnet(x, y, alpha = 0, lambda = grid)

## coef(ridge_model) gives a matrix of estimated coefficients.
## Rows = intercept + predictors
## Columns = one column for each lambda value in the grid
coef(ridge_model)
dim(coef(ridge_model)) # so we have 100 different sets of coefficients since our grid is size 100

## Look at coefficients for the 50th lambda value in the grid
ridge_model$lambda[50]
coef(ridge_model)[, 50]

## Obtain ridge regression coefficients for a specific lambda value, say lambda = 50
## Here s = 50 means lambda = 50, not the 50th column
predict(ridge_model, s = 50, type = "coefficients")[1:20, ]

################################################
## Split into training and test sets
################################################

## We now create a test set that will be saved until the very end.
## The training set will be used for cross-validation and model fitting.

set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- setdiff(1:nrow(x), train)

x_train <- x[train, ]
x_test <- x[test, ]
y_train <- y[train]
y_test <- y[test]


################################################
## Ridge: choose lambda using training data
################################################

## Use cross-validation only on the training set
set.seed(1)
cv_out_ridge <- cv.glmnet(x_train, y_train, alpha = 0, lambda = grid)

# if plot(cv.out) doesn't work
# Mac: use quartz()
# Windows: windows()

quartz()
plot(cv_out_ridge)


## Best lambda for ridge
best_lambda_ridge <- cv_out_ridge$lambda.min
best_lambda_ridge

################################################
## Ridge: test MSE
################################################

## Fit ridge on the training data
ridge_train <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda_ridge)

## Predict on the test set using the selected lambda
ridge_pred <- predict(ridge_train, s = best_lambda_ridge, newx = x_test)

## Compute ridge test MSE
ridge_test_mse <- mean((ridge_pred - y_test)^2)
ridge_test_mse

################################################
## Final ridge model
################################################

## Refit ridge on the full dataset using the selected lambda
final_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)

## Final ridge coefficients
coef(final_ridge)

########################
## Lasso Regression
########################

## Fit lasso regression over the full lambda grid
lasso_model <- glmnet(x, y, alpha = 1, lambda = grid)

################################################
## Lasso: choose lambda using training data
################################################

set.seed(1)

# alpha = 1 means lasso regression
# alpha = 0 is ridge
cv_out_lasso <- cv.glmnet(x_train, y_train, alpha = 1, lambda = grid)

# if plot(cv.out) doesn't work
# Mac: use quartz()
# Windows: windows()
plot(cv_out_lasso)

## Best lambda for lasso
best_lambda_lasso <- cv_out_lasso$lambda.min
best_lambda_lasso

################################################
## Lasso: test MSE
################################################

## Fit lasso on the training data
lasso_train <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda_lasso)

## Predict on the test set using the selected lambda
lasso_pred <- predict(lasso_train, s = best_lambda_lasso, newx = x_test)

## Compute lasso test MSE
lasso_test_mse <- mean((lasso_pred - y_test)^2)
lasso_test_mse

################################################
## Final lasso model
################################################

## Refit lasso on the full dataset using the selected lambda
final_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)

## Final lasso coefficients
coef(final_lasso)

################################################
## Compare ridge and lasso
################################################

ridge_test_mse
lasso_test_mse

## Smaller test MSE means better prediction performance on this test set.