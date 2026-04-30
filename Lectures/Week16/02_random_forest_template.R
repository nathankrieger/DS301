## Bagging and Random Forests

# install.packages("randomForest")
library(randomForest)
library(ISLR2)

## Note:
## Even with the same seed, results may differ slightly depending on
## your R version and the version of the randomForest package.

set.seed(1)

## Bagging is a special case of random forests where m = p.
## In other words, all predictors are considered at each split.
## Therefore, randomForest() can be used for both bagging and random forests.

## -------------------------
## Bagging
## -------------------------

# Randomly select half of the observations as the training set
train <- sample(1:nrow(Boston), nrow(Boston) / 2)

# Check the dimension of the Boston dataset
dim(Boston)

# Since Boston has 12 predictors, set mtry = 12 for bagging
bag.boston <- randomForest(
  medv ~ .,
  data = Boston,
  subset = train,
  mtry = ___,
  importance = ___,
  ntree = ___
)

## Variable importance
importance(bag.boston)

## Two measures of variable importance are reported:
## 1. %IncMSE:
##    A larger value means the predictor is more important for prediction.
##
## 2. IncNodePurity:
##    For regression trees, this measures the total decrease in RSS
##    from splits using that predictor, averaged over all trees.
##
##    For classification trees, node impurity is usually measured
##    using the Gini index.

?importance

# Plot variable importance
varImpPlot(bag.boston)

## Test MSE

# Predict on the test set
yhat.bag <- predict(bag.boston, newdata = Boston[-train, ])

# True response values in the test set
boston.test <- Boston[-train, "medv"]

# Plot predicted values versus true values
plot(yhat.bag, boston.test,
     xlab = "Predicted medv",
     ylab = "True medv",
     main = "Bagging: Predicted vs True Values")

# Add reference line y = x
abline(0, 1)

# Calculate test MSE
mean((yhat.bag - boston.test)^2)


## -------------------------
## Random Forest
## -------------------------

## By default:
## - For regression trees, randomForest() uses p/3 predictors at each split.
## - For classification trees, randomForest() uses sqrt(p) predictors at each split.
##
## Here we set mtry = 6 manually.

set.seed(1)

rf.boston <- randomForest(
  medv ~ .,
  data = Boston,
  subset = train,
  mtry = ___,
  importance = TRUE,
  ntree = 500
)

# Predict on the test set
yhat.rf <- predict(   )

# Calculate test MSE

# Variable importance

# Plot variable importance
