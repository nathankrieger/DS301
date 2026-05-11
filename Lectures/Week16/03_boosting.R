## Boosting for regression trees

## Install the package if needed
install.packages("gbm")

library(gbm)
set.seed(1)

## Fit a boosted regression tree model
## distribution = "gaussian" because medv is a quantitative response
## n.trees = 5000 means we fit 5000 small trees
## interaction.depth = 4 limits the depth/complexity of each tree
## use defaul shrinkage parameter 0.1
boost.boston <- gbm(
  medv ~ .,
  data = Boston[train, ],
  distribution = "gaussian",
  n.trees = 5000,
  interaction.depth = 4
)

## Show variable importance
## The plot and table show which predictors are most influential
summary(boost.boston)

## Partial dependence plot for rm
## Shows the marginal effect of rm on predicted medv
plot(boost.boston, i = "rm")

## Partial dependence plot for lstat
## Shows the marginal effect of lstat on predicted medv
plot(boost.boston, i = "lstat")

## Predict medv for the test set using all 5000 trees
yhat.boost <- predict(
  boost.boston,
  newdata = Boston[-train, ],
  n.trees = 5000
)

## Calculate test MSE
mean((yhat.boost - boston.test)^2)

## Fit another boosted model with a larger shrinkage parameter 0.2
## shrinkage is the learning rate lambda
## A larger shrinkage value means each tree has a larger effect
boost.boston <- gbm(
  medv ~ .,
  data = Boston[train, ],
  distribution = "gaussian",
  n.trees = 5000,
  interaction.depth = 4,
  shrinkage = 0.2,
  verbose = FALSE
)

## Predict on the test set using the new boosted model
yhat.boost <- predict(
  boost.boston,
  newdata = Boston[-train, ],
  n.trees = 5000
)

## Calculate test MSE for the model with shrinkage = 0.2
mean((yhat.boost - boston.test)^2)