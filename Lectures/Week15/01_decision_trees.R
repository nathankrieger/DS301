## -----------------------------
## Classification Tree: Carseats
## -----------------------------

library(ISLR2)
library(tree)

head(Carseats)

# Response is Sales.
# Convert Sales to a binary variable:
# High = Yes if Sales > 8
# High = No if Sales <= 8
Carseats$High <- factor(ifelse(Carseats$Sales <= 8, "No", "Yes"))

head(Carseats)

## -----------------------------
## Train/Test Split
## -----------------------------

set.seed(6)
train <- sample(1:nrow(Carseats), 200)

Carseats.test <- Carseats[-train, ]

# True class labels for the test set
High.test <- Carseats$High[-train]

## -----------------------------
## Fit Classification Tree
## -----------------------------

# We remove Sales because High was created from Sales.
# If we keep Sales, the tree can use Sales directly to predict High.
tree.carseats <- tree(High ~ . - Sales, data = Carseats, subset = train)

tree.carseats
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

## -----------------------------
## Prediction on Test Set
## -----------------------------

tree.pred <- predict(tree.carseats, newdata = Carseats.test, type = "class")

# Confusion matrix
table(tree.pred, High.test)

# Test accuracy
mean(tree.pred == High.test)

# Test error rate
mean(tree.pred != High.test)

## -----------------------------
## Cross-Validation for Pruning
## -----------------------------

set.seed(6)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)

cv.carseats
# $size: number of terminal nodes (tree complexity)
# $dev: cross-validation error

plot(cv.carseats$size, cv.carseats$dev, type = "b",
     xlab = "Tree Size",
     ylab = "Cross-Validation Error",
     main = "CV Error for Different Tree Sizes")

## -----------------------------
## Prune the Tree
## -----------------------------

# Choose the tree size with the smallest CV error
best.size <- cv.carseats$size[which.min(cv.carseats$dev)]
best.size

prune.carseats <- prune.misclass(tree.carseats, best = best.size)

plot(prune.carseats)
text(prune.carseats, pretty = 0)

## -----------------------------
## Prediction Using Pruned Tree
## -----------------------------

prune.pred <- predict(prune.carseats, newdata = Carseats.test, type = "class")

# Confusion matrix
table(prune.pred, High.test)

# Test accuracy
mean(prune.pred == High.test)

# Test error rate
mean(prune.pred != High.test)


############################################################################



## -----------------------------
## Fitting Regression Trees
## -----------------------------

library(MASS)
library(tree)

# Load and inspect the Boston housing data
head(Boston)

# Randomly split the data into training and test sets
set.seed(11)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)

# Fit a regression tree using the training data
tree.boston <- tree(medv ~ ., data = Boston, subset = train)

# Display the tree output
tree.boston

# The output includes:
# split: split criterion: 
# n: the number of observations
# deviance: goodness of fit (smaller is better)
# yval: overall prediction for the branch


summary(tree.boston)

# The summary shows:
# - which variables were used in the tree
# - the number of terminal nodes
# - the training deviance
# For regression trees, deviance is the residual sum of squares.

# Plot the fitted tree
plot(tree.boston)
text(tree.boston, pretty = 0)

## -----------------------------
## Test Set Prediction
## -----------------------------

# Create the test set
test <- Boston[-train, ]

# Predict median house value for the test set
tree.pred <- predict(tree.boston, newdata = test)

# Actual response values in the test set
Y.test <- Boston[-train, "medv"]

# Compute test MSE
mean((tree.pred - Y.test)^2)

## -----------------------------
## Pruning the Tree
## -----------------------------

# Use cross-validation to choose the optimal tree size
cv.boston <- cv.tree(tree.boston)

cv.boston

# size: number of terminal nodes
# dev: cross-validation error
# k: cost-complexity tuning parameter

# Plot CV error versus tree size
plot(cv.boston$size, cv.boston$dev, type = "b",
     xlab = "Tree Size",
     ylab = "Cross-Validation Error",
     main = "CV Error for Different Tree Sizes")

# Prune the tree to have 7 terminal nodes
prune.boston <- prune.tree(tree.boston, best = 7)

# Plot the pruned tree
plot(prune.boston)
text(prune.boston, pretty = 0)

# Predict using the pruned tree
tree.prune <- predict(prune.boston, newdata = test)

# Compute test MSE for the pruned tree
mean((tree.prune - Y.test)^2)

## -----------------------------
## Identify Observations in a Region
## -----------------------------

# Find training observations satisfying both conditions
index <- which(Boston[train, ]$lstat < 8.13 & Boston[train, ]$tax < 255)

head(index)

