# ==============================================================================
# EXAM MASTER REFERENCE FILE: Unit - Classification & Trees
# ==============================================================================

library(ISLR2)
library(MASS)
library(tree)
library(randomForest)
library(gbm)
library(class)
library(ROCR)
library(caret)

# ------------------------------------------------------------------------------
# 1. LOGISTIC REGRESSION (GLM)
# ------------------------------------------------------------------------------
set.seed(7)
# Fit model
glm.fit <- glm(default ~ student + balance, data = Default, family = 'binomial')

# Predict probabilities on test set
glm.prob <- predict(glm.fit, newdata = Default[test, ], type = "response")

# Convert probabilities to class labels with custom threshold
glm.pred <- rep("No", length(glm.prob))
glm.pred[glm.prob > 0.2] <- "Yes" # Lowering cutoff reduces False Negatives

# ------------------------------------------------------------------------------
# 2. DISCRIMINANT ANALYSIS (LDA/QDA) & NAIVE BAYES
# ------------------------------------------------------------------------------
# LDA: Assumes normal distribution
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = (Year < 2005))
lda.pred <- predict(lda.fit, Smarket[Smarket$Year == 2005, ])
# lda.pred$class = labels; lda.pred$posterior = probabilities

# QDA: Non-linear boundaries
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = (Year < 2005))

# Naive Bayes: Assumes independent predictors
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket, subset = (Year < 2005))

# ------------------------------------------------------------------------------
# 3. K-NEAREST NEIGHBORS (KNN)
# ------------------------------------------------------------------------------
# CRITICAL: Always scale data for KNN
standardized.X <- scale(Caravan[, -86]) 

# Fit KNN
knn.pred <- knn(train = train.X, test = test.X, cl = train.Y, k = 1)

# 5-Fold Cross Validation for K (Manual Loop)
flds <- createFolds(train.Y, k = 5, list = TRUE)
k.values <- c(1, 3, 5, 7, 9, 11)
cv.prec <- numeric(length(k.values))

for (j in 1:length(k.values)) {
  fold.prec <- numeric(5)
  for (i in 1:5) {
    # Split train/validation within the loop
    pred <- knn(train.X[-flds[[i]], ], train.X[flds[[i]], ], 
                cl = train.Y[-flds[[i]]], k = k.values[j])
    # Calculate metric (e.g., Precision)
    tab <- table(pred, train.Y[flds[[i]]])
    fold.prec[i] <- tab["Yes","Yes"] / sum(tab["Yes", ])
  }
  cv.prec[j] <- mean(fold.prec)
}
best_k <- k.values[which.max(cv.prec)] #

# ------------------------------------------------------------------------------
# 4. DECISION TREES (Classification & Regression)
# ------------------------------------------------------------------------------
# Fit tree (Exclude Sales if High was made from Sales!)
tree.fit <- tree(High ~ . - Sales, data = Carseats, subset = train)

# Summary and Plotting
summary(tree.fit)
plot(tree.fit); text(tree.fit, pretty = 0)

# Cross-Validation for Pruning
cv.fit <- cv.tree(tree.fit, FUN = prune.misclass) # Use prune.misclass for Classification
plot(cv.fit$size, cv.fit$dev, type = "b")
best.size <- cv.fit$size[which.min(cv.fit$dev)]

# Apply Pruning
prune.fit <- prune.misclass(tree.fit, best = best.size)

# ------------------------------------------------------------------------------
# 5. BAGGING AND RANDOM FORESTS
# ------------------------------------------------------------------------------
# Bagging: mtry = total number of predictors
bag.fit <- randomForest(medv ~ ., data = Boston, mtry = 12, importance = TRUE)

# Random Forest: mtry = p/3 (regression) or sqrt(p) (classification)
rf.fit <- randomForest(medv ~ ., data = Boston, mtry = 4, importance = TRUE)

# Importance metrics: %IncMSE and IncNodePurity
importance(rf.fit)
varImpPlot(rf.fit)

# ------------------------------------------------------------------------------
# 6. BOOSTING
# ------------------------------------------------------------------------------
# Fit Boosted Model (Sequential)
boost.fit <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian", 
                 n.trees = 5000, interaction.depth = 4, shrinkage = 0.1)

# Predict requires specifying number of trees
yhat.boost <- predict(boost.fit, newdata = Boston[-train, ], n.trees = 5000)

# ------------------------------------------------------------------------------
# 7. EVALUATION METRICS (The Essentials)
# ------------------------------------------------------------------------------
# Confusion Matrix
conf_mat <- table(Predicted = glm.pred, Actual = test_set$default)

# Error Rates
misclass_rate <- 1 - mean(glm.pred == test_set$default)
precision <- conf_mat["Yes", "Yes"] / sum(conf_mat["Yes", ]) # Predicted Yes that are Yes
false_negative_rate <- conf_mat["No", "Yes"] / sum(conf_mat[, "Yes"]) # Defaulted but predicted No

# ROC and AUC
ROCRpred <- prediction(glm.prob, test_set$default)
plot(performance(ROCRpred, 'tpr', 'fpr'), colorize = TRUE)
auc <- performance(ROCRpred, measure = "auc")@y.values[[1]] #