library(ISLR2)
head(Default)
dim(Default)

# ------------------------------
# Split data into training and test sets
# ------------------------------
set.seed(7)

n <- nrow(Default)
train <- sample(1:n, n / 2, replace = FALSE)
test <- setdiff(1:n, train)

# ------------------------------
# Fit logistic regression model
# Response: default
# Predictors: student, balance
# ------------------------------
glm.fit <- glm(default ~ student + balance,
               data = Default,
               subset = train,
               family = "binomial")

summary(glm.fit)

# ------------------------------
# Predict probabilities on test set
# type = "response" returns estimated probabilities
# ------------------------------
glm.prob <- predict(glm.fit, Default[test, ], type = "response")
head(glm.prob)

# ------------------------------
# Convert predicted probabilities to class labels
# Use 0.54 as the classification cutoff
# ------------------------------
glm.pred <- rep("No", length(test))
glm.pred[__________] <- "Yes"

# Confusion matrix
# Rows = predicted class
# Columns = true class
table(glm.pred, Default[test, ]$default)

# ------------------------------
# Misclassification rate
# ------------------------------
1 - mean(glm.pred ____ Default[test, ]$default)

# ------------------------------
# ROC curve analysis
# ROC curve shows the trade-off between:
# - True Positive Rate (TPR / sensitivity)
# - False Positive Rate (FPR = 1 - specificity)
# ------------------------------
#install.packages("ROCR")
library(ROCR)

# Create a prediction object that pairs predicted probabilities with true labels
# This is the required input format for ROC calculations
ROCRpred <- prediction(___)


# Plot the ROC curve: shows trade-off between True Positive Rate and False Positive Rate
# Each point corresponds to a different classification threshold
# ROC curve with cutoffs labeled
perf <- performance(ROCRpred, "tpr", "fpr")

plot(perf)


plot(perf,
     colorize = TRUE,
     print.cutoffs.at = _______,
     text.adj = c(-0.2, 1.7))

plot(perf,
     colorize = TRUE,
     print.cutoffs.at = ________,
     text.adj = c(-0.2, 1.7))

# ------------------------------
# Extract threshold, FPR, and TPR values
# Each threshold corresponds to one point on the ROC curve
# ------------------------------

thresholds <- data.frame(
  threshold = perf@________,  # cutoff values used to generate the ROC curve
  fpr = perf@_________,            # false positive rate at each cutoff
  tpr = perf@_______             # true positive rate at each cutoff
)

head(thresholds)

# Example: display thresholds with FPR less than 0.01
fpr0.01 <- subset(thresholds, fpr < 0.01)
head(fpr0.01)

# ------------------------------
# Compute Area Under the Curve (AUC)
# AUC summarizes overall classifier performance
# A classifier no better than random guessing has AUC = 0.5
# ------------------------------
auc.ROCR <- performance(ROCRpred, measure = "auc")
auc.ROCR@y.values[[1]]
