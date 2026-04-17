library(ISLR2)
library(MASS)
#install.packages("MASS")

head(Smarket)
# The Smarket data contains daily percentage returns for the S&P 500 from 2001 to 2005.
# Variables include:
# Lag1 - Lag5: percentage returns for the previous 1 to 5 trading days
# Volume: number of daily shares traded in billions
# Today: percentage return for the current day
# Direction: whether the market went Up or Down

# Create training and test data frames
Smarket_train <- Smarket[Smarket$Year < 2005, ]
Smarket_test  <- Smarket[Smarket$Year == 2005, ]

dim(Smarket_train) # Smarket_train contains data before 2005
dim(Smarket_test) # Smarket_test contains the 2005 data


###########
### LDA ###
###########

# Fit LDA using Lag1 and Lag2 as predictors
lda_fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket_train)

lda_fit
# This output shows:
# 1. Prior probabilities for each class
# 2. Group means for Lag1 and Lag2
# 3. Coefficients of the linear discriminant

# Prior probabilities
lda_fit$prior
# These are the estimated prior probabilities of Down and Up in the training data.
# For example, if the training set has 998 observations in total, with 491 Down days
# and 507 Up days, then
# P(Direction = Down) = 491 / 998 = 0.492
# P(Direction = Up)   = 507 / 998 = 0.508

  
lda_pred <- predict(lda_fit, Smarket_test)

names(lda_pred)

# Predict class labels
head(lda_pred$class)

# posterior: posterior probabilities P(Y = k | X)
# P(Direction = Down | X)
# P(Direction = Up | X)
head(lda_pred$posterior)

# Confusion matrix
table(lda_pred$class, Smarket_test$Direction)

# Classification accuracy
mean(lda_pred$class == Smarket_test$Direction)

# Misclassification rate
mean(lda_pred$class __ Smarket_test$Direction)


# Look at the first few posterior probabilities
lda_pred$posterior[1:10, ]
lda_pred$class[1:10]

# Default rule:
# Assign the class with the larger posterior probability.
# Since there are only two classes, this is equivalent to using a 0.5 threshold.

# Count how many observations have P(Up | X) > 0.52
sum(lda_pred$posterior[, "Up"] >  ____ )

# Use a stricter threshold: predict Up only if P(Up | X) > 0.52
lda_class <- rep("Down", nrow(Smarket_test))
lda_class[lda_pred$posterior[, "Up"] > 0.52] <- 

# Confusion matrix with new threshold
table(lda_class, Smarket_test$Direction)

# Misclassification rate with new threshold
mean(lda_class != Smarket_test$Direction)

# Accuracy with new threshold
mean(lda_class == Smarket_test$Direction)

###########
### QDA ###
###########

# Fit QDA using Lag1 and Lag2 as predictors
qda_fit <- ___ (Direction ~ Lag1 + Lag2, data = Smarket_train)

qda_fit

# Make predictions on the test data
qda_pred <- predict(qda_fit, Smarket_test)

names(qda_pred)

# Predicted classes
head(qda_pred$class)

# Posterior probabilities
head(qda_pred$posterior)

# Confusion matrix
table(qda_pred$class, Smarket_test$Direction)

# Misclassification rate
mean(qda_pred$class != Smarket_test$Direction)

# Classification accuracy
mean(qda_pred$class == Smarket_test$Direction)
