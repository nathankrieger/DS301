###########
### knn ###
###########

library(ISLR2)
#install.packages('class')
library(class)

# ------------------------------
# Load and inspect the data
# ------------------------------

head(Caravan)
# Caravan dataset (TIC Benchmark): 5822 customers, 86 variables
# Variables 1–43: sociodemographic data
# Variables 44–86: product ownership (based on zip code groups)
# Customers in the same zip code share the same sociodemographic attributes


# Response (Var 86): Purchase (Yes/No)
# Predictor (Var 1-85)
# Goal: predict whether a customer buys caravan insurance

# ------------------------------
# Why do we standardize variables?
# ------------------------------
# k-NN is distance-based, so variables with larger scales
# can dominate the distance calculation.
# Standardizing puts all predictors on a similar scale.
var(Caravan[,1])
var(Caravan[,2])

standardized.X = scale(Caravan[,-86])
var(standardized.X[,1])
var(standardized.X[,2])


# ------------------------------
# Create training and test sets
# ------------------------------
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Caravan$Purchase[-test]
test.Y = Caravan$Purchase[test]

# ------------------------------
# Fit k-NN with k = 1
# ------------------------------
set.seed(1)
knn.pred = knn(train.X,test.X,cl = train.Y,k=1)
head(knn.pred)

# Confusion matrix
table(knn.pred,test.Y)

# Test misclassification rate
mean(test.Y!=knn.pred)


table(test.Y)
mean(test.Y=="Yes") # proportion of customers who actually purchased


# Only ~6% of customers purchased insurance
# A naive classifier (always predicting "No") would have ~6% error

# Suppose contacting a customer is costly (e.g., salesperson visit)
# The goal is to target customers who are likely to buy

# So overall error rate is not the main focus
# Instead, evaluate: among those predicted "Yes",
# how many actually purchased? (precision)

# True Positives (correctly predicted "Yes")
table(knn.pred, test.Y)["Yes", "Yes"]

# Precision = True Positives / Total predicted "Yes"
table(knn.pred, test.Y)["Yes", "Yes"] / sum(table(knn.pred, test.Y)["Yes", ])


###### How to choose K? ##########
###### Cross-validation ##########

# You can write your own loop. 
# 5-fold CV

########################
## In-class Activity  ##
########################

## Implement 5-fold cross validation to choose the optimal value of k
## using the test/training set we created above. 
## for k-NN using the training set we created above.
## You may use library(caret) to generate the folds.
## Work in groups and write code to:
## 1. test several values of k(1,3,5,7,9,11,13),
## 2. compute the average CV accuracy for each k,
## 3. select the best k,
## 4. fit a model with best k with entire training set
## 4. evaluate the final model on the test set.


## Work in groups to come up with a solution and post to Piaaza. 

library(caret)
library(class)

set.seed(1)

# create 5 folds from the training data
flds <- createFolds(train.Y, k = 5, list = TRUE)

# candidate values of k
k.values <- c(1, 3, 5, 7, 9, 11)

# store average CV accuracy for each k
cv.prec <- numeric(length(k.values))

# Split folds: see M2: 02-cross_validation-code.R 
flds <- createFolds(train.Y, k = 5, list = TRUE)

# outer loop: try each candidate value of k
for (j in 1:length(k.values)) {
  
  k <- k.values[j]
  
  fold.prec <- numeric(5)
  
  # inner loop: go through the 5 folds
  for (i in 1:5 ) {
    
    # test set for this fold
    test_index <- flds[[i]] 
    test.X.cv <- train.X[test_index,]
    test.Y.cv <- train.Y[test_index]
    
    # training set for this fold
    train.X.cv <- train.X[-test_index,]
    train.Y.cv <- train.Y[-test_index]
    
    # fit k-NN and predict on the test fold
    pred <- knn(train.X.cv, test.X.cv, cl = train.Y.cv, k = k) #Hint: line 51
    
    # precision for this fold 
    #Hint: line 80 
    # Line 80: table(knn.pred,test.Y)["Yes","Yes"] / sum( table(knn.pred,test.Y)["Yes",] )
    tab <- table(pred, test.Y.cv)
    
    if (sum(tab["Yes", ] != 0 )) {
      fold.prec[i] <- tab["Yes","Yes"] / sum(tab["Yes", ])
    } else {
      fold.prec[i] <- 0
    }
  }
  
  # average CV precision for this k
  cv.prec[j] <- mean(fold.prec)
}


# choose the best k
cv.prec
best_k = k.values[which.max(cv.prec)]
best_k
# fit the final model using the full training set and best k
knn.pred <- knn(train = train.X,
                test = test.X,
                cl = train.Y,
                k = best_k)

# final model using full training set
knn.pred <- knn(train = train.X,
                test = test.X,
                cl = train.Y,
                k = best.k)

table(Predicted = knn.pred, Actual = test.Y)
mean(knn.pred == test.Y)



