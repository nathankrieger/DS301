library(ISLR2)   # contains the Smarket dataset
library(MASS)    # commonly used for classification methods such as LDA and QDA
library(e1071)   # provides the naiveBayes() function
library(klaR)    # provides NaiveBayes() with kernel density estimation option
install.packages('klaR')

head(Smarket)
# The Smarket data contains daily percentage returns for the S&P 500 from 2001 to 2005.
# Variables include:
# Lag1 - Lag5: percentage returns for the previous 1 to 5 trading days
# Volume: number of daily shares traded in billions
# Today: percentage return for the current day
# Direction: whether the market went Up or Down

# Create training and test data frames
Smarket_train <- Smarket[Smarket$Year < 2005, ] # data before 2005
Smarket_test  <- Smarket[Smarket$Year == 2005, ] # data from 2005


############################################################
# Naive Bayes using e1071
############################################################

# Default implementation assumes each quantitative predictor
# follows a normal distribution within each class
nb_fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = Smarket_train)

nb_fit

# If you want to manually check the class-specific means and SDs:
# mean(Smarket_train$Lag1[Smarket_train$Direction == "Down"])
# sd(Smarket_train$Lag1[Smarket_train$Direction == "Down"])
# mean(Smarket_train$Lag1[Smarket_train$Direction == "Up"])
# sd(Smarket_train$Lag1[Smarket_train$Direction == "Up"])

# Predicted class labels
nb_class <- predict(nb_fit, Smarket_test)

# Confusion matrix
table(nb_class, Smarket_test$Direction)

# Classification accuracy
mean(nb_class == Smarket_test$Direction)

# Posterior probabilities
nb_prob <- predict(nb_fit, Smarket_test, type = "raw")
head(nb_prob)

############################################################
# Naive Bayes with kernel density estimation using klaR
############################################################

# This version does not force a normal density assumption
# and instead estimates densities using kernels
nb_fit2 <- NaiveBayes(Direction ~ Lag1 + Lag2,
                      data = Smarket_train,
                      usekernel = TRUE)

# Predicted classes
nb2_pred <- predict(nb_fit2, Smarket_test)
nb2_class <- nb2_pred$class

# Posterior probabilities
nb2_prob <- nb2_pred$posterior

# Confusion matrix
table(nb2_class, Smarket_test$Direction)

# Classification accuracy
mean(nb2_class == Smarket_test$Direction)

# First few posterior probabilities
head(nb2_prob)
head(nb2_class)
