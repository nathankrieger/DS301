
# mock data (x, y) with MULTIPLE x, confidence intervals

############################################
## Setup
############################################
library(ISLR2)

# Fit the model (given)
m1 = lm(medv ~ crim + lstat, data = Boston)


############################################
## Hypothesis tests
############################################

# Goal: extract t-values and p-values for all coefficients

# 1) Look at the full summary output

summary(m1)


# 2) Extract all t-values

summary(m1)$coefficients[ , 3] # for all rows, get column 3

# 3) Extract all p-values

summary(m1)$coefficients[ , 4]

# 4) Count how many coefficients are significant at alpha = 0.05

p_values <- summary(m1)$coefficients[ , 4] 
sum(p_values < 0.05)

# 5) Write a conclusion in words (alpha = 0.05)

# The intercept and lstat columns are significant.
# The null hypothesis for beta 1 does not get rejected

############################################
## Confidence intervals
############################################

# 99% confidence intervals for beta0, beta1, beta2

confint(m1, "crim", level = 0.99)
confint(m1, level = 0.99)

############################################
## Confidence / prediction intervals
############################################

# For lstat = 5, crim = 0.5:
new_point = data.frame(lstat = 5, crim = 0.5)

# 95% confidence interval for mean response E(Y | X)

predict(m1, new_point, interval = 'confidence', level = 0.95)

# 95% prediction interval for a new Y

predict(m1, new_point, interval = 'prediction', level = 0.95)


########################
## In-class Activity  ##
########################

#1. Simulate a dataset with 500 observations and 100 predictors, 
#set only the first 5 predictors to have non-zero coefficient, 
#and fit a multiple linear regression model. 
#The goal is to determine how many predictors are statistically significant
#at the 0.01 level using the p-value and verify that it is approximately 5.

set.seed(123)  # For reproducibility

# Step 1: Simulation settings(E.g: number of predicors, observations...)

n <- 500
p <- 100

# Step 2: Generate 100 random predictors (X matrix)
#Hint: Use matrix() to create a dataset of 100 predictor variables.
#Hint: Assign column names X1, X2, ..., X100 for easy reference.E.g: colnames(X) <- paste0("X", 1:p)

x <- matrix(rnorm(n * p), n, p)
colnames(x) <- paste0("x", 1:p)

#Step 3: Generate coefficients (3 for first 5 true_predictors, 0 for the rest)
# Hint: Assign a coefficient of 3 for the first 5 predictors (true predictors).
# Hint: Set the remaining 95 predictors' coefficients to 0.
# Hint: use a vector for beta and rep function to assign values

beta <- c(rep(3, 5), rep(0, p - 5))


# Step 4: Generate response variable Y
#Hint: Use matrix multiplication (X %*% beta)
#Hint: Don't forget the random error

error <- rnorm(n)
y <- x %*% beta + error

#Step 5: Fit a Multiple Linear Regression Model
#Hint: Convert X into a data frame so it can be used in lm()
#Hint: Add Y as a new column to the data frame

df <- data.frame(y, x)
m2 <- lm(y ~ ., data = df)

#Step 6: Extract p-values for all predictors

summary <- summary(m2)
p_val <- summary$coefficients[-1, 4]

#Step 7: Count number of significant predictors at 0.01 level

count <- sum(p_val < 0.01)
print(count)