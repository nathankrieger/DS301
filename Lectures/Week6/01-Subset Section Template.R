
# Real data regfit
# AIC, BIC, Cp, Adjr^2 calculations


############################
##### Subset Selection #####
############################

# Install the 'leaps' package for subset selection if not already installed
# install.packages('leaps')

# Load necessary libraries
library(leaps)   # For best subset selection
library(ISLR2)   # Contains the 'Hitters' dataset

# Display the first few rows of the dataset
head(Hitters)
ncol(Hitters)
# Display information about Hitter
?Hitters

# Check the dimensions
n = dim(Hitters)[1]  # Number of observations

############################
##### Missing Value Check #####
############################

# Check for missing values in the entire dataset

is.na(Hitters)

# Total count of missing values in the dataset

sum(is.na(Hitters))

# Count of missing values per column

colSums(is.na(Hitters))

# Remove rows with missing values

Hitters <- na.omit(Hitters)

# Verify that missing values have been removed

dim(Hitters)

n = dim(Hitters)[1]  # Number of observations

############################
##### Best Subset Selection #####
############################
# Perform Best Subset Selection
# - Salary is the response variable
# - All other predictors are considered
# - nbest = 1 (only one best model per subset size)
# - nvmax = 19 (search up to 19 predictors)


# Get the summary of the subset selection results

regfit <- regsubsets(Salary ~ ., data = Hitters, nbest = 1, nvmax = 19)
regfit

# Display available metrics in the summary object

regfit_summary <- summary(regfit)

# Extract model selection metrics


names(regfit_summary)
regfit_summary$which

# Adjusted R-squared values

adjr2 <- regfit_summary$adjr2

# Mallows' Cp statistic

cp <- regfit_summary$cp

# Residual Sum of Squares (RSS)

rss <- regfit_summary$rss
  
# Number of predictors used in each subset

p <- rowSums(regfit_summary$which)

# AIC.

aic <- n*log(rss / n) + 2*p

# BIC (Bayesian information criterion)

bic <- n*log(rss / n) + p*log(n)

# !!! Don't use regfit.sum$bic 
# regfit.sum$bic is Schwartz's information criterion
# !!! NOT Bayesian information criterion



# Display results in a table format
cbind(p, rss, adjr2, cp, aic, bic)

# Plot BIC and AIC values to visualize model selection
plot(p, bic)
plot(p, aic)

# Identify the best model based on different selection criteria
# Find model with the lowest BIC 


# Find model size that minimizes AIC
which.min(aic)

# Find model size that minimizes Cp
which.min(cp)

# Find model size that maximizes Adjusted R-squared
which.max(adjr2)

# Extract and display the coefficients for selected models with different subset sizes
# Displays the coefficients for the best model with 1 predictor

coef(regfit, 1)

# Displays the coefficients for the best model with 3 predictor
coef(regfit, which.min(aic))
