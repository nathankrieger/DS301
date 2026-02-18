
# using training and testing data, finding MSE, simulating data with X and Y, training a model on it, checking for unbiasedness


#########################################################
######### Accuracy of our prediction? ################### 

## 1) Load data

patient <- read.table(file.choose(), header = FALSE)

names(patient) = c("satisf","age","severe","anxiety")

## Quick check

str(patient)

# Get the number of rows in the dataset
n <- nrow(patient)

## 2) divide our data into a training set and a test set (50% training, 50% testing)

#sample(1:100,10)
train_index <- sample(1:n, floor(n / 2), replace = FALSE)


# Create training and testing datasets

train_patients <- patient[train_index, ]
test_patients <- patient[-train_index, ]


## 3) Fit model on training set

m1 <- lm(satisf ~ ., data = train_patients)

summary(m1)

## 4) Training MSE (use predict() for consistency)


trainMSE <- sum(m1$residuals^2)/nrow(train_patients)

## 5) Test MSE

# Predict the response variable for the test set using the trained model

test_predict <- predict(m1, newdata = test_patients)


# Calculate Mean Squared Error (MSE) for the testing data

test_MSE <- mean((test_predict - test_patients$satisf)^2)

#Q: Will the training MSE always be smaller than the test MSE?

# Usually.

#########################################################
####### Properties of our linear regression model #######
#########################################################

## Let's simulate data where we know the true population 
## regression line

## Simple linear regression 
## suppose we have 1 single predictor (X1)

beta_0 <- 3
beta_1 <- 5
n <- 100

X1 <- seq(0, 10, length.out = n)

## Generate (or sample) Y based on true population regression line
## Hint: Y = beta_0 + beta_1*X1 + error
 ## generate 100 error values from 
## normal distribution with mean 0 and sd 1.

error = rnorm(n,0,1)
y <- beta_0 + beta_1 * X1 + error

#plot(X1,y)

## Then we use this sample to estimate the least square line. 
## If we were to take a different sample of Y, we would get a different least square line
## and different estimates for beta0 and beta1. Re-run lines 42 - 59 many times to see that the
## estimate coefficients would change. 

model <- lm(y ~ X1)
model$coefficients

## We hope that over many many many samples, on average, our estimates 
## would exactly equal the truth. 
## This is the idea of an unbiased estimator.
## How can we check this using simulations?  

########################
## In-class Activity  ##
########################

## 1. Design a simulation to check whether or not our least square estimates for beta_0 and beta_1 are unbiased. 

## HINTS:
## 1. Remember what "unbiased" means:
##    An estimator is unbiased if its average value (over many samples)
##    equals the true population parameter.

## 2. Since we only get ONE dataset in practice, we use simulation to
##    mimic the idea of "many samples".

## 3. Strategy:
##    - Fix the true values of beta_0 and beta_1.
##    - Fix the X values.
##    - Repeatedly generate new error terms similar to above

##    - Each time, generate a new Y using the population model.
##    - Fit a linear regression model and store the estimated coefficients.
##      Note: STORE the estimated coefficients (beta_0_hat and beta_1_hat)
##      in vectors.
##    - Each entry in the vector corresponds to one simulated dataset.

## 5. After the loop:
##    - Compute the average of the estimated beta_0 values.
##    - Compute the average of the estimated beta_1 values.


## Check beta_0 - the average of the estimated beta_0 (should be close to 0)
## Check beta_1 - the average of the estimated beta_1 (should be close to 0)


#plot a histogram of the distribution of beta_0


#plot a histogram of the distribution of beta_1 


## 2. After finishing HW 1, explain why using many simple linear regression models is not sufficient 
## compared to a multiple linear regression model.
## Note that fitting a simple linear regression model is computationally
## instantaneous since there are analytical solutions for our least square estimates. 




## Work in groups to come up with a solution. 
## Copy and paste any relevant code on Piazza. 
## Please be sure to list all your group members names. Only one group member needs to post on Discussion. 


# Step 1: Fix true parameters and X values
beta_0_true <- 3
beta_1_true <- 5
n <- 100
X1 <- seq(0, 10, length.out = n)

# Number of simulations (samples)
n_sim <- 1000

# Step 2: Create empty vectors to STORE results
beta_0_hats <- numeric(n_sim)
beta_1_hats <- numeric(n_sim)

# Step 3: Run the simulation loop
set.seed(123) # For reproducibility
for (i in 1:n_sim) {
  
  # Generate new error terms for each sample
  error <- rnorm(n, mean = 0, sd = 1)
  
  # Generate new Y based on the fixed true population model
  y_sim <- beta_0_true + beta_1_true * X1 + error
  
  # Fit the model on this specific sample
  model_sim <- lm(y_sim ~ X1)
  
  # Store the estimates
  beta_0_hats[i] <- model_sim$coefficients[1]
  beta_1_hats[i] <- model_sim$coefficients[2]
}

# Step 4: Compute averages to check for unbiasedness
avg_beta_0 <- mean(beta_0_hats)
avg_beta_1 <- mean(beta_1_hats)

cat("True Beta_0:", beta_0_true, "| Average Estimate:", avg_beta_0, "\n")
cat("True Beta_1:", beta_1_true, "| Average Estimate:", avg_beta_1, "\n")

# Step 5: Visualize the distributions
par(mfrow = c(1, 2)) # Side-by-side plots

hist(beta_0_hats, main = expression(paste("Distribution of ", hat(beta)[0])), 
     xlab = "Estimated Beta_0", col = "lightblue")
abline(v = beta_0_true, col = "red", lwd = 2) # Red line at the truth

hist(beta_1_hats, main = expression(paste("Distribution of ", hat(beta)[1])), 
     xlab = "Estimated Beta_1", col = "lightgreen")
abline(v = beta_1_true, col = "red", lwd = 2) # Red line at the truth

