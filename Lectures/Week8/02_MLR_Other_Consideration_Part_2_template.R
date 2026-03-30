library(ISLR2)
head(Auto)

plot(Auto$mpg, Auto$horsepower)

# Fit a simple linear regression model: mpg ~ horsepower
m1 = lm(mpg ~ horsepower, data = Auto)

summary(m1)

#Sets the plotting area to 2×2
par(mfrow=c(2,2))
##plot(m1) generates diagnostic plots to assess model assumptions:
# 1. Residuals vs. Fitted: Checks for non-linearity.
# 2. Normal Q-Q: Checks normality of residuals.
# 3. Scale-Location: Checks homoscedasticity (constant variance).
# 4. Residuals vs. Leverage: Identifies influential points.

plot(m1)

##################################
#### Exploring Transformations ###
##################################

# More of an art than a science
par(mfrow=c(2,2))

plot(m1, which=3)# Extract only the third plot: scale-location residual plot 

# log(mpg)
m2 <- lm(log(mpg)~horsepower,data=Auto)
plot(m2, which = 3)# Extract only the third plot: scale-location residual plot 

# log(horsepower)
m3 <- lm(mpg ~ log(horsepower),data=Auto)
plot(m3, which=3)

# horsepower^2
m4 <- lm(mpg~ (horsepower^2),data=Auto)
plot(m4, which=3)



##################################
##### Polynomial Regression ######
##################################

#poly() in R orthogonalizes these polynomial terms
# meaning it creates uncorrelated versions of X, X^2, X^3 .....

# Fit a 2nd-degree polynomial regression model
m_new = lm(mpg~poly(horsepower,2),data=Auto) 
summary(m_new)

# Diagnostic plots for the polynomial regression model
par(mfrow=c(2,2))
plot(m_new)

# Compare residual plots: Linear vs. Polynomial regression
par(mfrow=c(1,2))
plot(m1,which=3)# Residual plot for simple linear regression
plot(m_new,which=3)# Residual plot for polynomial regression




##################################
##### Wage Dataset Analysis ######
##################################

# Load and inspect the first few rows of the Wage dataset
head(Wage)

# Scatter plot of age vs. wage to visualize the relationship
plot(Wage$age, Wage$wage)

## How to decide on the appropriate polynomial degree?

### Option 1: Using Hypothesis Tests (ANOVA)
# Fit polynomial regression models of increasing degree
fit.1 <- lm(wage ~ age, data = Wage)          # Linear model
fit.2 <- lm(wage ~ poly(age,2), data = Wage)  # Quadratic model
fit.3 <- lm(wage ~ poly(age,3), data = Wage)  # Cubic model
fit.4 <- lm(wage ~ poly(age,4), data = Wage)  # 4th-degree polynomial
fit.5 <- lm(wage ~ poly(age,5), data = Wage)  # 5th-degree polynomial

# Compare models using ANOVA to test whether adding higher-degree terms improves fit
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Interpretation:
# Lower-degree models may underfit, while higher-degree models are not justified.
# The quadratic term clearly helps, the cubic term also helps,
# but the 4th and 5th degree terms do not add much improvement.
# A cubic or quadratic polynomial appears to provide a reasonable fit to the data.

### Option 2: Comparing Training and Test Mean Squared Error (MSE)
set.seed(10)  # Set seed for reproducibility
n = dim(Wage)[1]

# Split data into training (50%) and test (50%) sets
train_index <- sample(1:n, n/2, replace = FALSE)
train_wage <- Wage[train_index,]
test_wage <- Wage[-train_index,]

# Fit polynomial models on the training data
fit.1 <- lm(wage ~ age, data = train_wage)
fit.2 <- lm(wage ~ poly(age,2), data = train_wage)
fit.3 <- lm(wage ~ poly(age,3), data = train_wage)
fit.4 <- lm(wage ~ poly(age,4), data = train_wage)
fit.5 <- lm(wage ~ poly(age,5), data = train_wage)

# Compute training MSE for each model
MSE_train1 <- mean((train_wage$wage - fit.1$fitted.values)^2) 
MSE_train2 <- mean((train_wage$wage - fit.2$fitted.values)^2) 
MSE_train3 <- mean((train_wage$wage - fit.3$fitted.values)^2) 
MSE_train4 <- mean((train_wage$wage - fit.4$fitted.values)^2) 
MSE_train5 <- mean((train_wage$wage - fit.5$fitted.values)^2) 

# Plot training MSE vs. polynomial degree
degree <- c(1:5)

plot(degree, c(MSE_train1, MSE_train2, MSE_train3, MSE_train4, MSE_train5), type = 'b', 
     xlab = "Polynomial Degree", ylab = "Training MSE")


## Compute Test MSE for polynomial models up to degree 11
test_MSE = rep(NA, 11)

for(i in 1:11){
  fit = lm(wage ~ poly(age, degree = i), data = train_wage)
  predicted_values = predict(fit, newdata = test_wage)
  test_MSE[i] = mean((test_wage$wage - predicted_values)^2)
}

## Plot Test MSE vs. Polynomial Degree
plot(1:11, test_MSE, type = "b", pch = 19, col = "blue", 
     xlab = "Polynomial Degree", ylab = "Test MSE")
