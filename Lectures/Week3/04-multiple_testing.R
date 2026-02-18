# multiple predictors

#### Code is from In-class activity 3#######

# Run the simulation with alpha value: 0.01 and 0.05 
# to see how it affects the significant_count (type I error number)  ?

# Step 1: Simulation settings
n = 1000  # Number of observations
p = 100  # Number of predictors
true_predictors = 5  # Number of predictors with non-zero coefficient
alpha = 0.05

# Step 2: Generate 100 random predictors (X matrix)
#Hint: Use matrix() to create a dataset of 100 predictor variables.
#Hint: Assign column names X1, X2, ..., X100 for easy reference.E.g: colnames(X) <- paste0("X", 1:p)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- paste0("X", 1:p)
colnames(X)

#Step 3: Generate coefficients (3 for first 5 true_predictors, 0 for the rest)
beta <- c(rep(2, true_predictors), rep(0, p - true_predictors))  
beta

# Step 4: Generate response variable Y
#Hint:Use matrix multiplication (X %*% beta)
#Hint: don't forget the random error

error = rnorm(n, 0, 1)  # Random noise
Y = X %*% beta + error  # Linear combination of X with noise

#Step 5: Fit a Multiple Linear Regression Model
#Hint: Convert X into a data frame so it can be used in lm()
#Hint: Add Y as a new column to the data frame
data = as.data.frame(X)
data$Y = Y
model <- lm(Y ~ ., data = data)

#Step 6: Extract p-values for all predictors
p_values <- summary(model)$coefficients[-1,4]  # use -1 to Remove intercept

#Step 7: Count number of significant predictors at 0.05 level
significant_count = sum(p_values < alpha)
# or 
significant_count = length(which(p_values < alpha))
significant_count

type_1_error = max(0, significant_count - 5)

cat("At", alpha, "significant level: \n",
    "number of significant predictors:", significant_count, "\n", 
    "number of type_1_error: ", type_1_error)

