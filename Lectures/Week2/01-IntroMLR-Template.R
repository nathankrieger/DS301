############################################################
# Multiple Linear Regression (MLR): Patient Satisfaction
############################################################

########################
# 1) Load the dataset  #
########################

patient <- read.table(file.choose(), header = FALSE)


# Quick check

# Assign column names

head(patient)

names(patient) <- c("satisf", "age", "severe", "anxiety")

head(patient)

#########################
# 2) Exploratory plot 
#########################

pairs(patient)

############################
# 3) Fit regression model  #
############################

# Method 1

m1 <- lm(satisf ~ age + severe + anxiety, data = patient)



# Method2 Shortcut: fit the model using ALL other variables as predictors

# m1 <- lm(satisf ~ ., data = patient)

############################################
# 4) Model output: coefficients and residuals
############################################

# Print a full summary 

summary(m1)

# Show the names of components stored in the lm object

names(m1)

# Extract least squares regression coefficients

m1$coefficients

# Extract fitted values (predicted y-hat for each observation)

m1$fitted.values

# Extract residuals: observed y - fitted y



########################
# 5) Prediction examples
########################

# Predict satisfaction for a new patient with specific predictor values




############################################################
# In-class Activity
############################################################

# 1)Fit a linear regression model with the response as patient satisfaction
# and use all other variables as predictors. Report the least square regression coefficients. 

m2 <- lm(satisf ~ ., data = patient)
m2$coefficients

# 2) Predicted satisfaction for observations 1, 3, and 20

m2$fitted.values[c(1, 3, 20)]

# 3) Predicted satisfaction for a patient with:
#    age = 50, severe = 27, anxiety = 3

new_patient <- data.frame(age = 50, severe = 27, anxiety = 3)
predict(m2, newdata = new_patient)

# 4) Propose a way to quantify how well our model is able to predict patient 
# satisfaction (Y).

summary(m2)$r.squared

############################################################
# Group work reminder
############################################################
# Work in groups to propose a way to quantify how well the model
# predicts patient satisfaction (Y). Post relevant code + group
# member names (only one person needs to post).
#
# Post your solution to Piazza: In -class activity 1

############################################################