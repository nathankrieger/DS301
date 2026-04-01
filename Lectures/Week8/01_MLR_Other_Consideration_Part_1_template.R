

# Categorical predictors - real data
# Multicollinearity - real and fake data


##############################
### Qualitative predictors ###
##############################

library(ISLR2)
head(Credit)
str(Credit)

# Fit a regression model with qualitative predictor "Region"

m1 <- lm(Balance ~ Limit + Region, data = Credit)

# What are the fitted regression lines for each category of region? 

## Can change baseline:
Credit$Region <- relevel(Credit$Region, ref = "East")

m2 <- lm(Balance ~ Limit + Region, data = Credit)

# Compare models with and without "Region"
fit1 <- lm(Balance~Limit+Region+Student+Married,data=Credit)
fit2 <- lm(Balance~Limit+Student+Married,data=Credit)

# Conduct a partial F-test to determine if "Region" improves the model

anova(fit1, fit2) # These predictors are bad but that's not because they are categorical


##############################
##### Multicollinearity ######
##############################

##### simulation illustrating perfect correlation 

x1 = rnorm(100, mean=70, sd=15)
x2 = rnorm(100, mean=70, sd=15)

# Add in a linear combination of X1 and X2
x3 = (x1+x2)/2

# X4 is somewhat correlated with X1 but not relevant to Y
x4 = x1+runif(100,min=-2,max=2)

# Y is a linear combination of X1 and X2 plus noise
y = 0.7*x1 + 0.3*x2 + rnorm(100, 0, sqrt(15))


# Fit models with and without x3
m1 = lm(y~x1+x2+x3+x4) #

# x3 is NA because it doesn't add any new info at all. Its just x1 and x2
summary(m1)

m2 = lm(y~x1+x2+x4)  # 
summary(m2)

#install.packages("car")
library(car)

# Check for multicollinearity using VIF
m1 = lm(y~x1+x2+x3+x4)
vif(m1)

m2 = lm(y~x1+x2+x4)
vif(m2)

# VIF Values:
# 1 -> no multicollinearity
# 1<x<5 -> moderate correlation (acceptable)
# >5 -> high multicollinearity (bad)
# >10 -> sever multicollinearity

#######################
#### data example #####
#######################

library(ISLR2)
head(Credit)

## Rating and limit are highly correlated
# Visualize the correlation between Rating and Limit

plot(Rating~Limit,Credit)

fit0 = lm(Balance~Age+Limit+Rating, data=Credit)
summary(fit0)

fit1a = lm(Balance~Age+Limit,data=Credit)
summary(fit1a)

fit1b = lm(Balance~Age+Rating,data=Credit)
summary(fit1b)

fit2 = lm(Balance~Rating+Limit,data=Credit) 
summary(fit2)

# Check for multicollinearity using VIF
vif(fit2)

## Simple solutions to multicollinearity? 

########################
## In-class Activity  ##
########################

## Download the insurance.csv file on Canvas. 
## You can load your data into R using: 
#insurance=read.csv("/.../insurance.csv") 

insurance = read.csv(file.choose(), header = TRUE)
## Specify your pathway to be where you saved this file. 

## 1. This data set contains a few categorical predictors. 
## 1.1 Check that all the categorical predictors in our dataset
##     are stored correctly using str()
str(insurance)

## 1.2 If they are not, fix it. Copy and paste your output here. 
## Hint: change predictors to factor predictor with as.factor function
## Example, df$age<- as.factor(df$age)

insurance$gender <- as.factor(insurance$gender)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

str(insurance)

## 2. 
## 2.1 Fit a model with the response (Y) as health care charges and predictors
## x_1 = age, x2 = bmi, and x3 = gender. 

# m1 <- lm(y ~ age + bmi + gender, data = insurance)
fit <- lm(charges ~ age + bmi + gender, data = insurance)
summary(fit)

## 2.2 Based on your output, write out the fitted model's equation
##     for males only (gendermale = 1)

# charges = -6986.82 + 243.19 * insurance$age + 327.54 * insurance$bmi + 1344.54*gendermale

## 2.3 write out the fitted model's equation for females only (gendermale = 0). 

# charges = -6986.82 + 243.19 * insurance$age + 327.54 * insurance$bmi

## 3. Your classmate tells you that including gender as a dummy variable in the model is not necessary. 
## Instead you can just fit a model for males only and a separate model for females only.
## Your classmate claims this is approach is equivalent to what you did in part 2. 

## 3.1. Subset your data into two groups: data for males and data for females. 
##  HINT for filtering a dataframe 

###################################################################################################
# WRONG WAY TO DO IT:

males <- insurance[insurance$gender=='male',]
females <- insurance[insurance$gender=='female',]

## 3.2. Fit a model with bmi and age for the male group only. Call this model fit_males. 
##  HINT for filtering a dataframe 
## males <- subset(insurance, sex == "male")

fit_males <- lm(charges ~ age + bmi, data = males)
summary(fit_males)

## 3.3 Now do the same for the female group. Call this model fit_females. 

fit_females <- lm(charges ~ age + bmi, data = females)
summary(fit_females)

## 3.4 Based on your output, write out both model's estimated regression coefficients.

#     males:    charges = -8012.79 + 238.63 * age + 409.87 * bmi
#     females:  charges = -4515.22 + 246.92 * age + 241.32 * bmi

## 4. Compare your results in part 2 with part 3. 
## 4.1 Are they equivalent? 

# no

## 4.2 Explain in plain language to your classmate 
## why these two approaches will not give the same results. 

# the model is trained on different subsets of data
