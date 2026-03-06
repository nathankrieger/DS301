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

anova(fit1, fit2)


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

insurance = read.csv("~/git/DS301/Lectures/Week7/insurance.csv")
## Specify your pathway to be where you saved this file. 

## 1. This data set contains a few categorical predictors. 
## 1.1 Check that all the categorical predictors in our dataset
##     are stored correctly using str()

str(insurance)

## 1.2 If they are not, fix it. Copy and paste your output here. 
## Hint: change predictors to factor predictor with as.factor function
## Example, df$age<- as.factor(df$age)

insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)
insurance$gender <- as.factor(insurance$gender)

#'data.frame':	1338 obs. of  7 variables:
#$ age     : Factor w/ 47 levels "18","19","20",..: 2 1 11 16 15 14 29 20 20 43 ...
#$ gender  : Factor w/ 2 levels "female","male": 1 2 2 2 2 1 1 1 2 1 ...
#$ bmi     : num  27.9 33.8 33 22.7 28.9 ...
#$ children: Factor w/ 6 levels "0","1","2","3",..: 1 2 4 1 1 1 2 4 3 1 ...
#$ smoker  : Factor w/ 2 levels "no","yes": 2 1 1 1 1 1 1 1 1 1 ...
#$ region  : Factor w/ 4 levels "northeast","northwest",..: 4 3 3 2 2 3 3 2 1 2 ...
#$ charges : num  16885 1726 4449 21984 3867 ...

## 2. 
## 2.1 Fit a model with the response (Y) as health care charges and predictors
## x_1 = age, x2 = bmi, and x3 = gender. 

m <- lm (charges ~ age + bmi + gender, data = insurance)

summary(m)

## 2.2 Based on your output, write out the fitted model's equation
##     for males only (gendermale = 1)

#charges = -6986.82 + 243.19 * age + 327.54 * bmi + 1344.46 * gendermale

## 2.3 write out the fitted model's equation for females only (gendermale = 0). 

#charges = -6986.82 + 243.19 * age + 327.54 * bmi + 1344.46 * 0

## 3. Your classmate tells you that including gender as a dummy variable in the model is not necessary. 
## Instead you can just fit a model for males only and a separate model for females only.
## Your classmate claims this is approach is equivalent to what you did in part 2. 

## 3.1. Subset your data into two groups: data for males and data for females. 
##  HINT for filtering a dataframe 

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




## 4. Compare your results in part 2 with part 3. 
## 4.1 Are they equivalent? 



## 4.2 Explain in plain language to your classmate 
## why these two approaches will not give the same results. 


