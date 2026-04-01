############################
## Bootstrap Illustration ##
############################

library(ISLR2)

head(Auto)


########################################################
## Linear Regression: Standard Error and Confidence Interval
########################################################


# Fit the original linear regression model
# Response: mpg, Predictor: horsepower
fit_orig <- lm(mpg ~ horsepower, data = Auto)

summary(fit_orig)

summary(fit_orig)$coef

# beta_0 hat from original lm
beta0_star <- summary(fit_orig)$coef[1,1]

# Standard error of beta_0 from original lm
se_b0_star <- summary(fit_orig)$coef[1,2]

par(mfrow = c(2, 2))
plot(fit_orig)


###############################
## Bootstrap Standard Error  ##
###############################

# Sample size
n <- nrow(Auto)

# Number of bootstrap samples
B <- 2000

# Store bootstrap estimates of the intercept
beta0_boot <- numeric(B)


for (b in 1:B) {
  boot_index <- sample(1:n, size = n, replace = TRUE)
  boot_sample <- Auto[boot_index, ]
  
  fit_boot <- lm(mpg ~ horsepower, data = boot_sample)
  beta0_boot[b] <- fit_boot$coef[1]
}
# Bootstrap estimate of the standard error of beta_0
mean(beta0_boot)

se_beta0_boot <- sd(beta0_boot)
se_beta0_boot

# Compare with analytical standard error from lm()
se_b0_star

##################################################
## Bootstrap confidence interval for intercept  ##
##################################################

# Original estimate and analytical SE
beta0_hat <- coef(fit_orig)[1]
se_beta0_hat <- summary(fit_orig)$coef[1, 2]

# Outer bootstrap repetitions
B <- 500

# Inner bootstrap repetitions
M <- 100

# Store studentized bootstrap statistics
t_star <- numeric(B)

for (b in 1:B) {
  # First-level bootstrap sample
  boot_index_1 <- sample(1:n, size = n, replace = TRUE)
  boot_sample_1 <- Auto[boot_index_1, ]
  
  fit_boot_1 <- lm(mpg ~ horsepower, data = boot_sample_1)
  beta0_b <- coef(fit_boot_1)[1]
  
  # Second-level bootstrap to estimate SE within this bootstrap sample
  beta0_inner <- numeric(M)
  
  for (m in 1:M) {
    boot_index_2 <- sample(1:nrow(boot_sample_1), size = nrow(boot_sample_1), replace = TRUE)
    boot_sample_2 <- boot_sample_1[boot_index_2, ]
    
    fit_boot_2 <- lm(mpg ~ horsepower, data = boot_sample_2)
    beta0_inner[m] <- coef(fit_boot_2)[1]
  }
  
  se_beta0_b <- sd(beta0_inner)
  
  # Studentized bootstrap statistic
  t_star[b] <- (beta0_b - beta0_hat) / se_beta0_b
}

# 95% bootstrap-t confidence interval
quantile(t_star, c(0.025, 0.975))

ci_lower <- beta0_hat + quantile(t_star, 0.025) * se_beta0_hat
ci_upper <- beta0_hat + quantile(t_star, 0.975) * se_beta0_hat

c(ci_lower, ci_upper)

# Compare with analytical confidence interval
confint(fit_orig)

########################
## In-class Activity  ##
########################
  
## Let's practice bootstrap on the Boston dataset (part of library(ISLR2))
## (1) Based on this data set, provide an estimate for the population mean of medv. Call this muhat.
## (2) Estimate the standard error of muhat using bootstrap.
## (3) Compare this to the standard error of the estimated mean of medv using the analytical formula (the one you've seen in intro stats). 
## How do they compare? 


library(ISLR2)

########################
## In-class Activity  ##
########################

# Load data
head(Boston)

# Sample size
n <- nrow(Boston)

# (1) Estimate the population mean of medv
mu_hat <- mean(Boston$medv)
mu_hat

# (2) Use bootstrap to estimate the standard error of mu_hat (medv mean)
B <- 2000

boot_mean <- numeric(B) # (something to store boot mean, how many do we have ?)

for (b in 1:B) {
  boot_index <- sample(1:n, size = n, replace = TRUE)
  boot_sample <- Boston$medv[boot_index]
  
  boot_mean[b] <- mean(boot_sample)
}

se_boot <- sd(boot_mean)
se_boot

# (3) Analytical standard error
# SE(mean) = s / sqrt(n)
# Intuition: the sample mean is an average, and averaging reduces variability.
# As sample size increases, the mean becomes more stable.
# The variability shrinks at a rate of 1/sqrt(n).
# We use sample sd (s = sd(Boston$medv)) to estimate unknown population sd (sigma).
se_analytic <- sd(Boston$medv) / sqrt(n)
se_analytic

# Compare the two
c(bootstrap_SE = se_boot, analytic_SE = se_analytic)


## Work in groups to come up with a solution. 
## Please be sure to list all your group members names. Only one group member needs to post on Ed Discussion. 
