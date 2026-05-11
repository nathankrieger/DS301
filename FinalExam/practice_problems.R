# Practice problems


set.seed(1)


head(Hitters)
df = data.frame(Hitters)




Hitters = na.omit(Hitters)


train_index = sample(1:nrow(Hitters), nrow(Hitters) / 2)

train = Hitters[train_index, ]
test = Hitters[-train_index, ]

fit <- tree(log(Salary) ~ ., data = train)

summary(fit)

plot(fit)
text(fit, pretty = 0)


# 8 terminal nodes


pred <- predict(fit, newdata = test)
test_mse <- mean((pred - test$Salary)^2)
test_mse
