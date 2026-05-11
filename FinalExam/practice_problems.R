# Practice problems




head(Hitters)
df = data.frame(Hitters)




Hitters = no.omit(Hitters)


train_index = sample(1:nrow(hitters), nrow(hitters) / 2)

train = Hitters[train_index, ]
test = Hitters[-train_index, ]

fit <- tree(log(Salary) ~ ., data = train)

summary(fit)

plot(fit)
text(fit, pretty = 0)


# 9 terminal nodes