alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))}

## run this on the Portfolio data
library(ISLR2)
alpha.fn(Portfolio,1:100)
