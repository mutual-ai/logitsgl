library(logitsgl)

# warnings = errors
options(warn=2)

data(birds)

# down scale
X <- X[1:200, ]
Y <- Y[1:200, 1:3]

# Simple standardization
X <- scale(X)

# Test dens - dens
lambda <- logitsgl.lambda(X, Y, alpha = 0, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 0, lambda = lambda)

# print info
fit

# Test features
features(fit)
features_stat(fit)

# parameters
parameters(fit)
parameters_stat(fit)

nmod(fit)

models(fit)

coef(fit)
