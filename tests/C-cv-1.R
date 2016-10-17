library(logitsgl)

# warnings = errors
options(warn=2)

data(birds)

# down scale
X <- X[1:200, ]
Y <- Y[1:200, 1:3]

# Simple standardization
X <- scale(X)

# Do cross validation
lambda <- logitsgl.lambda(X, Y, alpha = 0.5, d = 25, lambda.min = 0.8)
fit.cv <- logitsgl.cv(X, Y, alpha = 0.5, lambda = lambda)

# TODO test error

## Test single fit i.e. K = 1
y <- Y1[,1]

lambda <- logitsgl.lambda(X, y, alpha = 0.5, d = 25, lambda.min = 0.5)
fit.cv <- logitsgl.cv(X, y, alpha = 0.5, lambda = lambda)

## Navigation tests
Err(fit.cv)
features_stat(fit.cv)
parameters_stat(fit.cv)
best_model(fit.cv)

### Test for errors if X or Y contains NA
Xna <- X
Xna[1,1] <- NA

res <- try(

  fit.cv <- logitsgl.cv(Xna, Y, alpha = 1, lambda = lambda),

  silent = TRUE)

if(class(res) != "try-error") stop()

Yna <- Y
Yna[1,1] <- NA

res <- try(

  fit.cv <- logitsgl.cv(X1, Yna, alpha = 0.5, lambda = lambda),

  silent = TRUE)

if(class(res) != "try-error") stop()
