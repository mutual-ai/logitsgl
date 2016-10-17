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

lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)

lambda <- logitsgl.lambda(X, Y, alpha = 1, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 1, lambda = lambda)

# Test dens - sparse
Y <- Matrix(Y, sparse = TRUE)

lambda <- logitsgl.lambda(X, Y, alpha = 0, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 0, lambda = lambda)

lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)

lambda <- logitsgl.lambda(X, Y, alpha = 1, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 1, lambda = lambda)

# Test sparse - sparse
X <- Matrix(X, sparse = TRUE)

lambda <- logitsgl.lambda(X, Y, alpha = 0, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 0, lambda = lambda)

lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)

lambda <- logitsgl.lambda(X, Y, alpha = 1, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 1, lambda = lambda)

# Test sparse - dense
Y <- as.matrix(Y)

lambda <- logitsgl.lambda(X, Y, alpha = 0, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 0, lambda = lambda)

lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)

lambda <- logitsgl.lambda(X, Y, alpha = 1, lambda.min = 0.8)
fit <- logitsgl(X, Y, alpha = 1, lambda = lambda)

# Test intercept = FALSE
lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.8, intercept=FALSE)
fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda,  intercept=FALSE)

# Test single fit i.e. K = 1
y <- Y[,1]

lambda<-logitsgl.lambda(X, y, alpha=0.5, lambda.min=0.8, intercept=FALSE)
fit <-logitsgl(X, y, alpha=1, lambda = lambda, intercept=FALSE)
res <- predict(fit, X)


### Navigation tests
print(res)
print(fit)
features_stat(fit)
parameters_stat(fit)

### Test for errors if X or Y contains NA
Xna <- X
Xna[1,1] <- NA

res <- try(

  lambda < -logitsgl.lambda(Xna, Y, alpha=1, lambda.min=.5, intercept=FALSE),

  silent = TRUE)

if(class(res) != "try-error") stop()

res <- try(

  fit <-logitsgl(Xna, Y, alpha=1, lambda = lambda, intercept=FALSE),

  silent = TRUE)

if(class(res) != "try-error") stop()

Yna <- Y
Yna[1,1] <- NA

res <- try(

  lambda<-logitsgl.lambda(X, Yna, alpha=1, lambda.min=.5, intercept=FALSE),

  silent = TRUE)

if(class(res) != "try-error") stop()

res <- try(

  fit <-logitsgl(X, Yna, alpha=1, lambda = lambda, intercept=FALSE),

  silent = TRUE)

if(class(res) != "try-error") stop()
