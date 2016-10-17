library(logitsgl)

# warnings = errors
options(warn=2)

data(birds)

# down scale
X <- X[1:200, 1:25]
Y <- Y[1:200, 1:3]

# Simple standardization
X <- scale(X)

grouping <- rep(LETTERS[1:5],5)

lambda<-logitsgl.lambda(X,Y, grouping = grouping, alpha=0.5, lambda.min=0.8)

fit <-logitsgl(X,Y, grouping = grouping, alpha=0.5, lambda = lambda)

# Test single fit i.e. K = 1
y <- Y[,1]

lambda <- logitsgl.lambda(X,y, grouping = grouping, alpha=0.5, lambda.min=0.8, intercept = FALSE)
fit <-logitsgl(X, y, grouping = grouping, alpha=0.5, lambda = lambda, intercept = FALSE)
res <- predict(fit, X)
