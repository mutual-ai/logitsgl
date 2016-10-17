library(logitsgl)

# warnings = errors
options(warn=2)

data(birds)

# down scale
X <- X[1:200, ]
Y <- Y[1:200, 1:3]

# Simple standardization
X <- scale(X)

train <- list(1:100, 1:200)
test <- list(150:200, 150:200)

 lambda <- lapply(train, function(idx)
		logitsgl.lambda(X[idx,], Y[idx,], alpha = 0.5, lambda.min = 0.4))

# Do subsampling
 fit.sub <- logitsgl.subsampling(X, Y,
		alpha = 0.5,
		train = train,
		test = test,
		lambda = lambda,
	  use_parallel = FALSE)

# Navigation tests
Err(fit.sub)
features_stat(fit.sub)
parameters_stat(fit.sub)
best_model(fit.sub)
