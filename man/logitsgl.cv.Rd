% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv.R
\name{logitsgl.cv}
\alias{logitsgl.cv}
\title{Cross Validation}
\usage{
logitsgl.cv(x, y, intercept = TRUE, grouping = factor(1:ncol(x)),
  groupWeights = NULL, parameterWeights = NULL, alpha = 1, lambda,
  fold = 10L, cv.indices = list(), use_parallel = FALSE,
  algorithm.config = logitsgl.standard.config)
}
\arguments{
\item{x}{design matrix, matrix of size \eqn{N \times p}.}

\item{y}{response matrix, matrix of size \eqn{N \times K}.}

\item{intercept}{should the model include intercept parameters}

\item{grouping}{grouping of features, a factor or vector of length \eqn{p}. Each element of the factor/vector specifying the group of the feature.}

\item{groupWeights}{the group weights, a vector of length \eqn{m} (the number of groups).}

\item{parameterWeights}{a matrix of size \eqn{K \times p}.}

\item{alpha}{the \eqn{\alpha} value 0 for group lasso, 1 for lasso, between 0 and 1 gives a sparse group lasso penalty.}

\item{lambda}{the lambda sequence for the regularization path.}

\item{fold}{the fold of the cross validation, an integer larger than 1 and less than \eqn{N+1}. Ignored if \code{cv.indices != NULL}.}

\item{cv.indices}{a list of indices of a cross validation splitting.
If \code{cv.indices = NULL} then a random splitting will be generated using the \code{fold} argument.}

\item{use_parallel}{If \code{TRUE} the \code{foreach} loop will use \code{\%dopar\%}. The user must registre the parallel backend.}

\item{algorithm.config}{the algorithm configuration to be used.}
}
\value{
\item{Yhat}{the cross validation estimated response matrix}
\item{Y.true}{the true response matrix, this is equal to the argument \code{y}}
\item{cv.indices}{the cross validation splitting used}
\item{features}{number of features used in the models}
\item{parameters}{number of parameters used in the models.}
}
\description{
Multiple logistic regression sparse group lasso cross validation using parallel computation.
}
\examples{
set.seed(100)
data(birds)

# Simple standardization
X <- scale(X)

lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.9)

# registre parallel backend
cl <- makeCluster(2)
registerDoParallel(cl)

# Do cross validation using 2 threads
fit.cv <- logitsgl.cv(X, Y, alpha = 0.5, fold = 10, lambda = lambda, use_parallel = TRUE)

stopCluster(cl)

# print some information
fit.cv
}
\author{
Martin Vincent
}

