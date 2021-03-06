% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/navigate.R
\name{coef.logitsgl}
\alias{coef.logitsgl}
\title{Extracting the nonzero coefficients}
\usage{
\method{coef}{logitsgl}(object, index = 1:nmod(object), ...)
}
\arguments{
\item{object}{a sgl object}

\item{index}{indices of the models}

\item{...}{ignored}
}
\value{
a list of with nonzero coefficients of the models
}
\description{
This function returns the nonzero coefficients
(that is the nonzero entries of the \eqn{beta} matrices)
}
\examples{
data(birds)

# Simple standardization
X <- scale(X)

lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)

# nonzero coefficients in model 50
coef(fit, 50)
}
\author{
Martin Vincent
}

