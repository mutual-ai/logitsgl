#
#     Description of this R script:
#     R interface for linear multi-response models using sparse group lasso.
#
#     Intended for use with R.
#     Copyright (C) 2014 Martin Vincent
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>
#

#TODO add auc error measure

#' @title Compute error rates
#'
#' @description
#' Compute error rates for each model.
#' If \code{type = "rate"} then the misclassification rates will be computed.
#' If \code{type = "count"} then the misclassification counts will be computed.
#' If \code{type = "loglike"} then the negative log-likelihood error will be computed.
#'
#' @param object a logitsgl object
#' @param data a design matrix (the \eqn{X} matrix)
#' @param response redirected to \code{y}
#' @param y a matrix of the true responses (the \eqn{Y} matrix)
#' @param loss type of error loss
#' @param ... additional parameters passed to the loss function
#' @return a vector of error rates
#'
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' # Split and scale down
#' X1 <- X[1:200, ]
#' Y1 <- Y[1:200, 1:3]
#' X2 <- X[201:322,]
#' Y2 <- Y[201:322, 1:3]
#'
#' # Fit models using X1
#' lambda <- logitsgl.lambda(X1, Y1, alpha = 0.5, lambda.min = 0.2)
#' fit <- logitsgl(X1, Y1, alpha = 0.5, lambda = lambda)
#'
#' ## Training errors:
#' Err(fit, X1)
#'
#' ## Errors predicting Y2:
#' Err(fit, X2, Y2)
#'
#' # Do cross validation
#' fit.cv <- logitsgl.cv(X1, Y1, alpha = 0.5, fold = 5, lambda = lambda)
#'
#' # Cross validation errors -- error rate
#' Err(fit.cv)
#'
#' @method Err logitsgl
#' @import sglOptim
#' @export
Err.logitsgl <- function(object,
	data = NULL,
	response = object$Y.true,
	y = response,
	loss = "loglike", ... ) {

	if(loss == "loglike") {

		loss <- function(x,y) -mean(y*log(x)+(1-y)*log(1-x))

		return( compute_error(object,
			data = data,
			response.name = "P",
			response = y,
			loss = loss))
	}

	stop("Unknown loss")

}


#' @title Nonzero features
#'
#' @description
#' Extracts the nonzero features for each model.
#'
#' @param object a logitsgl object
#' @param ... ignored
#' @return a list of of length \code{nmod(x)} containing the nonzero features (that is nonzero columns of the beta matrices)
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#' fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)
#'
#' # the nonzero features of model 1, 10 and 25
#' features(fit)[c(1,10,25)]
#'
#' # count the number of nonzero features in each model
#' sapply(features(fit), length)
#' @method features logitsgl
#' @import sglOptim
#' @export
features.logitsgl <- function(object, ...) {
	class(object) <- "sgl" # Use std function
	return(features(object))
}

#' @title Nonzero parameters
#'
#' @description
#' Extracts the nonzero parameters for each model.
#'
#' @param object a logitsgl object
#' @param ... ignored
#' @return a list of length \code{nmod(x)} containing the nonzero parameters of the models.
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.6)
#' fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)
#'
#' # Nonzero parameters for model 100
#' parameters(fit)[[100]]
#' @method parameters logitsgl
#' @import sglOptim
#' @export
parameters.logitsgl <- function(object, ...) {
	class(object) <- "sgl" # Use std function
	return(parameters(object))
}

#' @title Extract feature statistics
#'
#' @description
#' Extracts the number of nonzero features (or group) in each model.
#'
#' @param object a logitsgl object
#' @param ... ignored
#' @return a vector of length \code{nmod(x)} or a matrix containing the number of nonzero features (or group) of the models.
#'
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.8)
#' fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)
#'
#' # Number of features in models
#' features_stat(fit)
#'
#' fit.cv <- logitsgl.cv(X, Y, alpha = 0.5, fold = 2, lambda = lambda)
#'
#' # Number of features
#' features_stat(fit.cv)
#' @export
features_stat.logitsgl <- function(object, ...) {
	class(object) <- "sgl" # Use std function
	return(features_stat(object, ...))
}


#' @title Extracting parameter statistics
#'
#' @description
#' Extracts the number of nonzero parameters in each model.
#'
#' @param object a logitsgl object
#' @param ... ignored
#' @return a vector of length \code{nmod(x)} or a matrix containing the number of nonzero parameters of the models.
#'
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#' fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)
#'
#' # Number of parameters in models
#' parameters_stat(fit)
#'
#' fit.cv <- logitsgl.cv(X, Y, alpha = 0.5, fold = 2, lambda = lambda)
#'
#' # Number of parameters
#' parameters_stat(fit.cv)
#' @export
parameters_stat.logitsgl <- function(object, ...) {
	class(object) <- "sgl" # Use std function
	return(parameters_stat(object, ...))
}

#' @title Number of models used for fitting
#'
#' @description
#' Returns the number of models used for fitting.
#' However, note that the objects returned by \code{msgl.cv} and
#' \code{msgl.subsampling} does not contain any models even though
#' \code{nmod} returns a nonzero number.
#'
#' @param object an object
#' @param ... additional parameters (optional)
#' @return the number of models used when fitting the object \code{x}.
#'
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#' fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)
#'
#' nmod(fit)
#' @import sglOptim
#' @export
nmod.logitsgl <- function(object, ...) {
	class(object) <- "sgl" # Use std function
	return(nmod(object, ...))
}

#' @title Index of best model
#'
#' @description
#' Returns the index of the best model, in terms of lowest error rate
#' @param object a logitsgl object
#' @param ... additional parameters (ignored)
#' @return index of the best model.
#'
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#' fit.cv <- logitsgl.cv(X, Y, alpha = 0.5, fold = 5, lambda = lambda)
#'
#' # print some info
#' fit.cv
#'
#' best_model(fit.cv)
#' @export
best_model.logitsgl <- function(object, ...) {
	class(object) <- "sgl" # Use std function
	return(best_model(object, "logitsgl", ...))
}

#' @title Exstract the fitted models
#'
#' @description
#' Returns the fitted models, that is the estimated \eqn{\beta} matrices.
#'
#' @param object a logitsgl object
#' @param index indices of the models to be returned
#' @param ... ignored
#' @return a list of \eqn{\beta} matrices.
#'
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#' fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)
#'
#' # Exstract model 50
#' models(fit)[[50]]
#' @method models logitsgl
#' @import sglOptim
#' @export
models.logitsgl <- function(object, index = 1:nmod(object), ...) {
	class(object) <- "sgl" # Use std function
	return(models(object, ...))
}

#' @title Extracting the nonzero coefficients
#' @description
#' This function returns the nonzero coefficients
#' (that is the nonzero entries of the \eqn{beta} matrices)
#'
#' @param object a sgl object
#' @param index indices of the models
#' @param ... ignored
#' @return a list of with nonzero coefficients of the models
#'
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#' fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)
#'
#' # nonzero coefficients in model 50
#' coef(fit, 50)
#' @import sglOptim
#' @export
coef.logitsgl <- function(object, index = 1:nmod(object), ...) {
	class(object) <- "sgl" # Use std function
	return(coef(object, index = index, ...))
}


#' Print function for logitsgl
#'
#' This function will print some general information about the lsgl object
#'
#' @param x logitsgl object
#' @param ... ignored
#'
#' @method print logitsgl
#' @author Martin Vincent
#' @import sglOptim
#' @export
print.logitsgl <- function(x, ...) {
	sgl_print(x)
}
