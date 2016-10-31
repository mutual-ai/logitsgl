#
#     Description of this R script:
#     R interface for linear multi-response sparse group lasso routines.
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

#' @title Predict
#'
#' @description
#' Compute the predicted response matrix for a new data set.
#'
#' @param object an object of class logitsgl, produced with \code{logitsgl}.
#' @param x a data matrix of size \eqn{N_\textrm{new} \times p}.
#' @param sparse.data if TRUE \code{x} will be treated as sparse, if \code{x} is a sparse matrix it will be treated as sparse by default.
#' @param ... ignored.
#' @return
#' \item{Yhat}{the predicted response. A list, of length \code{nmod(object)}, of matrices (of size \eqn{N_\textrm{new} \times K})}
#' \item{link}{the linear predictors. A list, of length \code{nmod(object)}, of matrices  (of size \eqn{N_\textrm{new} \times K})}
#' \item{prob}{the predicted probabilities. A list, of length \code{nmod(object)}, of matrices (of size \eqn{N_\textrm{new} \times K})}
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#'
#' fit <- logitsgl(X, Y, alpha = 0.5, lambda = lambda)
#'
#' # training errors
#' res <- predict(fit, X)
#'
#' # print some info
#' res
#'
#' # The predicted probabilities for model with index 100
#' res$prob[[100]]
#' @method predict logitsgl
#' @export
#' @useDynLib logitsgl, .registration=TRUE
predict.logitsgl <- function(object, x, sparse.data = is(x, "sparseMatrix"), ...)
{
	# Get call
	cl <- match.call()

	if(is.null(object$beta)) stop("No models found -- missing beta")

	if(object$intercept){
		# add intercept
		x <- cBind(Intercept = rep(1, nrow(x)), x)
	}

	#Check dimension of x
	if(dim(object$beta[[2]])[2] != ncol(x)) stop("x has wrong dimension")

	data <- create.sgldata(x, NULL, sparseX = sparse.data, sparseY = FALSE)

	res <- sgl_predict(
		module_name = .get_callsym(data),
		PACKAGE = "logitsgl",
		object = object,
		data = data,
		responses = c("link", "prob"))

	#Responses
	res$P <- res$responses$prob
	res$link <- res$responses$link
	res$responses <- NULL

	#TODO response dimnames

	res$call <- cl

	class(res) <- "logitsgl"
	return(res)
}
