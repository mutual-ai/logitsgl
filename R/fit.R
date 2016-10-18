#
#     Description of this R script:
#     R interface for multi-label sparse group lasso logistic regression routines.
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

#' @title Multi-label Sparse Group Lasso Logistic Regression
#'
#' @description
#' Fit a multi-label sparse group lasso logistic regression using sparse group lasso.
#'
#' @param x design matrix, matrix of size \eqn{N \times p}.
#' @param y response matrix, matrix of size \eqn{N \times K}.
#' @param intercept should the model include intercept parameters
#' @param grouping grouping of features, a factor or vector of length \eqn{p}.
#' Each element of the factor/vector specifying the group of the feature.
#' @param groupWeights the group weights, a vector of length \eqn{m} (the number of groups).
#' @param parameterWeights a matrix of size \eqn{K \times p}.
#' @param alpha the \eqn{\alpha} value 0 for group lasso, 1 for lasso, between 0 and 1 gives a sparse group lasso penalty.
#' @param lambda lambda sequence.
#' @param algorithm.config the algorithm configuration to be used.
#'
#' @return
#' \item{beta}{the fitted parameters -- the list \eqn{\hat\beta(\lambda(1)), \dots, \hat\beta(\lambda(d))} of length \code{length(return)}.
#' With each entry of list holding the fitted parameters, that is matrices of size \eqn{K\times p} (if \code{intercept = TRUE} matrices of size \eqn{K\times (p+1)})}
#' \item{loss}{the values of the loss function.}
#' \item{objective}{the values of the objective function (i.e. loss + penalty).}
#' \item{lambda}{the lambda values used.}
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
#' # print some information
#' fit
#'
#' # Traning error
#' Err(fit, X)
#' @useDynLib logitsgl, .registration=TRUE
#' @export
#' @import Matrix
#' @import sglOptim
logitsgl <- function(x, y,
		intercept = TRUE,
		grouping = factor(1:ncol(x)),
		groupWeights = NULL,
		parameterWeights =  NULL,
		alpha = 1,
		lambda,
		algorithm.config = logitsgl.standard.config) {

	# Get call
	cl <- match.call()

	setup <- .process_args(x, y,
		intercept = intercept,
		grouping = grouping,
		groupWeights = groupWeights,
		parameterWeights = parameterWeights)

	data <- setup$data

	# Print some info
	cat("\nRunning logitsgl ")
	if(data$sparseX & data$sparseY) {
		cat("(sparse design and response matrices)\n\n")
	}
	if(data$sparseX & ! data$sparseY) {
		cat("(sparse design matrix)\n\n")
	}
	if( ! data$sparseX & data$sparseY) {
		cat("(sparse response matrix)\n\n")
	}
	if( ! data$sparseX & ! data$sparseY) {
		cat("\n\n")
	}

	print(data.frame('Samples: ' = print_with_metric_prefix(data$n.samples),
					'Features: ' = print_with_metric_prefix(data$n.covariate),
					'Models: ' = print_with_metric_prefix(data$n.models),
					'Groups: ' = print_with_metric_prefix(length(unique(setup$grouping))),
					'Parameters: ' = print_with_metric_prefix(length(setup$parameterWeights)),
					check.names = FALSE),
			row.names = FALSE, digits = 2, right = TRUE)
	cat("\n")

	# Call sglOptim
	res <- sgl_fit(
		module_name = setup$callsym,
		PACKAGE = "logitsgl",
		data = data,
		parameterGrouping = setup$grouping,
		groupWeights = setup$groupWeights,
		parameterWeights = setup$parameterWeights,
		alpha = alpha,
		lambda = lambda,
		algorithm.config = algorithm.config)

	# Add true response
	res$Y.true <- y

	res$intercept <- intercept
	res$logitsgl_version <- packageVersion("logitsgl")
	res$call <- cl

	class(res) <- "logitsgl"
	return(res)
}
