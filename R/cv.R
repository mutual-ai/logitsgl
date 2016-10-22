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

#' @title Cross Validation
#' @description
#' Multiple logistic regression sparse group lasso cross validation using parallel computation.
#'
#' @param x design matrix, matrix of size \eqn{N \times p}.
#' @param y response matrix, matrix of size \eqn{N \times K}.
#' @param intercept should the model include intercept parameters
#' @param grouping grouping of features, a factor or vector of length \eqn{p}. Each element of the factor/vector specifying the group of the feature.
#' @param groupWeights the group weights, a vector of length \eqn{m} (the number of groups).
#' @param parameterWeights a matrix of size \eqn{K \times p}.
#' @param alpha the \eqn{\alpha} value 0 for group lasso, 1 for lasso, between 0 and 1 gives a sparse group lasso penalty.
#' @param lambda the lambda sequence for the regularization path.
#' @param fold the fold of the cross validation, an integer larger than 1 and less than \eqn{N+1}. Ignored if \code{cv.indices != NULL}.
#' @param cv.indices a list of indices of a cross validation splitting.
#' If \code{cv.indices = NULL} then a random splitting will be generated using the \code{fold} argument.
#' @param use_parallel If \code{TRUE} the \code{foreach} loop will use \code{\%dopar\%}. The user must registre the parallel backend.
#' @param algorithm.config the algorithm configuration to be used.
#' @return
#' \item{Yhat}{the cross validation estimated response matrix}
#' \item{Y.true}{the true response matrix, this is equal to the argument \code{y}}
#' \item{cv.indices}{the cross validation splitting used}
#' \item{features}{number of features used in the models}
#' \item{parameters}{number of parameters used in the models.}
#' @author Martin Vincent
#' @examples
#' set.seed(100)
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' lambda <- logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#'
#' # registre parallel backend
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'
#' # Do cross validation using 2 threads
#' fit.cv <- logitsgl.cv(X, Y, alpha = 0.5, fold = 2, lambda = lambda, use_parallel = TRUE)
#'
#' stopCluster(cl)
#'
#' # print some information
#' fit.cv
#' @useDynLib logitsgl, .registration=TRUE
#' @export
logitsgl.cv <- function(x, y,
		intercept = TRUE,
		grouping = factor(1:ncol(x)),
		groupWeights = NULL,
		parameterWeights =  NULL,
		alpha = 1,
		lambda,
		fold = 10L,
		cv.indices = list(),
		use_parallel = FALSE,
		algorithm.config = logitsgl.standard.config)
{

	# Get call
	cl <- match.call()

	setup <- .process_args(x, y,
		intercept = intercept,
		grouping = grouping,
		groupWeights = groupWeights,
		parameterWeights = parameterWeights)

	data <- setup$data

	#TODO print some info

	res <- sgl_cv(
		module_name = setup$callsym,
		PACKAGE = "logitsgl",
		data = data,
		parameterGrouping = setup$grouping,
		groupWeights = setup$groupWeights,
		parameterWeights = setup$parameterWeights,
		alpha =  alpha,
		lambda = lambda,
		fold = fold,
		cv.indices = cv.indices,
		responses = c("link", "prob"),
		use_parallel = use_parallel,
		algorithm.config = algorithm.config
		)

	# Add true response
	res$Y.true <- y

	# Responses
	res$P <- lapply(res$responses$prob, t)
	res$link <- lapply(res$responses$link, t)
	res$responses <- NULL

	#TODO response dimnames

	res$logitsgl_version <- packageVersion("logitsgl")
	res$intercept <- intercept
	res$call <- cl

	class(res) <- "logitsgl"
	return(res)
}
