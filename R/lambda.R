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

#' @title Compute Lambda Sequence
#'
#' @description
#' Computes a decreasing lambda sequence of length \code{d}.
#' The sequence ranges from a data determined maximal lambda \eqn{\lambda_\textrm{max}} to the user inputed \code{lambda.min}.
#'
#' @param x design matrix, matrix of size \eqn{N \times p}.
#' @param y response matrix, matrix of size \eqn{N \times K}.
#' @param intercept should the model include intercept parameters
#' @param grouping grouping of features, a factor or vector of length \eqn{p}. Each element of the factor/vector specifying the group of the feature.
#' @param groupWeights the group weights, a vector of length \eqn{m} (the number of groups).
#' @param parameterWeights a matrix of size \eqn{K \times p}.
#' @param alpha the \eqn{\alpha} value 0 for group lasso, 1 for lasso, between 0 and 1 gives a sparse group lasso penalty.
#' @param d the length of lambda sequence
#' @param lambda.min the smallest lambda value in the computed sequence.
#' @param algorithm.config the algorithm configuration to be used.
#' @return a vector of length \code{d} containing the compute lambda sequence.
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' logitsgl.lambda(X, Y, alpha = 0.5, lambda.min = 0.5)
#' @useDynLib logitsgl, .registration=TRUE
#' @export
logitsgl.lambda <- function(x, y,
		intercept = TRUE,
		grouping = factor(1:ncol(x)),
		groupWeights = NULL,
		parameterWeights =  NULL,
		alpha = 1,
		d = 100L,
		lambda.min,
		algorithm.config = logitsgl.standard.config)
{

	setup <- .process_args(x, y,
		intercept = intercept,
		grouping = grouping,
		groupWeights = groupWeights,
		parameterWeights = parameterWeights)

	data <- setup$data

	#TODO print some info

	lambda <- sgl_lambda_sequence(
		module_name = setup$callsym,
		PACKAGE = "logitsgl",
		data = data,
		parameterGrouping = setup$grouping,
		groupWeights = setup$groupWeights,
		parameterWeights = setup$parameterWeights,
		alpha = alpha,
 		d = d,
		lambda.min = lambda.min,
		algorithm.config = algorithm.config,
		lambda.min.rel = TRUE)

	return(lambda)
}
