
#' @title Compute a lambda sequence for the regularization path
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
logitsgl.lambda <- function(x, y, intercept = TRUE,
		grouping = factor(1:ncol(x)),
		groupWeights = c(sqrt(ncol(y)*table(grouping))),
		parameterWeights =  matrix(1, nrow = ncol(y), ncol = ncol(x)),
		alpha = 1,
		d = 100L,
		lambda.min,
		algorithm.config = logitsgl.standard.config)
{

	if( nrow(x) != if(is.vector(y)) length(y) else nrow(y) ) {
		stop("x and y must have the same number of rows")
	}

	# cast
	grouping <- factor(grouping)

	# add intercept
	if(intercept) {
		x <- cBind(Intercept = rep(1, nrow(x)), x)
		groupWeights <- c(0, groupWeights)
		parameterWeights <- cbind(rep(0, if(is.vector(y)) 1 else ncol(y)), parameterWeights)
		grouping <- factor(c("Intercept", as.character(grouping)), levels = c("Intercept", levels(grouping)))
	}

	# create data
	data <- create.sgldata(x, y)

  # TODO ensure ColSums(Y != 0) != 0


	# call SglOptimizer function
	callsym <- paste("logitsgl_", if(data$sparseX) "xs_" else "xd_", if(data$sparseY) "ys" else "yd", sep = "")

	lambda <- sgl_lambda_sequence(
		module_name = callsym,
		PACKAGE = "logitsgl",
		data = data,
		parameterGrouping = grouping,
		groupWeights = groupWeights,
		parameterWeights = parameterWeights,
		alpha = alpha,
 		d = d,
		lambda.min = lambda.min,
		algorithm.config = algorithm.config,
		lambda.min.rel = TRUE)

	return(lambda)
}
