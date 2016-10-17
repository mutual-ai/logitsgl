#' @title Test objectives
#'
#' @description
#' TODO description
#'
#' @param x design matrix, matrix of size \eqn{N \times p}.
#' @param y response matrix, matrix of size \eqn{N \times K}.
#' @param grouping grouping of features, a factor or vector of length \eqn{p}. Each element of the factor/vector specifying the group of the feature.
#' @param groupWeights the group weights, a vector of length \eqn{m} (the number of groups).
#' @param parameterWeights a matrix of size \eqn{K \times p}.
#' @return number of detected problems
#' @author Martin Vincent
#' @examples
#' data(birds)
#'
#' # scale down
#' X <- X[1:100, 1:10]
#' Y <- Y[1:100, 1:3]
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' # look for problems in objective -- this may be slow for high dimensions
#' n_problems_detected <- test_objective_logitsgl(X, Y)
#'
#' if(n_problems_detected > 0) stop("problem detected in objective")
#' @useDynLib logitsgl, .registration=TRUE
#' @export
test_objective_logitsgl <- function(x, y,
		grouping = factor(1:ncol(x)),
		groupWeights = c(sqrt(ncol(y)*table(grouping))),
		parameterWeights =  matrix(1, nrow = ncol(y), ncol = ncol(x)))
{

	# cast
	grouping <- factor(grouping)

	# create data
	group.names <- if(is.null(colnames(y))) 1:ncol(y) else colnames(y)
	data <- create.sgldata(x, y, group.names = group.names)

	# get module name
	callsym <- paste("logitsgl_", if(data$sparseX) "xs_" else "xd_", if(data$sparseY) "ys" else "yd", sep = "")

	res <- sgl_test(
		module_name = callsym,
		PACKAGE = "logitsgl",
		data = data,
		parameterGrouping = grouping,
		groupWeights = groupWeights,
		parameterWeights = parameterWeights,
		)

	return(res)
}
