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
#' @return TODO
#' @author Martin Vincent
#' @useDynLib logitsgl, .registration=TRUE
#' @export
logitsgl.test <- function(x, y,
		grouping = factor(1:ncol(x)),
		groupWeights = c(sqrt(2*ncol(y)*table(grouping))),
		parameterWeights =  matrix(1, nrow = (ncol(y)^2-ncol(y))/2+2*ncol(y), ncol = ncol(x)))
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
