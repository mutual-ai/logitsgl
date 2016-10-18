#' @title Subsampling
#' @description
#' Subsampling using multiple possessors
#'
#' @param x design matrix, matrix of size \eqn{N \times p}.
#' @param y response matrix, matrix of size \eqn{N \times K}.
#' @param intercept should the model include intercept parameters
#' @param grouping grouping of features, a factor or vector of length \eqn{p}.
#' Each element of the factor/vector specifying the group of the feature.
#' @param groupWeights the group weights, a vector of length \eqn{m} (the number of groups).
#' @param parameterWeights a matrix of size \eqn{K \times p}.
#' @param alpha the \eqn{\alpha} value 0 for group lasso, 1 for lasso,
#' between 0 and 1 gives a sparse group lasso penalty.
#' @param lambda the lambda sequence for the regularization path.
#' @param train a list of training samples,
#' each item of the list corresponding to a subsample.
#' Each item in the list must be a vector with the indices of the training samples
#' for the corresponding subsample.
#' The length of the list must equal the length of the \code{test} list.
#' @param test a list of test samples, each item of the list corresponding to a subsample.
#' Each item in the list must be vector with the indices of the test samples for the corresponding subsample.
#' The length of the list must equal the length of the \code{training} list.
#' @param collapse if \code{TRUE} the results for each subsample will be collapse into one result (this is useful if the subsamples are not overlapping)
#' @param use_parallel If \code{TRUE} the \code{foreach} loop will use \code{\%dopar\%}. The user must registre the parallel backend.
#' @param algorithm.config the algorithm configuration to be used.
#' @return
#' \item{Yhat}{TODO}
#' \item{Y.true}{TODO}
#' \item{features}{number of features used in the models}
#' \item{parameters}{number of parameters used in the models.}
#' @examples
#' data(birds)
#'
#' # Simple standardization
#' X <- scale(X)
#'
#' train <- list(1:250, 1:300)
#' test <- list(301:322, 301:322)
#'
#' lambda <- lapply(train, function(idx)
#'		logitsgl.lambda(X[idx,], Y[idx,], alpha = 0.5, lambda.min = 0.4))
#'
#' # registre parallel backend
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'
#' # Do subsampling using 2 threads
#' fit.sub <- logitsgl.subsampling(X, Y,
#'		alpha = 0.5,
#'		train = train,
#'		test = test,
#'		lambda = lambda,
#'		use_parallel = TRUE)
#'
#' stopCluster(cl)
#'
#' # print some information
#' fit.sub
#' @author Martin Vincent
#' @useDynLib logitsgl, .registration=TRUE
#' @export
#' @importFrom utils packageVersion
logitsgl.subsampling <- function(x, y,
		intercept = TRUE,
		grouping = factor(1:ncol(x)),
		groupWeights = NULL,
		parameterWeights =  NULL,
		alpha = 1,
		lambda,
		train,
		test,
		collapse = FALSE,
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

	res <- sgl_subsampling(
		module_name = setup$callsym,
		PACKAGE = "logitsgl",
		data = data,
		parameterGrouping = setup$grouping,
		groupWeights = setup$groupWeights,
		parameterWeights = setup$parameterWeights,
		alpha =  alpha,
		lambda = lambda,
		training = train,
		test = test,
		collapse = collapse,
		use_parallel = use_parallel,
		)

	# Add true response
	res$Y.true <- lapply(test, function(i) y[i, , drop = FALSE])

	# Format response
	if(collapse) {

		res$P <- lapply(res$responses$prob, t)
		res$link <- lapply(res$responses$link, t)
		res$Yhat <- lapply(res$responses$classes, t)

	} else {

		res$P <- lapply(res$responses$prob, function(x) lapply(x, t))
		res$link <- lapply(res$responses$link, function(x) lapply(x, t))
		res$Yhat <- lapply(res$responses$classes, function(x) lapply(x, t))

	}

	res$responses <- NULL

	res$logitsgl_version <- packageVersion("logitsgl")
	res$intercept <- intercept
	res$call <- cl

	class(res) <- "logitsgl"

	return(res)
}
