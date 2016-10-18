# Check and setup sgl call arguments
.process_args <- function(x, y,
  intercept,
  grouping,
  groupWeights,
  parameterWeights) {

# TODO ensure ColSums(Y != 0) != 0

if( nrow(x) != if(is.vector(y)) length(y) else nrow(y) ) {
  stop("x and y must have the same number of rows")
}

# cast
grouping <- factor(grouping)

# Cat y as matrix
if( is.vector(y) ) {
  y <- matrix(y, nrow = length(y), ncol = 1)
}

# Initialize groupWeights
if( is.null(groupWeights) ) {
   groupWeights <- c(sqrt(ncol(y)*table(grouping)))
}

# Initialize parameterWeights
if( is.null(parameterWeights) ) {
  parameterWeights <-  matrix(1, nrow = ncol(y), ncol = ncol(x))
}

# add intercept
if(intercept) {
  x <- cBind(Intercept = rep(1, nrow(x)), x)
  groupWeights <- c(0, groupWeights)
  parameterWeights <- cbind(rep(0, ncol(y)), parameterWeights)
  grouping <- factor(c("Intercept", as.character(grouping)), levels = c("Intercept", levels(grouping)))
}

# create data
group.names <- if(is.vector(y)) "response" else if(is.null(colnames(y))) 1:ncol(y) else colnames(y)
data <- create.sgldata(x, y, group.names = group.names)

# Call sglOptim function
callsym <- paste("logitsgl_", if(data$sparseX) "xs_" else "xd_", if(data$sparseY) "ys" else "yd", sep = "")

setup <- list()
setup$data <- data
setup$callsym <- callsym
setup$grouping <- grouping
setup$groupWeights <- groupWeights
setup$parameterWeights <- parameterWeights

return(setup)

}
