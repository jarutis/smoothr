##' Local Polynomial Kernel Smoother
##'
##' @param formula y ~ x, where y is teh response variable
##' @param data a dataframe with y and x columns
##' @param grouped_by optional grouping column name
##' @param bandwidth optional smoothing parameter, determined by GCV rule
##' if it is not set
##' @param significance significance value for calculating significance bounds
##' @param degree polynomial degree used in smoothing
##' @param kernel kernel function used in smoothing
##' @param robust whether to adjust for outliers
##' @return a functions f(x0) which takes x0 as argument and outputs y estimate at x0
lpfit <- function(formula, data, grouped_by = NULL, bandwidth = NULL,
                  significance = 0.05, degree = 1, kernel = gaussianK,
                  robust = FALSE) {
  frame <- model.frame(formula, data)
  ## extract variables
  x <- frame[,2]
  y <- frame[,1]
  n <- length(x)
  z <- qnorm(1 - (significance / 2))
  ## function to calculate smoother matrix
  smoother <- function(x0, bandwidth) {
    diff <- (x - x0)
    scaled_diff <- diff / bandwidth
    X <- vapply(0:degree, function(degree) diff ^ degree, numeric(n))
    W <- diag(kernel(scaled_diff))
    (MASS::ginv(t(X) %*% W %*% X) %*% (t(X) %*% W))[1,]
  }
  ## select bandwidth
  gcv <- function(bandwidth) {
    smoother_matrix <- t(vapply(x, smoother, numeric(n), bandwidth = bandwidth))
    estimate <- as.vector(smoother_matrix %*% y)
    mean((y - estimate) ^ 2)/(1 - trace(smoother_matrix) / n) ^ 2
  }
  if(is.null(bandwidth)) {
    xrange <- diff(range(x))
    start <- 1.1 * xrange / n
    end <- xrange / 8
    bandwidth <- .find_smoothing_parameter(gcv, start, end)
  }
  ## result
  smoother_matrix <- t(vapply(x, smoother, numeric(n), bandwidth = bandwidth))
  estimate <- as.vector(smoother_matrix %*% y)
  fit <- list(data = data,
              formula = formula,
              estimate = estimate,
              bandwidth = bandwidth,
              fn = function(x0) (smoother(x0, bandwidth) %*% y)[[1]])
  fit$error <- .error_rate(x, y, estimate, smoother_matrix)
  fit$significance <- cbind(lower = estimate - z * fit$error,
                            upper = estimate + z * fit$error)
  class(fit) <- "smooth_function"
  fit
}

.error_rate <- function(x, y, estimate, smoother_matrix) {
  n <- length(x)
  sig <- sum((y - estimate) ^ 2) / (n - trace(smoother_matrix))
  sqrt(sig * diag(smoother_matrix %*% t(smoother_matrix)))
}

.find_smoothing_parameter <- function(gcv_fn, start, end, steps = 15) {
  candidates <- logspace(start, end, steps)
  gcv <- vapply(candidates, gcv_fn, numeric(1))
  param <- candidates[which.min(gcv)]
  attr(param, "candidates") <- list(param = candidates, gcv = gcv)
  param
}
