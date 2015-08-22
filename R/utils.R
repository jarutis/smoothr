##' Calculate trace of a rectangular matrix
##'
##' @param A a matrix
##' @return The sum of its elements alog the diagonal
##' @examples
##' trace(matrix(c(1,2,3,4), 2))
trace <- function(A) sum(diag(A))

##' Create logarithmicaly spaced points
##'
##' @param start a starting point
##' @param end an ending pont
##' @param n number of steps
##' @return Vector of logarithmicaly spaced points
logspace <- function(start, end, n) {
  10 ^ seq(log10(start), log10(end), length.out=n)
}
