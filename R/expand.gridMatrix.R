
#' utility similar to expand.grid, but returns a matrix not data.frame
#' 
#' @description This function is similar to \code{\link{expand.grid}}, in the sense that it returns a 
#'   matrix that has 2 columns, one for each input, and one row per combination, cycling through the first field first.
#'   It differs from expand.grid in that this returns a matrix not data.frame, only accepts two parameters creating two columns, for now,
#'   and lacks the other parameters of expand.grid
#' @param x required vector
#' @param y required vector
#' @return This function returns a matrix and tries to assign colnames based on the two input parameters. If they are variables, it uses those names as colnames. Otherwise it uses "x" and "y" as colnames.
#' @seealso \code{\link{expand.grid}}
#' @examples
#'  expand.gridMatrix(99:103, 1:2) 
#'  zz <- 1:10; top <- 1:2
#'  expand.gridMatrix(zz, top) 
#'  
#' @keywords internal
#' @export
#' 
expand.gridMatrix <- function(x, y) {
  z <- cbind(rep(x, length(y)), rep(y, each = length(x)))
  a <- as.character(substitute(x))
  b <- as.character(substitute(y))
  if (length(a) != 1) {a <- 'x'}
  if (length(b) != 1) {b <- 'y'}
  colnames(z) <- c(a, b)
  return(z)
}
