#' Summary of \code{\link{MBWCov}} objects
#'
#' Edits the Cumulative Explained Variance, Block Explained Variance per Dimension and Block Saliences per Dimension of a \code{\link{MBWCov}} object.
#'
#' @param object An object resulting from \code{\link{MBWCov}}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The summary.
#'
#' @seealso \code{\link{plot.MBWCov}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbwcov <- MBWCov(X, Y, block, name.block = names(block))
#' summary(res.mbwcov)
#'
#' @export

summary.MBWCov=function(object,...){
  if (!inherits(object, c("MBWCov"))){
    stop("class(object) must be MBWCov")
  }
  cat("################## Cumulative Explained Variance ###########################")
  cat("\n")
  cat("\n")
  print(object$cumexplained)
  cat("\n")
  cat("################## Block Explained Variance per Dimension ##################")
  cat("\n")
  cat("\n")
  print(object$explained.X)
  cat("\n")
  cat("################## Block Saliences per Dimension ###########################")
  cat("\n")
  cat("\n")
  print(object$saliences)
}
