#' Summary of \code{\link{MBPLS}} objects
#'
#' Edits the Cumulative Explained Variance, Block Explained Variance per Dimension and Block Saliences per Dimension of a \code{\link{MBPLS}} object.
#'
#' @param object An object resulting from \code{\link{MBPLS}}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The summary.
#'
#' @seealso \code{\link{plot.MBPLS}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, block, name.block = names(block))
#' summary(res.mbpls)
#'
#' @export

summary.MBPLS=function(object,...){
  if (!inherits(object, c("MBPLS"))){
    stop("class(object) must be MBPLS")
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
