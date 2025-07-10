#' Summary of \code{\link{ComDim}} objects
#'
#' Edits the Cumulative Explained Variance, Block Explained Variance per Dimension and Block Saliences per Dimension of a \code{\link{ComDim}} object.
#'
#' @param object An object resulting from \code{\link{ComDim}}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The summary.
#'
#' @seealso \code{\link{plot.ComDim}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' block=ham$block
#' res.comdim <- ComDim(X,block,name.block=names(block))
#' summary(res.comdim)
#'
#' @export

summary.ComDim=function(object,...){
 
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
