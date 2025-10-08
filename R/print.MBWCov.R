#' Print of \code{\link{MBWCov}} Objects
#'
#' @param x An object resulting from \code{\link{MBWCov}}.
#' @param ... further arguments passed to or from other methods.
#'
#' @seealso \code{\link{summary.MBWCov}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbwcov <- MBWCov(X, Y, block, name.block = names(block))
#' print(res.mbwcov)
#'
#' @export
#'
print.MBWCov =  function (x, ...)
{

  n <- nrow(x$call$X)
  p <- ncol(x$call$X)
  pY <- ncol(x$call$Y)
  nam <- x$call$name.block
  K <- length(nam)
  pk <- x$call$size.block


  var.scal <- x$call$scale
  block.scal <- x$call$scale.block

  ncomp <- x$call$ncomp

  cat("\n")
  cat(paste("Number of observations: ", n), sep = " ")
  cat("\n")
  cat(paste("Total number of X-variables: ", p), sep = " ")
  cat("\n")
  cat(paste("Number of X-blocks: ", K), sep = " ")
  cat("\n")
  cat("Block's Name (Nb variables): ")
  cat(paste(paste(nam,"(",pk,")",sep=""),",",sep=" "))
  cat("\n")
  cat(paste("Total number of Y-variables: ", pY), sep = " ")
  cat("\n")
  cat("\n")
  cat("# Scaling options chosen")
  cat("\n")
  cat("All variables are centered by default")
  cat("\n")
  cat(paste("Individual variable scaling: ", var.scal), sep = " ")
  cat("\n")
  cat(paste("Block scaling : ", block.scal), sep = " ")
  cat("\n")
  cat("\n")
  cat(paste("For the",ncomp,"computed dimensions, outputs include:" ), sep = " ")
  cat("\n")
  cat("   - Global ($T.g) and X-block components ($Block$T.b),")
  cat("\n")
  cat("   - Global weights ($W.g) and X-block weigths ($Block$W.b),")
  cat("\n")
  cat("   - Y-weigths ($Y$W.Y),")
  cat("\n")
  cat("   - Cumulative explained variance ($cumexplained) for X and Y (see summary.MBWCov). ")
  cat("\n")
  cat("   - explained variance ($explained.X) by X-block and by dimension (see summary.MBWCov). ")
  cat("\n")
  cat("   - salience ($saliences) by X-block and by dimension (see summary.MBWCov). ")


}
