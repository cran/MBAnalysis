#' Print of \code{\link{ComDim}} Objects
#'
#' @param x An object resulting from \code{\link{ComDim}}.
#' @param ... further arguments passed to or from other methods.
#'
#' @seealso \code{\link{summary.ComDim}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' block=ham$block
#' res.comdim <- ComDim(X,block,name.block=names(block))
#' print(res.comdim)
#'
#' @export
#'
print.ComDim =  function (x, ...)
{

  n <- nrow(x$call$X)
  p <- ncol(x$call$X)
  nam <- x$call$name.block
  K <- length(nam)
  pk <- x$call$size.block

  var.scal <- x$call$scale
  block.scal <- x$call$scale.block

  ncomp <- x$call$ncomp

  cat("\n")
  cat(paste("Number of observations: ", n), sep = " ")
  cat("\n")
  cat(paste("Total number of variables: ", p), sep = " ")
  cat("\n")
  cat(paste("Number of blocks of variables: ", K), sep = " ")
  cat("\n")
  cat("Block's Name (Nb variables): ")
  cat(paste(paste(nam,"(",pk,")",sep=""),",",sep=" "))
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
  cat("   - Global ($T.g) and block components ($Block$T.b),")
  cat("\n")
  cat("   - Global weights ($W.g) and block weigths ($Block$W.b),")
  cat("\n")
  cat("   - Cumulative explained variance ($cumexplained), explained variance ($cumexplained) and salience ($saliences) by block and by dimension (see summary.ComDim). ")


}
