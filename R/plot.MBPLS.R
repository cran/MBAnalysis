#' Default plots for \code{\link{MBPLS}} objects
#'
#' Successively performs \code{\link{MBplotScores}}, \code{\link{MBplotVars}} and \code{\link{MBplotBlocks}} with the default values of parameters but axes and size.
#'
#' @param x An object resulting from \code{\link{MBPLS}}.
#' @param axes Which dimensions should be plotted?
#' @param size The overall size of labels, points, etc.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The default plots.
#'
#' @seealso \code{\link{MBplotScores}} \code{\link{MBplotVars}} \code{\link{MBplotBlocks}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, block, name.block = names(block))
#' plot(res.mbpls)
#'
#' @import ggplot2
#' @import ggrepel
#'
#' @export

plot.MBPLS=function(x,axes=c(1,2),size=2.25,...){
  
  if (is.integer(axes) | is.numeric(axes)){
    if (length(axes)!=2){
      stop("length(axes) must be 2")
    }else{
      if (max(axes)>x$call$ncomp){
        stop("max(axes) must be lower than or equal to x$call$ncomp")
      }
    }
  }else{
    stop("axes must be of class integer or numeric (e.g. c(1,2)")
  }
  if (is.numeric(size)){
    if (length(size)>1){
      stop("length(size) must be 1")
    }
  }else{
    stop("size must be of class numeric")
  }
  print(MBplotScores(x,axes=axes,size=size))
  print(MBplotVars(x,axes = axes,size=size,block = 1:length(x$call$size.block)))
  print(MBplotVars(x,axes = axes,size=size,block = length(x$call$size.block)+1,which="correlation"))
  print(MBplotBlocks(x,axes=axes,size=size))
}
