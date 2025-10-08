#' Customizable Plots of Scores
#'
#' Plots scores related information of \code{\link{MBPCA}}, \code{\link{ComDim}}, \code{\link{MBPLS}} or \code{\link{MBWCov}} with several options of customization.
#'
#' @param res An object resulting from \code{\link{MBPCA}}, \code{\link{ComDim}}, \code{\link{MBPLS}} or \code{\link{MBWCov}}.
#' @param axes Which dimensions should be plotted?
#' @param block Of which block? Block 0 corresponds to global components.
#' @param color Either NULL (default) or a character vector of length \emph{select}. Controls the color of each individual plotted. Useful if individuals pertain to different a priori known groups. By default individuals are colored in black for global components and in the block color (the same as in \code{\link{MBplotVars}}) for block components.
#' @param select A numeric or integer vector to select which individuals should be plotted. By default, all individuals are plotted.
#' @param title An optional title to be added to the plot.
#' @param size The overall size of labels, points, etc.
#'
#' @return The required plot.
#'
#' @seealso \code{\link{plot.MBPCA}} \code{\link{plot.ComDim}} \code{\link{plot.MBPLS}} \code{\link{plot.MBWCov}}
#'
#' @examples
#' # Unsupervised example
#'
#' data(ham)
#' X=ham$X
#' block=ham$block
#' res.mbpca <- MBPCA(X,block, name.block=names(block))
#' MBplotScores(res.mbpca)
#'
#' # Supervised example
#'
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, block, name.block=names(block))
#' MBplotScores(res.mbpls)
#'
#' @import ggplot2
#' @import ggrepel
#' @import grDevices
#'
#' @export


MBplotScores=function(res,axes=c(1,2),block=0,color=NULL,select=1:nrow(res$Scor.g),title=NULL,size=2.25){
  if (!inherits(res,c("MBPCA","ComDim","MBPLS","MBWCov"))){
    stop("First argument must come from MBAnalysis")
  }
  if (is.integer(axes) | is.numeric(axes)){
    if (length(axes)!=2){
      stop("length(axes) must be 2")
    }else{
      if (max(axes)>res$call$ncomp){
        stop("max(axes) must be lower than or equal to res$call$ncomp")
      }
    }
  }else{
    stop("axes must be of class integer or numeric (e.g. c(1,2)")
  }
  if (is.integer(block) | is.numeric(block)){
    if (length(block)!=1){
      stop("length(block) must be 1")
    }else{
      if (block<0){
        stop("block must be positive")
      }
      if (block>length(res$call$size.block)){
        stop("block must be lower than or equal to length(res$call$size.block)")
      }
    }
  }else{
    stop("block must be of class integer or numeric (e.g. 0)")
  }
  if (is.integer(select) | is.numeric(select)){
    if (any(select<=0)){
      stop("select must contain only postitive integers")
    }
    if (any(select>nrow(res$Scor.g))){
      stop("max(select) should be lower than or equal to nrow(res$Scor.g)")
    }
  }else{
    stop("select must be of class integer or numeric (e.g. 1:10 or c(1,3,10)")
  }
  if (!is.null(color) & !is.character(color)){
    stop("color must NULL or of class character")
  }
  if (is.null(color)){
    if (block==0){
      color=rep("black",length(select))
    }else{
      colors <- c("blue", "red","green3","orange2","deepskyblue1","darkgrey","chocolate","violet","pink2","purple","khaki3")
      if (length(res$call$size.block)<=length(colors)){
        color=rep(colors[block],length(select))
      }else{
        color=rep(rainbow(length(res$call$size.block))[block],length(select))
      }
    }
  }else{
    if (length(color)!=length(select)){
      stop("When color is of class character, length(color) must equal length(select)")
    }
  }
  if (!is.null(title) & !is.character(title)){
    stop("title must be NULL or of class character")
  }
  if (is.numeric(size)){
    if (length(size)>1){
      stop("length(size) must be 1")
    }
  }else{
    stop("size must be of class numeric")
  }
  if (block==0){
    matplot=res$Scor.g[select,axes,drop=FALSE]
    pmin=min(matplot)*1.05
    pmax=max(matplot)*1.05
    if (is.null(res$Y)){
      ratio.var=res$cumexplained[axes,1]
      hlab=paste("Dim.",axes[1], " (", ratio.var[1], "%)",sep = "")
      vlab=paste("Dim.",axes[2], " (", ratio.var[2], "%)",sep = "")
    }else{
      ratio.var.X=res$cumexplained[axes,1]
      ratio.var.Y=res$cumexplained[axes,3]
      hlab=paste("Dim.",axes[1], " (", ratio.var.X[1], "% X) (",ratio.var.Y[1],"% Y)",sep = "")
      vlab=paste("Dim.",axes[2], " (", ratio.var.X[2], "% X) (",ratio.var.Y[2],"% Y)",sep = "")
    }
  }else{
    matplot=as.data.frame(res$Block$T.b[select,axes,block,drop=FALSE])
    pmin=min(matplot)*1.05
    pmax=max(matplot)*1.05
    hlab=paste("Dim.",axes[1],sep = "")
    vlab=paste("Dim.",axes[2],sep = "")
  }
  p = ggplot(as.data.frame(matplot), aes(x = matplot[,1], y = matplot[,2])) + theme_bw()
  p = p + xlim(pmin, pmax) + ylim(pmin, pmax) + xlab(hlab) + ylab(vlab) + ggtitle(title)
  p = p + theme(axis.title.x = element_text(size = 5*size, face = "bold"),axis.title.y = element_text(size = 5*size, face = "bold"),plot.title = element_text(hjust = 0.5, face = "bold",size = 7*size),axis.text = element_text(size=3*size))
  p = p + geom_hline(yintercept = 0, linetype = "dashed",linewidth = 1) + geom_vline(xintercept = 0,linetype = "dashed",linewidth = 1)
  p = p + geom_point(data = as.data.frame(matplot), aes(x = matplot[,1], y = matplot[, 2]), fill = color, color="black",shape=21,size = size)
  lab=matplot
  nudge = lab * 0.01
  p = p + geom_label_repel(as.data.frame(lab), mapping = aes(x = lab[,1], y = lab[, 2], label = rownames(lab)), label.size = NA,colour = color, size = size*2, segment.size = 1,label.padding = 0, nudge_x = nudge[, 1], nudge_y = nudge[,2], fill="gray100",min.segment.length = 1)
  return(p)
}
