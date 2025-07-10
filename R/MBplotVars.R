#' Customizable plots of variables related information
#'
#' Plots variables related information of \code{\link{MBPCA}}, \code{\link{ComDim}}, \code{\link{MBPLS}} or \code{\link{MBWCov}} with several options of customization.
#'
#' @param res An object resulting from \code{\link{MBPCA}}, \code{\link{ComDim}}, \code{\link{MBPLS}} or \code{\link{MBWCov}}.
#' @param axes Which dimensions should be plotted?
#' @param which Either "correlation" or "loading".
#' @param block Selection of variables by blocks. A number or integer, possibly a vector, corresponding to the index of the blocks from which the variables should be plotted. For \code{\link{MBPLS}} and \code{\link{MBWCov}} the Y response block corresponds to the index \emph{length(res$call$size.block)+1}. By default, all variables of all blocks are plotted. Only one of \emph{block} and \emph{select} can differ from 0 (the default) both at the same time.
#' @param select Selection of variables by index. A number or integer, possibly a vector, corresponding to the index of the variables that should be plotted. For \code{\link{MBPLS}} and \code{\link{MBWCov}} the Y variables index start from \emph{ncol(res$call$X)+1}. By default, all variables of all blocks are plotted. Only one of \emph{block} and \emph{select} can differ from 0 (the default) both at the same time.
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
#' MBplotVars(res.mbpca)
#'
#' # Supervised example
#'
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, block, name.block=names(block))
#' MBplotVars(res.mbpls)
#'
#' @import ggplot2
#' @import ggrepel
#' @import stats
#' @import grDevices
#'
#' @export


MBplotVars=function(res,axes=c(1,2),which=ifelse(res$call$scale,"correlation","loading"),block=0,select=0,title=NULL,size=2.25){
  if (!inherits(res,c("MBPCA","ComDim","MBPLS","MBWCov"))){
    stop("First argument must come from MBAnalysis")
  }
  if (is.integer(axes) | is.numeric(axes)){
    if (length(axes)!=2){
      stop("length(axes) must be 2")
    }else{
      if (max(axes)>res$call$ncomp){
        stop("max(axes) must be lower than or equal to res$components")
      }
    }
  }else{
    stop("axes must be of class integer or numeric (e.g. c(1,2)")
  }
  if (!which%in%c("loading","correlation")){
    stop("which must be loading or correlation")
  }
  if (is.integer(block) | is.numeric(block)){
    if (any(block<0)){
      stop("block must be positive")
    }
    if (max(block)>length(res$call$size.block) & is.null(res$Y)){
      stop("max(block) must be lower than or equal to length(res$call$size.block)")
    }
    if (!is.null(res$Y)){
      if (max(block)>(length(res$call$size.block)+1)){
        stop("max(block) must be lower than or equal to length(res$call$size.block)+1")
      }
    }
    if (length(block)>length(res$call$size.block) & is.null(res$Y)){
      stop("length(block) must be lower than or equal to length(res$call$size.block)")
    }
    if (!is.null(res$Y)){
      if (length(block)>(length(res$call$size.block)+1)){
        stop("length(block) must be lower than or equal to length(res$call$size.block)+1")
      }
    }
  }else{
    stop("class(block) must be integer or numeric")
  }
  if (is.integer(select) | is.numeric(select)){
    if (any(select<0)){
      stop("select must be positive")
    }
    if (max(select)>ncol(res$call$X) & is.null(res$Y)){
      stop("max(select) must be lower than or equal to ncol(res$call$X)")
    }
    if (!is.null(res$Y)){
      if (max(select)>(ncol(res$call$X)+ncol(res$call$Y))){
        stop("max(select) must be lower than or equal to (ncol(res$call$X)+ncol(res$call$Y))")
      }
    }
    if (length(select)>ncol(res$call$X) & is.null(res$Y)){
      stop("length(select) must be lower than or equal to ncol(res$call$X)")
    }
    if (!is.null(res$Y)){
      if (length(select)>(ncol(res$call$X)+ncol(res$call$Y))){
        stop("length(select) must be lower than or equal to (ncol(res$call$X)+ncol(res$call$Y))")
      }
    }
  }else{
    stop("class(select) must be integer or numeric")
  }
  if (!setequal(block,0) & !setequal(select,0)){
    stop("block and select cannot differ from 0 (the default) both at the same time")
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
  if (!is.null(res$Y)){
    XY=cbind(pretreatment(res),pretreatmentY(res))
  }
  bp1=length(res$call$size.block)+1
  if (!setequal(block,0) | !setequal(select,0)){
    vec.if=c(!setequal(block,0),!setequal(select,0))
    if (vec.if[1]){
      vars.selected=c()
      for (b in block){
        if (b==1){
          vars.selected=c(vars.selected,1:res$call$size.block[1])
        }else if (b>1 & b<bp1){
          vars.selected=c(vars.selected,(1+sum(res$call$size.block[(1:(b-1))])):(res$call$size.block[b]+sum(res$call$size.block[(1:(b-1))])))
        }else{
          vars.selected=c(vars.selected,(sum(res$call$size.block)+1):ncol(XY))
        }
      }
    }else{
      vars.selected=select
    }
  }else{
    if (!is.null(res$Y)){
      vars.selected=1:ncol(XY)
    }else{
      vars.selected=1:ncol(res$call$X)
    }
  }
  colors <- c("blue", "red","green3","orange2","deepskyblue1","darkgrey","chocolate","violet","pink2","purple","khaki3")
  if (length(res$call$size.block)<=length(colors)){
    col.plot=colors[1:length(res$call$size.block)]
  }else{
    col.plot=rainbow(length(res$call$size.block))
  }
  if (!is.null(res$Y)){
    col.plot=c(col.plot,"black")
  }
  if (!is.null(res$Y)){
    color=rep(col.plot,c(res$call$size.block,ncol(res$call$Y)))[vars.selected]
  }else{
    color=rep(col.plot,res$call$size.block)[vars.selected]
  }
  if (!is.null(res$Y)){
    block.belong=factor(rep(c(res$call$name.block,"Y"),c(res$call$size.block,ncol(res$call$Y)))[vars.selected],levels = c(res$call$name.block,"Y"))
  }else{
    block.belong=factor(rep(res$call$name.block,res$call$size.block)[vars.selected],levels = res$call$name.block)
  }
  if (which=="loading"){
    if (!is.null(res$Y)){
      mat1=res$Scor.g[,axes]
      mat2=as.matrix(XY)[,vars.selected,drop=FALSE]
      calc.coord=round(chol2inv(chol(crossprod(mat1))),12)%*%crossprod(mat1,mat2)
      matplot=t(calc.coord)
    }else{
      mat1=res$Scor.g[,axes]
      mat2=as.matrix(pretreatment(res))[,vars.selected,drop=FALSE]
      calc.coord=round(chol2inv(chol(crossprod(mat1))),12)%*%crossprod(mat1,mat2)
      matplot=t(calc.coord)
    }
  }else{
    if (!is.null(res$Y)){
      mat1=res$Scor.g[,axes]
      mat2=as.matrix(XY)[,vars.selected,drop=FALSE]
      calc.coord=cor(mat1,mat2)
      matplot=t(calc.coord)
    }else{
      mat1=res$Scor.g[,axes]
      mat2=as.matrix(pretreatment(res))[,vars.selected,drop=FALSE]
      calc.coord=cor(mat1,mat2)
      matplot=t(calc.coord)
    }
  }
  pmin=min(min(matplot)*1.05,-0.05)
  pmax=max(matplot)*1.05
  if (which=="correlation"){
    pmin=-1.05
    pmax=1.05
  }
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
  matplot.block=cbind.data.frame(matplot,block.belong)
  p = ggplot(as.data.frame(matplot.block), aes(x = matplot.block[,1], y = matplot.block[,2]),color= matplot.block[,3]) + theme_bw()
  p = p + xlim(pmin, pmax) + ylim(pmin, pmax) + xlab(hlab) + ylab(vlab) + ggtitle(title)
  p = p + theme(axis.title.x = element_text(size = 5*size, face = "bold"),axis.title.y = element_text(size = 5*size, face = "bold"),plot.title = element_text(hjust = 0.5, face = "bold",size = 7*size),axis.text = element_text(size=3*size))
  p = p + geom_hline(yintercept = 0, linetype = "dashed",linewidth = 1) + geom_vline(xintercept = 0,linetype = "dashed",linewidth = 1)
  if (which=="correlation"){
    p = p + annotate("path",x=0+cos(seq(0,2*pi,length.out=100)),y=0+sin(seq(0,2*pi,length.out=100)),linewidth=1)
    p = p + annotate("path",x=0+sqrt(0.50)*cos(seq(0,2*pi,length.out=100)),y=0+sqrt(0.50)*sin(seq(0,2*pi,length.out=100)),linewidth=1)
  }
  p = p + geom_segment(data = as.data.frame(matplot.block),aes(x = 0,y = 0, xend = matplot.block[, 1], yend = matplot.block[, 2],colour = matplot.block[,3]),arrow = arrow(length = unit(0.3, "cm"), type = "open"), linewidth = 1)
  p = p + scale_color_manual(name="Block:",values=unique(color))+theme(legend.title = element_text(size = 5*size, face = "bold"),legend.text = element_text(size = 5*size),legend.position = "top")
  lab=matplot
  nudge = lab * 0.01
  p = p + geom_label_repel(as.data.frame(lab), mapping = aes(x = lab[,1], y = lab[, 2], label = rownames(lab)), label.size = NA,colour = color, size = size*2, segment.size = 1,label.padding = 0, nudge_x = nudge[, 1], nudge_y = nudge[,2],fill="gray100", min.segment.length = 1)
  return(p)
}
