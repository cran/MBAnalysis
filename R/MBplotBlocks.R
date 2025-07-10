#' Customizable plots of blocks related information
#'
#' Plots blocks related information of \code{\link{MBPCA}}, \code{\link{ComDim}}, \code{\link{MBPLS}} or \code{\link{MBWCov}} with several options of customization.
#'
#' @param res An object resulting from \code{\link{MBPCA}}, \code{\link{ComDim}}, \code{\link{MBPLS}} or \code{\link{MBWCov}}.
#' @param which Either "explained.blocks&Y", "scree", "structure" or "blocks.axes". See details.
#' @param axes Which global dimensions should be plotted? Only useful if \emph{which=structure} or \emph{which=blocks.axes}
#' @param blocks.axes Which individual blocks dimensions should be correlated with global ones? Only useful if \emph{which=blocks.axes}
#' @param title An optional title to be added to the plot.
#' @param size The overall size of labels, points, etc.
#'
#' @details
#' \itemize{
#'   \item \strong{explained.blocks&Y}: Barplot of the percentages of inertia explained in each block of variables (and Y for \code{\link{MBPLS}} or \code{\link{MBWCov}}) by each global components.
#'   \item \strong{scree}: Barplot of the saliences of each block of variables on each global components.
#'   \item \strong{structure}: Blocks coordinates (saliences) on the global selected \emph{axes}
#'   \item \strong{blocks.axes}: Correlations of the selected individual \emph{blocks.axes} with the global selected \emph{axes}.
#' }
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
#' MBplotBlocks(res.mbpca,which="explained.blocks&Y")
#' MBplotBlocks(res.mbpca,which="scree")
#' MBplotBlocks(res.mbpca,which="structure")
#' MBplotBlocks(res.mbpca,which="blocks.axes")
#'
#' # Supervised example
#'
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, block, name.block=names(block))
#' MBplotBlocks(res.mbpls,which="explained.blocks&Y")
#' MBplotBlocks(res.mbpls,which="scree")
#' MBplotBlocks(res.mbpls,which="structure")
#' MBplotBlocks(res.mbpls,which="blocks.axes")
#'
#' @import ggplot2
#' @import ggrepel
#' @import grDevices
#' @import stats
#'
#' @export


MBplotBlocks=function(res,which="explained.blocks&Y",axes=c(1,2),blocks.axes=1:max(axes),title=NULL,size=2.25){
  if (!inherits(res,c("MBPCA","ComDim","MBPLS","MBWCov"))){
    stop("First argument must come from MBAnalysis")
  }
  if (!which%in%c("explained.blocks&Y","scree","structure","blocks.axes")){
    stop("which must be explained.blocks&Y, scree, structure or blocks.axes")
  }
  if (which=="structure"){
    if (is.integer(axes) | is.numeric(axes)){
      if (length(axes)!=2){
        stop("length(axes) must be 2")
      }else{
        if (max(axes)>res$call$ncomp){
          stop("max(axes) must be lower than or equal to res$call$ncomp")
        }
        if (min(axes)<=0){
          stop("min(axes) must be larger than 0")
        }
      }
    }else{
      stop("axes must be of class integer or numeric (e.g. c(1,2)")
    }
  }
  if (which=="block axes"){
    if (!is.integer(blocks.axes) & !is.numeric(blocks.axes)){
      stop("blocks.axes must be of class integer or numeric (e.g. 1 or 1:5)")
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
  colors <- c("blue", "red","green3","orange2","deepskyblue1","darkgrey","chocolate","violet","pink2","purple","khaki3")
  if (length(res$call$size.block)<=length(colors)){
    col.plot=colors[1:length(res$call$size.block)]
  }else{
    col.plot=rainbow(length(res$call$size.block))
  }
  if (which=="structure"){
    matplot=res$saliences[,axes]
    pmin=-0.05
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
    matplot.block=cbind.data.frame(matplot,block=factor(res$call$name.block,levels=res$call$name.block))
    p = ggplot(as.data.frame(matplot.block), aes(x = matplot.block[,1], y = matplot.block[,2]),color= matplot.block[,3]) + theme_bw()
    p = p + xlim(pmin, pmax) + ylim(pmin, pmax) + xlab(hlab) + ylab(vlab) + ggtitle(title)
    p = p + theme(axis.title.x = element_text(size = 5*size, face = "bold"),axis.title.y = element_text(size = 5*size, face = "bold"),plot.title = element_text(hjust = 0.5, face = "bold",size = 7*size),axis.text = element_text(size=3*size))
    p = p + geom_hline(yintercept = 0, linetype = "dashed",linewidth = 1) + geom_vline(xintercept = 0,linetype = "dashed",linewidth = 1)
    p = p + geom_point(as.data.frame(matplot.block),mapping= aes(x = matplot.block[,1], y = matplot.block[, 2],fill = matplot.block[, 3]), size = size,shape=21,color="black")
    p = p + scale_fill_manual(name="Block:",values=col.plot)+theme(legend.title = element_text(size = 5*size, face = "bold"),legend.text = element_text(size = 5*size),legend.position = "top")
    lab=matplot
    p = p + geom_label_repel(as.data.frame(lab), mapping = aes(x = lab[,1], y = lab[, 2], label = rownames(lab)), label.size = NA,colour = col.plot, size = size*2, segment.size = 1,label.padding = 0, min.segment.length = 1)
  }else if (which=="scree"){
    vecplot=as.vector(res$saliences)
    matplot=data.frame(inertie=vecplot,block=factor(rep(rownames(res$saliences),ncol(res$saliences)),levels = res$call$name.block),dimension=factor(rep(colnames(res$saliences),each=nrow(res$saliences)),levels = unique(rep(colnames(res$saliences),each=nrow(res$saliences)))))
    vlab = "Salience (Absolute)"
    p = ggplot(as.data.frame(matplot), aes(x = matplot[,3], y = matplot[,1],fill=matplot[,2])) + theme_bw()
    p = p + ylab(vlab) + ggtitle(title) + ylim(0,max(vecplot)*1.05)
    p = p + theme(axis.text.x = element_text(size=4*size,angle = 38,vjust=0.60),axis.title.x = element_blank(),axis.title.y = element_text(size = 5*size, face = "bold"),plot.title = element_text(hjust = 0.5, face = "bold",size = 7*size))
    p = p + scale_fill_manual(name="Block:",values=col.plot)+theme(legend.title = element_text(size = 5*size, face = "bold"),legend.text = element_text(size = 5*size),legend.position = "top")
    p = p + geom_bar(stat="identity",position = position_dodge(),color="black")
  }else if (which=="explained.blocks&Y"){
    if (is.null(res$Y)){
      vecplot=as.vector(res$explained.X)
      matplot=data.frame(inertie=vecplot,block=factor(rep(rownames(res$explained.X),ncol(res$explained.X)),levels = res$call$name.block),dimension=factor(rep(colnames(res$explained.X),each=nrow(res$explained.X)),levels = unique(rep(colnames(res$explained.X),each=nrow(res$explained.X)))))
      vlab = "Explained Variance (Percentage)"
      p = ggplot(as.data.frame(matplot), aes(x = matplot[,3], y = matplot[,1],fill=matplot[,2])) + theme_bw()
      p = p + ylab(vlab) + ggtitle(title) + ylim(0,105)
      p = p + theme(axis.text.x = element_text(size=4*size,angle = 38,vjust=0.60),axis.title.x = element_blank(),axis.title.y = element_text(size = 5*size, face = "bold"),plot.title = element_text(hjust = 0.5, face = "bold",size = 7*size))
      p = p + scale_fill_manual(name="Block:",values=col.plot)+theme(legend.title = element_text(size = 5*size, face = "bold"),legend.text = element_text(size = 5*size),legend.position = "top")
      p = p + geom_bar(stat="identity",position = position_dodge(),color="black")
    }else{
      vecplot=as.vector(rbind(res$explained.X,res$cumexplained[,3]))
      matplot=data.frame(inertie=vecplot,block=factor(rep(c(rownames(res$explained.X),"Y"),ncol(res$explained.X)),levels = c(res$call$name.block,"Y")),dimension=factor(rep(colnames(res$explained.X),each=nrow(res$explained.X)+1),levels = unique(rep(colnames(res$explained.X),each=nrow(res$explained.X)+1))))
      vlab = "Explained Variance (Percentage)"
      p = ggplot(as.data.frame(matplot), aes(x = matplot[,3], y = matplot[,1],fill=matplot[,2])) + theme_bw()
      p = p + ylab(vlab) + ggtitle(title) + ylim(0,105)
      p = p + theme(axis.text.x = element_text(size=4*size,angle = 38,vjust=0.60),axis.title.x = element_blank(),axis.title.y = element_text(size = 5*size, face = "bold"),plot.title = element_text(hjust = 0.5, face = "bold",size = 7*size))
      p = p + scale_fill_manual(name="Block:",values=c(col.plot,"black"))+theme(legend.title = element_text(size = 5*size, face = "bold"),legend.text = element_text(size = 5*size),legend.position = "top")
      p = p + geom_bar(stat="identity",position = position_dodge(),color="black")
    }
  }else{
    matplot=data.frame(matrix(0,length(blocks.axes)*length(res$call$size.block),2),block=rep(factor(res$call$name.block,levels=res$call$name.block),each=length(blocks.axes)))
    rownames(matplot)=paste(matplot$block,rep(paste("Dim",blocks.axes,sep=""),length(res$call$size.block)),sep=".")
    for (b in 1:length(res$call$size.block)){
      if (b==1){
        Xb=pretreatment(res)[,1:res$call$size.block[b]]
      }else{
        Xb=pretreatment(res)[,(1+sum(res$call$size.block[(1:(b-1))])):(res$call$size.block[b]+sum(res$call$size.block[(1:(b-1))]))]
      }
      udv=svd(tcrossprod(Xb),nu=max(blocks.axes),nv=max(blocks.axes))
      toaddmatplot=cor(udv$u[,blocks.axes,drop=FALSE],res$Scor.g[,axes])
      if (any(udv$d[1:max(blocks.axes)]<1e-12)){
        ou.zero=which(udv$d[1:max(blocks.axes)]<1e-12)
        toaddmatplot[ou.zero,]=0
        warning("Blocks axes with null singular values (<1e-12) has received null correlations with global axes")
      }
      matplot[matplot$block==res$call$name.block[b],1:2]=toaddmatplot
    }
    pmin=-1.05
    pmax=1.05
    ratio.var=res$cumexplained[axes,1]
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
    p = ggplot(as.data.frame(matplot), aes(x = matplot[,1], y = matplot[,2]),color= matplot[,3]) + theme_bw()
    p = p + xlim(pmin, pmax) + ylim(pmin, pmax) + xlab(hlab) + ylab(vlab) + ggtitle(title)
    p = p + annotate("path",x=0+cos(seq(0,2*pi,length.out=100)),y=0+sin(seq(0,2*pi,length.out=100)),linewidth=1)
    p = p + annotate("path",x=0+sqrt(0.50)*cos(seq(0,2*pi,length.out=100)),y=0+sqrt(0.50)*sin(seq(0,2*pi,length.out=100)),linewidth=1)
    p = p + theme(axis.title.x = element_text(size = 5*size, face = "bold"),axis.title.y = element_text(size = 5*size, face = "bold"),plot.title = element_text(hjust = 0.5, face = "bold",size = 7*size),axis.text = element_text(size=3*size))
    p = p + geom_hline(yintercept = 0, linetype = "dashed",linewidth = 1) + geom_vline(xintercept = 0,linetype = "dashed",linewidth = 1)
    p = p + geom_segment(data = as.data.frame(matplot),aes(x = 0,y = 0, xend = matplot[, 1], yend = matplot[, 2],colour = matplot[,3]),arrow = arrow(length = unit(0.3, "cm"), type = "open"), linewidth = 1)
    p = p + scale_color_manual(name="Block:",values=unique(col.plot))+theme(legend.title = element_text(size = 5*size, face = "bold"),legend.text = element_text(size = 5*size),legend.position = "top")
    lab=matplot[,1:2]
    nudge = lab * 0.01
    p = p + geom_label_repel(as.data.frame(lab), mapping = aes(x = lab[,1], y = lab[, 2], label = rownames(lab)), label.size = NA,colour = rep(col.plot,each=length(blocks.axes)), size = size*2, segment.size = 1,label.padding = 0, nudge_x = nudge[, 1], nudge_y = nudge[,2],fill="gray100", min.segment.length = 1)
  }
  return(p)
}
