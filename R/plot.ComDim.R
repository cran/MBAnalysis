#' Main Graphs for Common Dimensions analysis (ComDim)
#'
#' Plot the main graphs for ComDim.
#'
#' @param x  An object of class \code{ComDim}.
#' @param axes  A vector of length two which specifies the global components to plot (by default the first two).
#' @param graphtype  Type of graph to plot. Either "saliences", "globalscores", "blockscores","globalcor","blockcor", "expl", "cumexpl", "crit" or "contrib".
#' Refer to the details section.
#' @param select  Selection of elements to plot. By default (NULL), all the elements are plotted. Refer to the details section.
#' @param max.overlaps  Exclude text labels that overlap too many things (by default, 20).
#' @param xlim  Range for the plotted 'x' values.
#' @param ylim  Range for the plotted 'y' values.
#' @param title   Title of the graph to draw.
#' @param color  Color for the plot.
#' @param ... Further arguments.
#'
#' @details The explanation of the arguments graphtype and select is as follow.\cr
#' If graphtype="saliences", the relationships between blocks of variables are shown.\cr
#' If select=NULL, all the blocks are shown, otherwise, only the selected ones are shown.\cr
#' If graphtype="globalscores", individuals are projected on the space formed by the global components.\cr
#' In this case, if for example, select=NULL, all the individuals are plotted. However, if select=5, only the first five individuals are plotted.\cr
#' If graphtype="blockscores", individuals are projected on the space formed by the block components.\cr
#' If select=NULL, individuals of each block are plotted on separate figures.\cr
#' If select=c(1,3), individuals of blocks 1 and 3 are plotted on separate figures.\cr
#' If graphtype="globalcor", correlations of original variables with the global components are depicted.\cr
#' If select=NULL, correlations of the variables of all the blocks are plotted on the same figure.\cr
#' If select=c(1,3), correlations of the variables of blocks 1 and 3 are plotted.\cr
#' If graphtype="blockcor", correlations of original variables with the block components are depicted.\cr
#' If select=NULL, correlations of the variables of each block are plotted on seperate figures.\cr
#' If select=c(1,3), correlations of the variables of blocks 1 and 3 are plotted.\cr
#' If graphtype="expl", percentages of inertia of all the blocks explained by the global components are drawn.\cr
#' If graphtype="cumexpl", cumulative percentages of inertia of all the blocks explained by the global components are drawn.\cr
#' graphtype="crit" plots the values of the maximization criterion.\cr
#' graphtype="contrib" depicts the contribution of each block of variables to the determination of the global components.\cr
#' For graphtype="expl", "cumexpl", "crit" and "contrib", if select=NULL, all the dimensions are plotted. \cr
#' But if for example, select=5, only the first five dimensions are plotted.\cr
#'
#
#' @return Returns graphs showing the relationships between blocks of variables, projection of individuals in both global and block components, the correlations of variables with the global and block components,
#' the percentages of inertia explained by the global components and their cumulative values, the values of the maximization criterion and the contributions of the blocks
#' to the determination of global components.
#'
#'
#' @author
#' Essomanda TCHANDAO MANGAMANA \email{tchanesso@@yahoo.fr}, Véronique CARIOU, Evelyne VIGNEAU.
#'
#' @seealso \code{\link{ComDim}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' group=ham$group
#' res.comdim <- ComDim(X, group, plotgraph=FALSE)
#' plot(res.comdim, graphtype="saliences")
#' plot(res.comdim, graphtype="globalcor")
#'
#' @export
#' @importFrom  ggplot2 ggplot aes theme_bw labs theme element_text element_blank geom_point geom_hline geom_vline geom_segment arrow unit geom_path  geom_bar scale_color_manual scale_fill_manual xlab ylab xlim ylim
#' @importFrom  ggrepel geom_text_repel
#' @importFrom  grDevices dev.new

# Plot method for ComDim class

plot.ComDim <- function(x, axes = c(1, 2), graphtype = c("saliences", "globalscores", "blockscores", "globalcor", "blockcor", "expl", "cumexpl", "crit", "contrib"), select=NULL, max.overlaps = 20, xlim = NULL, ylim = NULL, title=NULL, color=NULL, ...){

  Res     <- x
  if (!inherits(Res, "ComDim"))
    stop("non convenient data")

  appel   <- as.list(Res$call)
  group   <- eval.parent(appel$group)
  ntab    <- length(group)

  palcolor=c("black","red", "blue","maroon", "green3", "deeppink", "darkorchid", "darkgreen", "turquoise", "lightpink","darkgoldenrod","magenta", "violet", "orange", "cyan","darkgray", "lavender", "yellow","lightgreen", "lightgrey", "lightblue", "darkkhaki","darkmagenta", "darkolivegreen", "lightcyan", "darkorange","darkorchid", "darkred", "darksalmon", "darkseagreen","darkslateblue", "darkslategray", "darkslategrey","darkturquoise", "darkviolet", "lightgray", "lightsalmon","lightyellow")

  xlab=paste("Dim.",axes[1]," (", round(Res$cumexplained[axes[1],1], 2), "%)")
  ylab=paste("Dim.",axes[2]," (", round(Res$cumexplained[axes[2],1], 2), "%)")

  if (!graphtype %in% c("saliences", "globalscores", "blockscores","globalcor","blockcor", "expl", "cumexpl", "crit", "contrib")) stop("\n\ngraphtype must be either saliences, globalscores, blockscores, globalcor, blockcor, expl, cumexpl, crit or contrib. \n \n")

  if (graphtype=="saliences"){
    if (is.null(select)) {
      select=1:ntab
    } else {
      select=select
    }
    S=Res$saliences[select,];
    if (is.null(xlim))
      xlim<- c(0,max(S[,axes[1]]))*1.2
    else xlim=xlim
    if (is.null(ylim))
      ylim<- c(0,max(S[,axes[2]]))*1.2
    else ylim=ylim
    if (is.null(title))
      title=paste("ComDim: Specific weights of X blocks on dimensions", axes[1],"and", axes[2])
    else title<- title
    dev.new()
    p <- ggplot(as.data.frame(S), aes(S[,axes[1]],S[,axes[2]]))+
      theme_bw() + labs(title = title)+  xlab(xlab) +  ylab(ylab)+ xlim(xlim) + ylim(ylim)+
      theme(plot.title = element_text(hjust = 0.5))+ # center the title
      geom_text_repel(colour = palcolor[select],label = rownames(S),min.segment.length = 0,box.padding = 0.1, max.overlaps = max.overlaps)+
      theme(panel.grid.minor = element_blank()) +
      theme(panel.grid.major = element_blank()) +
      geom_point(colour = palcolor[select],size = 1)
    print(p)
    #dev.off()
  }

    if (graphtype=="globalscores"){
      if (is.null(select)) {
        select=1:nrow(Res$C)
      } else {
        select=select
      }
      C = Res$C[select,]
      c1=C[,axes[1]]; c2=C[,axes[2]]
      if (is.null(xlim))
        xlim<- c(min(c1),max(c1))*1.2
      else xlim=xlim
      if (is.null(ylim))
        ylim<- c(min(c2),max(c2))*1.2
      else ylim=ylim
      if (is.null(title))
        title=paste("ComDim: Projection of individuals in dimensions", axes[1],"and", axes[2])
      else title<- title

      dev.new()
      p <- ggplot(as.data.frame(C), aes(c1, c2))+
        theme_bw() + labs(title = title)+  xlab(xlab) +  ylab(ylab)+ xlim(xlim) + ylim(ylim)+
        theme(plot.title = element_text(hjust = 0.5))+ # center the title
        geom_hline(aes(yintercept = 0), linetype = 2)+
        geom_vline(aes(xintercept = 0), linetype = 2)+
        geom_text_repel(aes(label = rownames(C)),min.segment.length = 0,box.padding = 0.1, max.overlaps = max.overlaps)+
        theme(panel.grid.minor = element_blank()) +
        theme(panel.grid.major = element_blank()) +
        geom_point(colour = "black", size = 1)
      print(p)
      # dev.off()
    }

  if (graphtype=="blockscores"){
    if (is.null(select)) {
      select=1:ntab
    } else {
      select=select
    }

    for (i in select) {
      c1=Res$Block$T.b[,axes[1],i]; c2=Res$Block$T.b[,axes[2],i]

      xlab=paste("Dim.",axes[1]," (", round(Res$explained.X[i,axes[1]], 2), "%)")
      ylab=paste("Dim.",axes[2]," (", round(Res$explained.X[i,axes[2]], 2), "%)")
      dev.new()
      p <- ggplot(as.data.frame(Res$Block$T.b[,,i]), aes(c1, c2))+
        theme_bw() + labs(title = paste("ComDim: Projection of individuals of block",i,"in dimensions", axes[1],"and", axes[2]))+  xlab(xlab) +  ylab(ylab)+
        xlim(xlim=c(min(c1),max(c1))*1.2) + ylim(ylim=ylim<- c(min(c2),max(c2))*1.2)+
        theme(plot.title = element_text(hjust = 0.5))+ # center the title
        geom_hline(aes(yintercept = 0), linetype = 2)+
        geom_vline(aes(xintercept = 0), linetype = 2)+
        geom_text_repel(label = rownames(Res$Block$T.b[,,i]), aes(colour=palcolor[i]),min.segment.length = 0,box.padding = 0.1, max.overlaps = max.overlaps,show.legend=FALSE)+
        theme(panel.grid.minor = element_blank()) +
        theme(panel.grid.major = element_blank())+
        geom_point(aes(colour = palcolor[i]), size = 1)+#
        scale_color_manual(name = "Block", guide = "legend",values=palcolor[i],labels=names(group[i]))
      print(p)
      # dev.off()
    }
  }

    if (graphtype=="globalcor"){
      if (is.null(select)) {
        select=1:ntab
      } else {
        select=select
      }
      c1=Res$globalcor[,axes[1]]; c2=Res$globalcor[,axes[2]]; Block= as.factor(rep(1:ntab, group))
      CORVAR <- data.frame(c1,c2,Block)
      CORVAR <- CORVAR[CORVAR$Block %in% select, ]

      if (is.null(color))   palcolor
      else color=color

      if (is.null(xlim))
        xlim<- c(-1,1)
      else xlim=xlim
      if (is.null(ylim))
        ylim<- c(-1,1)
      else ylim=ylim
      if (is.null(title))
        title=paste("ComDim: Correlation of variables with dimensions", axes[1],"and", axes[2])
      else title<- title
      #pdf(file="Variables.pdf")
      dev.new()
      p <- ggplot(data=CORVAR, aes(x=c1, y=c2))+
        geom_path(data=circle(c(0,0),2,npoints = 1000) ,aes(x,y))+
        #geom_point(size = 1, aes(colour=Block))+
        theme_bw() + labs(title = title)+ xlab(xlab) +  ylab(ylab)+ xlim(xlim) + ylim(ylim)+
        theme(plot.title = element_text(hjust = 0.5))+ # center the title
        geom_hline(aes(yintercept = 0), linetype = 2)+
        geom_vline(aes(xintercept = 0), linetype = 2)+
        geom_text_repel(label = rownames(CORVAR), aes(colour=Block),min.segment.length = 0,box.padding = 0.1, max.overlaps = 20,show.legend=FALSE)+
        theme(panel.grid.minor = element_blank()) +
        theme(panel.grid.major = element_blank())+
        geom_segment(aes(x = 0,y = 0, xend = c1, yend = c2,colour=Block),arrow = arrow(length = unit(0.2,"cm")))+
        scale_color_manual(name = "Block", guide = "legend",values=palcolor[select],labels=names(group[select]))#1:ntab
      print(p)
      #dev.off()
    }


    if (graphtype=="blockcor"){
      if (is.null(select)) {
        select=1:ntab
      } else {
        select=select
      }

      if (is.null(color))   palcolor
      else color=color

      for (i in select) {
        c1=Res$Block$blockcor[[i]][,axes[1]]; c2=Res$Block$blockcor[[i]][,axes[2]]
        xlab=paste("Dim.",axes[1]," (", round(Res$explained.X[i,axes[1]], 2), "%)")
        ylab=paste("Dim.",axes[2]," (", round(Res$explained.X[i,axes[2]], 2), "%)")
        dev.new()
        p <- ggplot(data=as.data.frame(Res$Block$blockcor[[i]]), aes(x=c1, y=c2))+
          theme_bw() + labs(title = paste("ComDim: Correlation of variables of block",i," with dimensions", axes[1],"and", axes[2]))+
          xlab(xlab) +  ylab(ylab)+ xlim(xlim=c(-1,1)) + ylim(ylim=c(-1,1))+
          theme(plot.title = element_text(hjust = 0.5))+ # center the title
          geom_hline(aes(yintercept = 0), linetype = 2)+
          geom_vline(aes(xintercept = 0), linetype = 2)+
          geom_text_repel(label = rownames(Res$Block$blockcor[[i]]), aes(colour=palcolor[i]),min.segment.length = 0,box.padding = 0.1, max.overlaps = 20,show.legend=FALSE)+
          theme(panel.grid.minor = element_blank()) +
          theme(panel.grid.major = element_blank())+
          geom_segment(aes(x = 0,y = 0, xend = c1, yend = c2,colour=palcolor[i]),arrow = arrow(length = unit(0.2,"cm")))+#
          scale_color_manual(name = "Block", guide = "legend",values=palcolor[i],labels=names(group[i]))
        print(p)
        #dev.off()
      }
    }


    if (graphtype=="expl"){
      if (is.null(select)) {
        select=Res$components[2]
      } else {
        select=select
      }

      if (select > Res$components[2])  stop(cat("\nselect should be less or equal to", Res$components[2],"\n\n"))

      if (is.null(ylim))
        ylim<- c(0,max(Res$cumexplained[1:select,1]))
      else ylim=ylim
      if (is.null(title))
        title=paste("ComDim: Scree plot")
      else title<- title

      #pdf(file="Inertia.pdf")
      dev.new()
      p <- ggplot(as.data.frame(Res$cumexplained[1:select,]), aes(x=row.names(Res$cumexplained[1:select,]), y=Res$cumexplained[1:select,1]))+
        geom_bar(stat="identity", width=0.2,fill="blue")+
        theme_bw() + labs(title = title, x="Dimension",y="Percentages of inertia explained in all the blocks")+ ylim(ylim)+
        theme(plot.title = element_text(hjust = 0.5)) # center the title
      print(p)
      #dev.off()
    }

    if (graphtype=="cumexpl"){
      if (is.null(select)) {
        select=Res$components[2]
      } else {
        select=select
      }

      if (select > Res$components[2])  stop(cat("\nselect should be less or equal to", Res$components[2],"\n\n"))

      if (is.null(ylim))
        ylim<- c(0,max(Res$cumexplained[1:select,2]))
      else ylim=ylim
      if (is.null(title))
        title=paste("ComDim: Cumulative percentages of inertia explained in all the blocks")
      else title<- title

      #pdf(file="Inertia.pdf")
      dev.new()

      p <- ggplot(as.data.frame(Res$cumexplained[1:select,]), aes(x=row.names(Res$cumexplained[1:select,]), y=Res$cumexplained[1:select,2]))+
        geom_bar(stat="identity", width=0.2,fill="blue")+
        theme_bw() + labs(title = title, x="Dimension",y="Cumulative percentages of inertia explained in all the blocks")+ ylim(ylim)+
        theme(plot.title = element_text(hjust = 0.5)) # center the title
      print(p)
      #dev.off()
    }

    if (graphtype=="crit"){
      if (is.null(select)) {
        select=Res$components[2]
      } else {
        select=select
      }

      if (select > Res$components[2])  stop(cat("\nselect should be less or equal to", Res$components[2],"\n\n"))

      if (is.null(ylim))
        ylim<- c(0,max(Res$optimalcrit[1:select]))
      else ylim=ylim
      if (is.null(title))
        title=paste("ComDim: Values of the maximization criterion")
      else title<- title

      #pdf(file="Inertia.pdf")
      dev.new()
      p <- ggplot(as.data.frame(Res$optimalcrit[1:select]), aes(x=names(Res$optimalcrit[1:select]), y=Res$optimalcrit[1:select]))+
        geom_bar(stat="identity", width=0.2,fill="blue")+
        theme_bw() + labs(title = title, x="Dimension",y="Values of the maximization criterion")+ ylim(ylim)+
        theme(plot.title = element_text(hjust = 0.5)) # center the title
      print(p)
      #dev.off()
    }


    if (graphtype=="contrib"){
      if (is.null(select)) {
        select=Res$components[2]
      } else {
        select=select
      }

      if (select > Res$components[2])  stop(cat("\nselect should be less or equal to", Res$components[2],"\n\n"))

      if (is.null(title))
        title=paste("ComDim: Contribution of blocks to the determination of dimensions")
      else title<- title
      Dimension <- rep(colnames(Res$contrib),each=nrow(Res$contrib))
      Block <- rep(row.names(Res$contrib),times=ncol(Res$contrib))
      Value <- c(Res$contrib)
      data <- data.frame(Dimension,Block,Value)
      nr <- select*ntab
      data <- data[1:nr,]
      #pdf(file="Inertia.pdf")
      dev.new()

      p <- ggplot(data, aes(x=Dimension, y=Value, fill=Block)) +
        geom_bar(position="dodge", stat="identity", width=0.3)+
        scale_fill_manual(values=palcolor[1:nrow(Res$contrib)])+
        theme_bw() + labs(title = title, x="Dimension",y="Contribution of blocks to the determination of various dimensions")+
        theme(plot.title = element_text(hjust = 0.5)) # center the title
      print(p)
      #dev.off()
    }

  }
