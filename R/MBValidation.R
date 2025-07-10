#' Cross-Validation of MBPLS or MBWCov models
#'
#' Computes MSEP and corresponding standard error based on Leave One Out (LOO) or Out Of Bag (OOB) Cross-Validation (CV) by number of components of a MBPLS or MBWCov model from \code{\link{MBPLS}} or \code{\link{MBWCov}}.
#'
#' @param res An object resulting from \code{\link{MBPLS}} or \code{\link{MBWCov}}.
#' @param ncomp.max The maximum number of components to be investigated in the CV procedure.
#' @param method Either "LOO" or "OOB". Default is LOO.
#' @param nboot Number of bootstrap samples to be generated in case of OOB CV.
#' @param graph Logical. Should the results be plotted? Default is TRUE.
#' @param size.graph If \emph{graph=TRUE}, the overall size of labels, points, etc.
#'
#' @return A matrix with two rows (MSEP and std.error) and \emph{ncomp.max+1} columns. The +1 column corresponds to the null model (Dim.0) where Y is predicted by its empirical average on the training sample.
#'
#' @seealso \code{\link{predict.MBPLS}}   \code{\link{predict.MBWCov}}
#'
#' @examples
#' # With MBPLS
#'
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, block, name.block = names(block))
#' MBValidation(res.mbpls)
#'
#' # With MBWCov
#'
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbwcov <- MBWCov(X, Y, block, name.block = names(block))
#' MBValidation(res.mbwcov)
#'
#' @import ggplot2
#' @import ggrepel
#' @import stats
#' @import utils
#'
#'
#' @export

MBValidation=function(res,ncomp.max=min(res$call$ncomp,nrow(res$call$X)-2,ncol(X)),method="LOO",nboot=1000,graph=TRUE,size.graph=2.25){
  if (!inherits(res, c("MBPLS","MBWCov"))){
    stop("class(res) must be MBPLS or MBWCov")
  }
  if (res$call$ncomp==0){
    stop("res$call$ncomp must be strictly larger than 0")
  }
  if (is.numeric(ncomp.max) | is.integer(ncomp.max)){
    if (ncomp.max<=0){
      stop("ncomp.max must be strictly larger than 0")
    }
  }else{
    stop("class(ncomp.max) must be numeric or integer")
  }
  if (is.character(method)){
    if (!method%in%c("LOO","OOB")){
      stop("method must equal LOO or OOB")
    }
  }else{
    stop("class(method) must be character")
  }
  if (method=="OOB"){
    if (is.numeric(nboot) | is.integer(nboot)){
      if (nboot<30){
        stop("nboot must be larger than or equal to 30")
      }
    }else{
      stop("class(nboot) must be numeric or integer")
    }
  }
  if (!is.logical(graph)){
    stop("class(graph) must be logical (i.e TRUE or FALSE)")
  }
  if (is.numeric(size.graph) | is.integer(size.graph)){
    if (size.graph<=0){
      stop("size.graph must be larger than 0")
    }
  }else{
    stop("class(size.graph) must ne numeric or integer")
  }

  X=res$call$X
  Y=res$call$Y

  if (is.numeric(ncomp.max) | is.integer(ncomp.max)){
    if (ncomp.max>res$call$ncomp){
      stop("ncomp.max must be lower than or equal to res$call$ncomp")
    }
    if (method=="OOB"){
      maxh.oob=qbinom(1-(1/(nboot*10)),nrow(X),1-exp(-1),lower.tail = FALSE)
      if (ncomp.max>maxh.oob & maxh.oob<=ncol(X)){
        warning(paste("Given nrow(X) and required nboot for OOB validation, method OOB should be performed with ncomp.max lower than or equal to ",maxh.oob,"\n"," Validation has been performed using method = OOB and considering ncomp.max = ",maxh.oob,sep=""))
        ncomp.max=maxh.oob
      }
    }
    if (method=="LOO" & (nrow(X)-2)<=ncol(X) & ncomp.max>(nrow(X)-2)){
      warning(paste("Given nrow(X)<=ncol(X) and required LOO validation, ncomp.max must be lower than or equal to ",(nrow(X)-2),"\n"," LOO has been performed with ncomp.max = ",(nrow(X)-2),sep=""))
      ncomp.max=(nrow(X)-2)
    }
  }else{
    stop("class(ncomp.max) must be numeric or integer")
  }
  if (method=="LOO"){
    error.stock=matrix(0,nrow(X),ncomp.max+1)
    dimnames(error.stock)=list(paste("loo",1:nrow(X),sep="."),paste("Dim",0:ncomp.max,sep="."))
    pb <- txtProgressBar(min = 0, max = nrow(X), style = 3)
    for (loo in 1:nrow(X)){
      inbag=(1:nrow(X))[-loo]
      oob=loo
      Xbag=X[inbag,,drop=FALSE]
      Ybag=Y[inbag,,drop=FALSE]
      if (inherits(res, "MBPLS")){
        resloo=MBPLS(X=Xbag, Y=Ybag, block = res$call$size.block, name.block=res$call$name.block, ncomp=ncomp.max, scale=res$call$scale, scale.block=res$call$scale.block, scale.Y=res$call$scale.Y)
        for (h in 0:ncomp.max){
          if (h==0){
            pred.loo=colMeans(Ybag)
            MSEP=mean(sweep(Y[oob,,drop=FALSE],2,pred.loo,"-")^2)
            error.stock[loo,h+1]=MSEP
          }else{
            pred.loo=predict(resloo,X[oob,,drop=FALSE],ncomp=h)
            MSEP=mean((Y[oob,,drop=FALSE]-pred.loo)^2)
            error.stock[loo,h+1]=MSEP
          }
        }
      }else{
        resloo=MBWCov(X=Xbag, Y=Ybag, block = res$call$size.block, name.block=res$call$name.block, ncomp=ncomp.max, scale=res$call$scale, scale.block=res$call$scale.block, scale.Y=res$call$scale.Y)
        for (h in 0:ncomp.max){
          if (h==0){
            pred.loo=colMeans(Ybag)
            MSEP=mean(sweep(Y[oob,,drop=FALSE],2,pred.loo,"-")^2)
            error.stock[loo,h+1]=MSEP
          }else{
            pred.loo=predict(resloo,X[oob,,drop=FALSE],ncomp=h)
            MSEP=mean((Y[oob,,drop=FALSE]-pred.loo)^2)
            error.stock[loo,h+1]=MSEP
          }
        }
      }
      setTxtProgressBar(pb, loo)
    }
  }else{
    error.stock=matrix(0,nboot,ncomp.max+1)
    dimnames(error.stock)=list(paste("boot",1:nboot,sep="."),paste("Dim",0:ncomp.max,sep="."))
    pb <- txtProgressBar(min = 0, max = nboot, style = 3)
    for (boot in 1:nboot){
      inbag=sample(1:nrow(X),nrow(X),replace = TRUE)
      oob=(1:nrow(X))[!1:nrow(X)%in%inbag]
      Xbag=X[inbag,,drop=FALSE]
      Ybag=Y[inbag,,drop=FALSE]
      if (inherits(res, "MBPLS")){
        resboot=MBPLS(X=Xbag, Y=Ybag, block = res$call$size.block, name.block=res$call$name.block, ncomp=ncomp.max, scale=res$call$scale, scale.block=res$call$scale.block, scale.Y=res$call$scale.Y)
        for (h in 0:ncomp.max){
          if (h==0){
            pred.boot=colMeans(Ybag)
            MSEP=mean(sweep(Y[oob,,drop=FALSE],2,pred.boot,"-")^2)
            error.stock[boot,h+1]=MSEP
          }else{
            pred.boot=predict(resboot,X[oob,,drop=FALSE],ncomp=h)
            MSEP=mean((Y[oob,,drop=FALSE]-pred.boot)^2)
            error.stock[boot,h+1]=MSEP
          }
        }
      }else{
        resboot=MBWCov(X=Xbag, Y=Ybag, block = res$call$size.block, name.block=res$call$name.block, ncomp=ncomp.max, scale=res$call$scale, scale.block=res$call$scale.block, scale.Y=res$call$scale.Y)
        for (h in 0:ncomp.max){
          if (h==0){
            pred.boot=colMeans(Ybag)
            MSEP=mean(sweep(Y[oob,,drop=FALSE],2,pred.boot,"-")^2)
            error.stock[boot,h+1]=MSEP
          }else{
            pred.boot=predict(resboot,X[oob,,drop=FALSE],ncomp=h)
            MSEP=mean((Y[oob,,drop=FALSE]-pred.boot)^2)
            error.stock[boot,h+1]=MSEP
          }
        }
      }
      setTxtProgressBar(pb, boot)
    }
  }
  avg=colMeans(error.stock,na.rm=TRUE)
  if (method=="LOO"){
    std.error=sqrt(apply(error.stock,2,var,na.rm=TRUE)/sum(!is.na(error.stock[,1])))
  }else{
    std.error=sqrt(apply(error.stock,2,var,na.rm=TRUE))
  }
  if (graph){
    df.plot=data.frame(h=0:ncomp.max,MSEP=avg)
    g=ggplot(df.plot,aes(x=h,y=MSEP))+theme_bw()+ggtitle("One Sigma rule",subtitle = "Absolute Minimum")
    g = g + theme(axis.title.x = element_text(size = 5*size.graph, face = "bold"),axis.title.y = element_text(size = 5*size.graph, face = "bold"),plot.title = element_text(hjust = 0.5, face = "bold",size = 7*size.graph,color="blue"),plot.subtitle = element_text(hjust = 0.5,size = 6*size.graph,color="gray35"),axis.text = element_text(size=4*size.graph))
    g = g + xlab("Number of Components") + ylab("MSEP")
    g = g + scale_x_continuous(breaks = 0:ncomp.max)
    g=g+geom_point(size=size.graph)+geom_line(linewidth=1)
    ci.u=avg+std.error
    ci.l=avg-std.error
    g=g+ylim(min(ci.l,0),max(ci.u))
    g=g+geom_errorbar(aes(ymin=ci.l, ymax=ci.u, width=.3),linewidth=1)
    g=g+geom_vline(xintercept = which.min(avg)-1,linewidth=1,linetype="dotted",color="gray35")
    lim=ci.u[which.min(avg)]
    ou=min((0:ncomp.max)[avg<=lim])
    g=g+geom_vline(xintercept=ou,linewidth=1,linetype="dashed",color="blue")
    g=g+geom_hline(yintercept = min(avg)+std.error[which.min(avg)],linewidth=1,linetype="dotted",color="gray35")
    print(g)
  }
  retour=rbind(avg,std.error)
  rownames(retour)[1]="MSEP"
  return(retour)
}
