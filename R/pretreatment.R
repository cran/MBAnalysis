# Data pretreatement
# apply the centering, the variable scaling according to scale and scale.block option
# param : an object of class MBPCA, ComDim, MBPLS, MBWCov
pretreatment=function (res){
  # verif class
  if (!inherits(res, c("MBPCA","ComDim","MBPLS","MBWCov"))){
    stop("non convenient objects")
  }
  X<-res$call$X
  m<-res$call$Xscale$mean
  scal<-res$call$Xscale$scale
  Xpretr=sweep(X,2,m,"-")
  Xpretr=sweep(Xpretr,2,scal,"/")
  return(Xpretr)
}
