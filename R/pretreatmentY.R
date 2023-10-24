# Data pretreatement of Y
# apply the centering, the variable scaling according to scaleY option
# param : an object of class MBPLS, MBWCov
pretreatmentY=function (res){
  # verif class
  if (!inherits(res, c("MBPLS","MBWCov"))){
    stop("non convenient objects")
  }
  Y<-res$call$Y
  m<-res$call$Yscale$mean
  scal<-res$call$Yscale$scale
  Ypretr=sweep(Y,2,m,"-")
  Ypretr=sweep(Ypretr,2,scal,"/")
  return(Ypretr)
}
