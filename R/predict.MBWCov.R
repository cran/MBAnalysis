#' Prediction from MBWCov models
#'
#' Computes predictions of Y from \code{\link{MBWCov}} using calibration X (default) or new X observations.
#'
#' @param object An object resulting from \code{\link{MBWCov}}.
#' @param newdata A matrix or data.frame of (new) observations having the same ncol and same colnames as the X of fitting observations.
#' @param ncomp The number of components to be considered in the model to perform the predictions. By default, all components computed in \code{\link{MBWCov}} are considered.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A matrix of predicted Y values where each row corresponds to an observation and each column corresponds to a Y variable.
#'
#' @seealso \code{\link{coef.MBWCov}}    \code{\link{MBValidation}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbwcov <- MBWCov(X, Y, block, name.block = names(block))
#' predict(res.mbwcov)
#'
#' @export

predict.MBWCov=function(object,newdata=object$call$X,ncomp=object$call$ncomp,...){

  if (!is.matrix(newdata) & !is.data.frame(newdata)){
    stop("class(newdata) must be matrix or data.frame")
  }
  if (ncol(newdata)!=ncol(object$call$X)){
    stop("ncol(newdata) must equal ncol(object$call$X)")
  }
  if (!all(colnames(newdata)==colnames(object$call$X))){
    stop("colnames(newdata) must equal colnames(object$call$X)")
  }
  if (is.numeric(ncomp) | is.integer(ncomp)){
    if (ncomp <=0 | ncomp > object$call$ncomp){
      stop("ncomp must be larger than 0 and lower than or equal to object$call$ncomp")
    }
  }else{
    stop("class(ncomp) must be numeric or integer")
  }
  newdata=as.matrix(newdata)
  predictor.pretr=sweep(newdata,2,object$call$Xscale$mean,"-")
  predictor.pretr=sweep(predictor.pretr,2,object$call$Xscale$scale,"/")
  beta=coef.MBWCov(object,ncomp)
  Ypred.pretr=predictor.pretr%*%beta
  Ypred=sweep(Ypred.pretr,2,object$call$Yscale$scale,"*")
  Ypred=sweep(Ypred,2,object$call$Yscale$mean,"+")
  return(Ypred)
}
