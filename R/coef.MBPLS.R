#' Regression Coefficients of MBPLS
#'
#' Computes regression coefficients from \code{\link{MBPLS}}.
#'
#' @param object An object resulting from \code{\link{MBPLS}}.
#' @param ncomp The number of components to be considered in the model. By default, all components computed in \code{\link{MBPLS}} or \code{\link{MBWCov}} are considered.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A matrix of regression coefficients where each row corresponds to a variable in X and each column corresponds to a variable in Y.
#'
#' @seealso \code{\link{predict.MBPLS}}
#'
#' @examples

#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, block, name.block = names(block))
#' coef(res.mbpls)
#'
#' @export

coef.MBPLS=function(object,ncomp=object$call$ncomp,...){

  if (is.numeric(ncomp) | is.integer(ncomp)){
  if (ncomp <=0 | ncomp > min(nrow(object$call$X)-1,ncol(object$call$X))){
        stop("ncomp must be larger than 0 and lower than or equal to min(nrow(object$call$X)-1,ncol(object$call$X))")
      }
  }else{
       stop("class(ncomp) must be numeric or integer")
  }



  # Beta coefficients
  beta <- object$Proj.g[,1:ncomp,drop=FALSE]%*%chol2inv(chol(crossprod(object$Scor.g[,1:ncomp,drop=FALSE])))%*%crossprod(object$Scor.g[,1:ncomp,drop=FALSE],pretreatmentY(object))
  return(beta)
}
