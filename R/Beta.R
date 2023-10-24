#' Regression coefficients of MBPLS or MBWCov models
#'
#' Computes regression coefficients from \code{\link{MBPLS}} or \code{\link{MBWCov}}.
#'
#' @param res An object resulting from \code{\link{MBPLS}} or \code{\link{MBWCov}}.
#' @param ncomp The number of components to be considered in the model. By default, all components computed in \code{\link{MBPLS}} or \code{\link{MBWCov}} are considered.
#'
#' @return A matrix of regression coefficients where each row corresponds to a variable in X and each column corresponds to a variable in Y.
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
#' Beta(res.mbpls)
#'
#' # With MBWCov
#'
#' data(ham)
#' X=ham$X
#' block=ham$block
#' Y=ham$Y
#' res.mbwcov <- MBWCov(X, Y, block, name.block = names(block))
#' Beta(res.mbwcov)
#'
#' @export

Beta=function(res,ncomp=res$call$ncomp){
  if (!inherits(res, c("MBPLS","MBWCov"))){
    stop("class(res) must be MBPLS or MBWCov")
  }
  if (is.numeric(ncomp) | is.integer(ncomp)){
    if (ncomp <=0 | ncomp > min(nrow(res$call$X)-1,ncol(res$call$X))){
      stop("ncomp must be larger than 0 and lower than or equal to min(nrow(res$call$X)-1,ncol(res$call$X))")
    }
  }else{
    stop("class(ncomp) must be numeric or integer")
  }
  # Beta
  Beta <- res$Proj.g[,1:ncomp,drop=FALSE]%*%solve(crossprod(res$Scor.g[,1:ncomp,drop=FALSE]),tol=1e-300)%*%crossprod(res$Scor.g[,1:ncomp,drop=FALSE],pretreatmentY(res))
  return(Beta)
}
