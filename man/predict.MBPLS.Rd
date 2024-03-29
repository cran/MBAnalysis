% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.MBPLS.R
\name{predict.MBPLS}
\alias{predict.MBPLS}
\title{Prediction from MBPLS models}
\usage{
\method{predict}{MBPLS}(object, newdata = object$call$X, ncomp = object$call$ncomp, ...)
}
\arguments{
\item{object}{An object resulting from \code{\link{MBPLS}}.}

\item{newdata}{A matrix or data.frame of (new) observations having the same ncol and same colnames as the X of fitting observations.}

\item{ncomp}{The number of components to be considered in the model to perform the predictions. By default, all components computed in \code{\link{MBPLS}} are considered.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
A matrix of predicted Y values where each row corresponds to an observation and each column corresponds to a Y variable.
}
\description{
Computes predictions of Y from \code{\link{MBPLS}} using calibration X (default) or new X observations.
}
\examples{
data(ham)
X=ham$X
block=ham$block
Y=ham$Y
res.mbpls <- MBPLS(X, Y, block, name.block = names(block))
predict(res.mbpls)

}
\seealso{
\code{\link{Beta}}    \code{\link{MBValidation}}
}
