% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.MBPLS.R
\name{summary.MBPLS}
\alias{summary.MBPLS}
\title{Summary of \code{\link{MBPLS}} objects}
\usage{
\method{summary}{MBPLS}(object, ...)
}
\arguments{
\item{object}{An object resulting from \code{\link{MBPLS}}.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The summary.
}
\description{
Edits the Cumulative Explained Variance, Block Explained Variance per Dimension and Block Saliences per Dimension of a \code{\link{MBPLS}} object.
}
\examples{
data(ham)
X=ham$X
block=ham$block
Y=ham$Y
res.mbpls <- MBPLS(X, Y, block, name.block = names(block))
summary(res.mbpls)

}
\seealso{
\code{\link{plot.MBPLS}}
}
