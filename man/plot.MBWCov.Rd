% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.MBWCov.R
\name{plot.MBWCov}
\alias{plot.MBWCov}
\title{Default plots for \code{\link{MBWCov}} objects}
\usage{
\method{plot}{MBWCov}(x, axes = c(1, 2), size = 2.25, ...)
}
\arguments{
\item{x}{An object resulting from \code{\link{MBWCov}}.}

\item{axes}{Which dimensions should be plotted?}

\item{size}{The overall size of labels, points, etc.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The default plots.
}
\description{
Successively performs \code{\link{MBplotScores}}, \code{\link{MBplotVars}} and \code{\link{MBplotBlocks}} with the default values of parameters but axes and size.
}
\examples{
data(ham)
X=ham$X
block=ham$block
Y=ham$Y
res.mbwcov <- MBWCov(X, Y, block, name.block = names(block))
plot(res.mbwcov)

}
\seealso{
\code{\link{MBplotScores}} \code{\link{MBplotVars}} \code{\link{MBplotBlocks}}
}
