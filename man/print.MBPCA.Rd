% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.MBPCA.R
\name{print.MBPCA}
\alias{print.MBPCA}
\title{Main Results for Multiblock Principal Components Analysis (MB-PCA)}
\usage{
\method{print}{MBPCA}(x, ...)
}
\arguments{
\item{x}{An object of class \code{MBPCA}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
Returns a list of the following elements:\cr

components  :  Numeric vector of length two that indicates the number of global components of the analysis and the number of global components to print.

optimalcrit  :  Numeric vector that gives the optimal value of the criterion to be maximized for each dimension.

cumexplained  :  Two columns matrix of percentages of total inertia of the blocks of variables explained by the successive global components and their cumulative values.

explained.X  :  Matrix of percentages of inertia explained in each Xb block.

contrib  :  Matrix of contribution of each Xb block to the determination of global components.

T  :  Matrix of global components (scores of individuals).

C  :  Compromise matrix (unnormed global components).

globalcor  :  Matrix of correlation coefficients between variables and global components.

cor.g.b  :  Array that gives the correlation of the global components with their respective block components.

\item{Block}{ : Results associated with each block Xb.
         \itemize{
               \item {T.b}{ : Array that contains the matrices of block components.}
               \item {blockcor}{ : List of matrices of correlation coefficients between the original variables of each block and the block components.}
       }}
}
\description{
Print the main results for MB-PCA.
}
\examples{
data(ham)
X=ham$X
group=ham$group
res.mbpca <- MBPCA(X, group, plotgraph=FALSE)
print(res.mbpca)

}
\seealso{
\code{\link{MBPCA}}
}
\author{
Essomanda TCHANDAO MANGAMANA \email{tchanesso@yahoo.fr}, Véronique CARIOU, Evelyne VIGNEAU.
}
