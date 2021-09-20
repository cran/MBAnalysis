#' Main Results for Multiblock Principal Components Analysis (MB-PCA)
#'
#' Print the main results for MB-PCA.
#'
#' @param x  An object of class \code{MBPCA}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Returns a list of the following elements:\cr
#' @return components  :  Numeric vector of length two that indicates the number of global components of the analysis and the number of global components to print.
#' @return optimalcrit  :  Numeric vector that gives the optimal value of the criterion to be maximized for each dimension.
#' @return cumexplained  :  Two columns matrix of percentages of total inertia of the blocks of variables explained by the successive global components and their cumulative values.
#' @return explained.X  :  Matrix of percentages of inertia explained in each Xb block.
#' @return contrib  :  Matrix of contribution of each Xb block to the determination of global components.
#' @return T  :  Matrix of global components (scores of individuals).
#' @return C  :  Compromise matrix (unnormed global components).
#' @return globalcor  :  Matrix of correlation coefficients between variables and global components.
#' @return cor.g.b  :  Array that gives the correlation of the global components with their respective block components.
#' @return \item{Block}{ : Results associated with each block Xb.
#'          \itemize{
#'                \item {T.b}{ : Array that contains the matrices of block components.}
#'                \item {blockcor}{ : List of matrices of correlation coefficients between the original variables of each block and the block components.}
#'        }}
#'
#'
#' @author
#' Essomanda TCHANDAO MANGAMANA \email{tchanesso@@yahoo.fr}, Véronique CARIOU, Evelyne VIGNEAU.
#'
#' @seealso \code{\link{MBPCA}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' group=ham$group
#' res.mbpca <- MBPCA(X, group, plotgraph=FALSE)
#' print(res.mbpca)
#'
#' @export
#' @importFrom grDevices dev.off pdf
#' @importFrom stats cor


# Print method for MBPCA class
print.MBPCA<-function (x, ...)
{
  Res<- x
  if (!inherits(Res, "MBPCA"))
    stop("non convenient data")
  appel   <- as.list(Res$call)
  group   <- eval.parent(appel$group)

  cat("-------------------------------------------------------------------------\n")
  cat("* Results for Multiblock Principal Components analysis                           \n")
  cat("-------------------------------------------------------------------------\n")
  cat("* The analysis was performed on", length(group),
      "blocks of", group[1:length(group)], "variables respectively          \n")
  cat("-------------------------------------------------------------------------\n")
  cat( "* The number of individuals in each block is",nrow(Res$C),"                                     \n")
  cat("-------------------------------------------------------------------------\n")
  cat("* Results are available in the following objects:                                    \n")
  #cat("------------------------------------------------------------------------\n")
  cat("-------------------------------------------------------------------------\n")
  Res <- array("", c(11, 5), list(1:11, c("|","NAME OF OBJECT", "|", "DESCRIPTION OF OBJECT", "|")))
  cat("-------------------------------------------------------------------------\n")
  Res[1, ] <- c("|", "$components", "|", "Number of global components in total and printed", "|")
  Res[2, ] <- c("|", "$optimalcrit", "|", "Values of the criterion to be maximized", "|")
  Res[3, ] <- c("|", "$cumexplained", "|", "% of inertia explained in all blocks and cumul.", "|")
  Res[4, ] <- c("|", "$explained.X","|", "% of inertia explained in each block", "|")
  Res[5, ] <- c("|", "$contrib", "|", "Contributions of blocks to global components", "|")
  Res[6, ] <- c("|", "$T","|", "Global components", "|")
  Res[7, ] <- c("|", "$C","|", "Compromise matrix", "|")
  Res[8, ] <- c("|", "$globalcor","|", "Correlations variables-global components", "|")
  Res[9, ] <- c("|", "$cor.g.b","|", "Correlations global components-block components", "|")
  Res[10, ] <- c("|", "$Block$T.b","|","Block components", "|")
  Res[11, ] <- c("|", "$Block$blockcor","|", "Correlations variables-block components", "|")
  print(Res[1:11, ],quote=FALSE)
  cat("-------------------------------------------------------------------------\n\n")
}
