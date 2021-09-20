#' Main Results for Multiblock Weighted Covariate analysis (MB-WCov)
#'
#' Print the main results for MB-WCov.
#'
#' @param x  An object of class \code{MBWCov}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Returns a list of the following elements:\cr
#' @return components  :  Numeric vector of length two that gives the number of global components of the analysis and the number of global components to print.
#' @return optimalcrit  :  Numeric vector that gives the optimal value of the criterion to be maximized for each dimension.
#' @return cumexplained   :  Four columns matrix of percentages of total inertia of the explanatory blocks, percentages of inertia of the response block
#' explained by the successive global components and their cumulative values.
#' @return explained.X  :  Matrix of percentages of inertia explained for each Xb block.
#' @return explained.Y  :  Matrix of percentages of inertia explained for each Y variable.
#' @return saliences  :  Matrix containing the specific weights of each explanatory block of variables on global components.
#' @return contrib  :  Matrix of contribution of each Xb block to the determination of global components.
#' @return T  :  Matrix of global components (scores of individuals).
#' @return C  :  Compromise matrix (unnormed global components).
#' @return U  :  Matrix of components associated with the response block of variables.
#' @return globalcor  :  Matrix of correlation coefficients between the original variables and the global components.
#' @return cor.g.b  :  Array that gives the correlation of the global components with their respective block components.
#' @return betaY  :  Array of regression coefficients.
#' @return \item{Block}{ : Results associated with each block of variables.
#'          \itemize{
#'                \item {T.b}{ : Array that contains the matrices of block components.}
#'                \item {blockcor}{ : List of matrices of correlation coefficients between the original variables of each block of variables and the block components.}
#'        }}
#'
#'
#' @author
#' Essomanda TCHANDAO MANGAMANA \email{tchanesso@@yahoo.fr}, Véronique CARIOU, Evelyne VIGNEAU.
#'
#' @seealso \code{\link{MBWCov}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' group=ham$group
#' Y=ham$Y
#' res.mbwcov <- MBWCov(X, Y, group)
#' print(res.mbwcov)
#'
#' @export
#' @importFrom grDevices dev.off pdf
#' @importFrom stats cor
#' @importFrom graphics abline arrows axis barplot legend lines par plot text

# Print method for MBWCov class
print.MBWCov<-function (x, ...)
{
  Res<- x
  if (!inherits(Res, "MBWCov"))
    stop("non convenient data")
  appel   <- as.list(Res$call)
  group   <- eval.parent(appel$group)

  cat("-------------------------------------------------------------------------\n")
  cat("* Results for Multiblock Weighted Covariate analysis                                    \n")
  cat("-------------------------------------------------------------------------\n")
  cat("* The Y block of", (nrow(Res$explained.Y)-1), "variables is predicted from ", length(group),
      "blocks of", group[1:length(group)], "variables        \n")
  cat("-------------------------------------------------------------------------\n")
  cat( "* The number of individuals in each block is",nrow(Res$C),"                                            \n")
  cat("-------------------------------------------------------------------------\n")
  cat("* Results are available in the following objects:                                           \n")
  #cat("------------------------------------------------------------------------\n")
  cat("-------------------------------------------------------------------------\n")
  Res <- array("", c(15, 5), list(1:15, c("|","NAME OF OBJECT", "|", "DESCRIPTION OF OBJECT", "|")))
  cat("-------------------------------------------------------------------------\n")
  Res[1, ] <- c("|", "$components", "|", "Number of global components in total and printed", "|")
  Res[2, ] <- c("|", "$optimalcrit", "|", "Values of the criterion to be maximized", "|")
  Res[3, ] <- c("|", "$cumexplained", "|", "% of inertia explained in all Xb, Y and cumul.", "|")
  Res[4, ] <- c("|", "$explained.X","|", "% of inertia explained in each Xb", "|")
  Res[5, ] <- c("|", "$explained.Y","|", "% of inertia explained in Y", "|")
  Res[6, ] <- c("|", "$saliences","|", "Specific weights of blocks of variables", "|")
  Res[7, ] <- c("|", "$contrib","|", "Contribution of blocks to the global components", "|")
  Res[8, ] <- c("|", "$T","|", "Global components", "|")
  Res[9, ] <- c("|", "$C","|", "Compromise matrix", "|")
  Res[10, ] <- c("|", "$U","|", "Components associated with Y", "|")
  Res[11, ] <- c("|", "$globalcor","|", "Correlations variables-global components", "|")
  Res[12, ] <- c("|", "$cor.g.b","|", "Correlations global components-block components", "|")
  Res[13, ] <- c("|", "$betaY","|", "Regression coefficients", "|")
  Res[14, ] <- c("|", "$Block$T.b","|","Block components", "|")
  Res[15, ] <- c("|", "$Block$blockcor","|", "Correlations variables-block components", "|")
  print(Res[1:15, ],quote=FALSE)
  cat("-------------------------------------------------------------------------\n\n")
}
