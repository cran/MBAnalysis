#' Summary Results for Multiblock Principal Components Analysis (MB-PCA)
#'
#' Gives key results for MB-PCA.
#'
#' @param  object  An object of class \code{MBPCA}.
#' @param  nvar  Number of variables to print. By default (NULL), all the variables are printed.
#' @param  ncompprint  Number of global components to print. By default (NULL), the number of global components printed for the main function MBPCA.
#' @param  digits Number of decimal points (by default 2).
#' @param ... Further arguments.
#'
#' @return Returns the percentages of inertia explained by successive global components, their cumulative values
#' and the correlations of the original variables with the global components.
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
#' summary(res.mbpca)
#' @export


# Summary method for MBPCA class
summary.MBPCA<-function (object, nvar=NULL, ncompprint = NULL, digits=2,...){
  Res<-object
  if (!inherits(Res, "MBPCA"))
    stop("non convenient data")

  appel   <- as.list(Res$call)
  group   <- eval.parent(appel$group)
  ntab <- length(group)

  if (is.null(nvar))  nvar=nrow(Res$globalcor)
  if (is.null(ncompprint)) ncompprint=Res$components[2]
  if (ncompprint > Res$components[2])  stop(cat("\nncompprint should be less or equal to", Res$components[2],"\n\n"))
  eig=cbind("|",round(Res$cumexplained[1:ncompprint,1], digits),"|",round(Res$cumexplained[1:ncompprint,2], digits),"|")
  colnames(eig) <-c("|", "% of inertia","|","Cumul % of inertia","|")
  cat("\n Percentages of inertia explained in X blocks for the first", ncompprint,"dimensions and their cumulative values. \n\n")
  print(eig,quote=FALSE)

  variables=cbind("|",round(Res$globalcor[1:nvar,1:ncompprint], digits),"|")
  colnames(variables) <-c("|", paste("Dim.", 1:ncompprint, sep=""),"|")
  cat("\n Correlation of the first", nvar, "variables of X blocks with the first", ncompprint,"dimensions. \n\n")
  print(variables,quote=FALSE)

}
