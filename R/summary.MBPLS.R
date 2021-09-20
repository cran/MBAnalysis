#' Summary Results for Multiblock Partial Least Squares (MB-PLS) regression
#'
#' Gives key results for MB-PLS.
#'
#' @param  object  An object of class \code{MBPLS}.
#' @param  nvar  Number of variables to print. By default (NULL), all the variables are printed.
#' @param  ncompprint  Number of global components to print.
#' By default (NULL), the number of global components printed for the main function MBPLS.
#' @param  digits Number of decimal points (by default 2).
#' @param ... Further arguments.
#'
#' @return Returns the percentages of inertia explained by successive global components (for both X and Y), their cumulative values
#' and the correlations of the variables with the global components.
#'
#' @author
#' Essomanda TCHANDAO MANGAMANA \email{tchanesso@@yahoo.fr}, Véronique CARIOU, Evelyne VIGNEAU.
#'
#' @seealso \code{\link{MBPLS}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' group=ham$group
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, group, plotgraph=FALSE)
#' summary(res.mbpls)
#' @export


# Summary method for MBPLS class
summary.MBPLS<-function (object, nvar=NULL, ncompprint = NULL, digits=2,...){
  Res<-object
  if (!inherits(Res, "MBPLS"))
    stop("non convenient data")

  if (is.null(nvar))  nvar=nrow(Res$globalcor)
  if (is.null(ncompprint)) ncompprint=Res$components[2]
  if (ncompprint > Res$components[2])  stop(cat("\nncompprint should be less or equal to", Res$components[2],"\n\n"))
  eig=cbind("|",round(Res$cumexplained[1:ncompprint,1], digits),"|",round(Res$cumexplained[1:ncompprint,2], digits),"|",round(Res$cumexplained[1:ncompprint,3], digits),"|",round(Res$cumexplained[1:ncompprint,4], digits),"|")
  colnames(eig) <-c("|", "% of inertia in X","|","Cumul % of inertia in X","|", "% of inertia in Y","|","Cumul % of inertia in Y","|")
  cat("\n Percentages of inertia explained in X blocks and Y block for the first", ncompprint,"dimensions and their cumulative values. \n\n")
  print(eig,quote=FALSE)
  variables=cbind("|",round(Res$globalcor[1:nvar,1:ncompprint], digits),"|")
  colnames(variables) <-c("|", paste("Dim.", 1:ncompprint, sep=""),"|")
  cat("\n Correlation of the first", nvar, "variables of X blocks with the first", ncompprint,"dimensions. \n\n")
  print(variables,quote=FALSE)
}