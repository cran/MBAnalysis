#' Summary Results for Common Dimensions analysis (ComDim)
#'
#' Gives key results for ComDim.
#'
#' @param  object  An object of class \code{ComDim}.
#' @param  nvar  Number of variables to print. By default (NULL), all the variables are printed.
#' @param  ncompprint  Number of global components to print. By default (NULL), the number of global components printed for the main function ComDim.
#' @param  digits Number of decimal points (by default 2).
#' @param ... Further arguments.
#'
#' @return Returns the percentages of inertia explained by successive global components, their cumulative values, the saliences and the correlations of the original variables with the global components.
#'
#' @author
#' Essomanda TCHANDAO MANGAMANA \email{tchanesso@@yahoo.fr}, Véronique CARIOU, Evelyne VIGNEAU.
#'
#' @seealso \code{\link{ComDim}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' group=ham$group
#' res.comdim <- ComDim(X, group, plotgraph=FALSE)
#' summary(res.comdim)
#' @export


# Summary method for ComDim class
summary.ComDim<-function (object, nvar=NULL, ncompprint = NULL, digits=2,...){
  Res<-object
  if (!inherits(Res, "ComDim"))
    stop("non convenient data")

  if (is.null(nvar))  nvar=nrow(Res$globalcor)
  if (is.null(ncompprint)) ncompprint=Res$components[2]
  if (ncompprint > Res$components[2])  stop(cat("\nncompprint should be less or equal to", Res$components[2],"\n\n"))
  eig=cbind("|",round(Res$cumexplained[1:ncompprint,1], digits),"|",round(Res$cumexplained[1:ncompprint,2], digits),"|")
  colnames(eig) <-c("|", "% of inertia","|","Cumul % of inertia","|")
  cat("\n Percentages of inertia explained in X blocks for the first", ncompprint,"dimensions and their cumulative values. \n\n")
  print(eig,quote=FALSE)
  saliences=cbind("|",round(Res$saliences[,1:ncompprint], digits),"|")
  colnames(saliences) <-c("|", paste("Dim.", 1:ncompprint, sep=""),"|")
  cat("\n Specific weight of X blocks of variables on the first", ncompprint,"dimensions. \n\n")
  print(saliences,quote=FALSE)
  variables=cbind("|",round(Res$globalcor[1:nvar,1:ncompprint], digits),"|")
  colnames(variables) <-c("|", paste("Dim.", 1:ncompprint, sep=""),"|")
  cat("\n Correlation of the first", nvar, "variables with the first", ncompprint,"dimensions. \n\n")
  print(variables,quote=FALSE)
}
