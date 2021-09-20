#' Multiblock Principal Components Analysis (MB-PCA)
#'
#' Performs MB-PCA on a set of quantitative blocks of variables.
#'
#' @param X  Block obtained by horizontally merging all the blocks of variables.
#' @param group  Vector which indicates the number of variables in each block.
#' @param algo  Type of algorithm to use. Either "eigen" (default) or "nipals".
#' @param ncompprint  Number of global components to print.
#' By default (NULL), all the global components of the analysis are printed.
#' @param scale  Type of standardization applied to the variables. Either "none" (default) or "sd".
#' If scale="sd", each variable is divided by its standard deviation.
#' @param option  Type of normalization applied to each block of variables (either "none" or "uniform").
#' If option="uniform" (default), each block of variables is divided by its Frobenius norm.
#' @param nstart  Number of random initializations of the global component in case of nipals algorithm (by default 10).
#' @param threshold  Value used to break the iterative loop (by default 1e-8).
#' @param plotgraph  Boolean (TRUE/FALSE). If TRUE (default), graphs depicting scores of individuals, correlations of variables with the global components and contributions of blocks of variables to the determination of global components are displayed.
#' @param axes  Vector of length two which specifies the global components to plot (by default the first two).
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
#' @author
#' Essomanda TCHANDAO MANGAMANA \email{tchanesso@@yahoo.fr}, Véronique CARIOU, Evelyne VIGNEAU.
#'
#' @references
#' S. Wold, S. Hellberg, T. Lundstedt, M. Sjostrom, H. Wold (1987). Hierarchical multiblock PLS and PC models for easier model interpretation and as an alternative to variable
#' selection, in: Proc. Symp. On PLS Model Building: Theory and Application, Frankfurt am Main.\cr
#'
#' E. Tchandao Mangamana, V. Cariou, E. Vigneau, R. Glèlè Kakaï, E.M. Qannari (2019). Unsupervised multiblock data analysis: A unified approach and extensions, Chemometrics and Intelligent Laboratory Systems, 194, 103856.
#'
#' @seealso \code{\link{print.MBPCA}}    \code{\link{plot.MBPCA}}    \code{\link{summary.MBPCA}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' group=ham$group
#' res.mbpca <- MBPCA(X, group)
#' res.mbpca
#'
#' @export
#' @importFrom stats cor rnorm


MBPCA <- function(X, group, algo="eigen", ncompprint=NULL, scale="none", option="uniform", nstart=10, threshold=1e-8, plotgraph=TRUE, axes=c(1,2)){
  # ---------------------------------------------------------------------------
  # 0. Preliminary tests
  # ---------------------------------------------------------------------------

  if (any(is.na(X)))
    stop("No NA values are allowed")
  if (sum(group) != ncol(X))
    stop("The sum of group must be equal to the total number of variables of in all the blocks")
  if (!algo %in% c("eigen", "nipals"))
    stop("algo must be either eigen or nipals")
  if (is.character(scale)) {
    if (!scale %in% c("none","sd"))
      stop("scale must be either none or sd")
  }
  else {
    if (!is.numeric(scale) | length(scale)!=ncol(X))
      stop("Non convenient scaling parameter")
  }
  if (!option %in% c("none","uniform"))
    stop("option must be either none or uniform")
  X             <- as.matrix(X)

  # ---------------------------------------------------------------------------
  # 1. Output preparation
  # ---------------------------------------------------------------------------

  if (is.null(rownames(X)))
    rownames(X) <- paste("Ind.", seq_len(nrow(X)), sep="") # Give the names for rows if X does not have.
  if (is.null(colnames(X)))
    colnames(X) <- paste("X", seq_len(ncol(X)), sep=".")   # Give the names for columns if X does not have.
  if (is.null(names(group)))
    names(group) <- paste("Block", 1:length(group), sep=" ") # Give the names for the groups if it was not defined.


  ntab          <- length(group);    # Number of blocks of variables
  nind          <- nrow(X)           # Number of individuals
  ncolX         <- sum(group)        # Number of columns in all the blocks of variables
  ncomp         <- min(nind-1, ncolX)# Number of global components of the analysis
  if (is.null(ncompprint)) ncompprint=ncomp
  if (ncompprint > ncomp)  stop(cat("\n ncompprint should be less or equal to", ncomp,".\n"))
  J             <- rep(1:ntab,group) # Will be used to identify the variables of each dataset
  critnstart    <- matrix(0,nstart,ncomp)            # Values of the criterion for each start and each dimension for NIPALS algorithms
  niteration    <- matrix(0,nstart,ncomp)            # Gives the number of iteration made to reach convergence for each start and each dimension
  dimnames(critnstart) <- dimnames(niteration) <- list(paste("Iter.",1:nstart,sep=""), paste("Dim.",1:ncomp,sep=""))
  optimalcrit   <- vector("numeric",length=ncomp)    # Gives the optimal value of the criterion
  names(optimalcrit) <- paste("Dim.",1:ncomp,sep="")

  contrib           <- matrix(0,ntab,ncomp)
  dimnames(contrib) <- list(names(group), paste("Dim.",1:ncomp,sep=""))

  P            <- array(0,dim=c(nind,nind,ntab));   # Cross product matrices
  Trace        <- vector("numeric",length=ntab)      # Trace of (XbXb')
  criterion    <- vector("numeric",length=ntab)      # Storage of the criterion to be maximized
  NNLAMBDA     <- matrix(0,ntab,ncomp)   # Specific weights for each dataset and each dimension
  dimnames(NNLAMBDA) <- list(names(group), paste("Dim.",1:ncomp,sep=""))
  T            <- matrix(0,nrow=nind,ncol=ncomp)       # Global components associated with X
  dimnames(T)  <- list(rownames(X), paste("Dim.",1:ncomp,sep=""))
  T.b          <- array(0,dim=c(nind,ncomp,ntab))      # Block components
  dimnames(T.b)<- list(rownames(X), paste("Dim.",1:ncomp,sep=""), names(group))

  cor.g.b    <- array(0,dim=c(ncomp,ncomp,ntab))  # Correlations of global components with their respective block components
  dimnames(cor.g.b) <- list(paste("Dim.",1:ncomp,sep=""), paste("Dim.",1:ncomp,sep=""), names(group))

  W.b       <- vector("list",length=ntab)   # Weights for the block components
  blockcor  <- vector("list",length=ntab)   # Correlation coefficient between the original variables of each block and its block components

  for (j in 1:ntab) {
    W.b[[j]]  <- blockcor[[j]]  <- matrix(0,nrow=group[j],ncol=ncomp)
    dimnames(W.b[[j]]) <- dimnames(blockcor[[j]]) <- list(colnames(X[,J==j]), paste("Dim.",1:ncomp,sep=""))
  }

  Px            <- matrix(0,nrow=ncolX,ncol=ncomp)      # Loadings for the global components
  W             <- matrix(0,nrow=ncolX,ncol=ncomp)      # Weights for the global components
  Wm            <- matrix(0,nrow=ncolX,ncol=ncomp)      # Modified Weights to take account of deflation
  dimnames(Px)  <- dimnames(W) <- dimnames(Wm) <- list(colnames(X), paste("Dim.",1:ncomp,sep=""))
  IT.X          <- vector("numeric",length=ntab+1)
  explained.X   <- matrix(0,nrow=ntab+1,ncol=ncomp)      # Percentage of explained inertia of each Xb block and global
  dimnames(explained.X)<-  list(c(names(group),'Global'), paste("Dim.",1:ncomp, sep=""))
  cumexplained  <- matrix(0,nrow=ncomp,ncol=2)
  rownames(cumexplained) <- paste("Dim.",1:ncomp,sep="")
  colnames(cumexplained) <- c("%explX", "cum%explX")
  tb            <- matrix(0,nrow=nind,ncol=ntab)     # Concatenated Xb block components at current dimension comp
  components    <- vector("numeric",length=2)

  Xscale        <- NULL
  Block         <- NULL
  res           <- NULL


  # ---------------------------------------------------------------------------
  # 2. Required parameters and data preparation
  # ---------------------------------------------------------------------------

  Xscale$mean   <- apply(X, 2, mean)
  X             <-scale(X,center=Xscale$mean, scale=FALSE)   # Default centering

  if (scale=="none") {
    Xscale$scale <-rep(1,times=ncol(X))
  }   else {
    if (scale=="sd") {
      sd.tab    <- apply(X, 2, function (x) {return(sqrt(sum(x^2)/length(x)))})   # sd based on biased variance
      temp      <- sd.tab < 1e-14
      if (any(temp)) {
        warning("Variables with null variance not standardized.")
        sd.tab[temp] <- 1
      }
      X         <- sweep(X, 2, sd.tab, "/")
      Xscale$scale <-sd.tab
    }   else {     # Specific scaling depending on blocks defined as a vector with a scaling parameter for each variable
      X         <- sweep(X, 2, scale, "/")
      Xscale$scale <-scale
    }
  }

  #  Pre-processing: block weighting to set each block inertia equal to 1
  if (option=="uniform") {
    inertia     <- sapply(1:ntab, function(j) inertie(X[, J==j]))

    w.tab       <- rep(sqrt(inertia) , times =  group )     # Weighting parameter applied to each variable
    X           <- sweep(X, 2, w.tab, "/")
    Xscale$scale<- Xscale$scale*w.tab
  }

  IT.X[1:ntab]     <- sapply(1:ntab, function(j) {inertie(X[,J==j]) })  # Inertia of each block of variable
  IT.X[ntab+1]     <- sum(IT.X[1:ntab])

  X00=X           # Keep the blocks of variables with the pre-treatment made (as a matrix)

  # Computation of the cross-product matrices among individuals (or association matrices)
  X                <- lapply(seq_len(ntab),function(j) {as.matrix(X[,J==j])}) #mod1

  Trace            <- sapply(seq_len(ntab), function(j) {sum(diag(tcrossprod(X[[j]])))})

  X0=X            # Keep the blocks of variables with the pre-treatment made (as a list).

  Itot             <- 0

  # ---------------------------------------------------------------------------
  # 3. computation of Q and LAMBDA for the various dimensions
  # ---------------------------------------------------------------------------

  for (comp in 1:ncomp)  {                           # Iterative computation of the various components

    if  (algo=="eigen"){
      for(j in 1:ntab) {
        P[,,j]      <- tcrossprod(X[[j]])
      }
    sumP    <- apply(P,c(1,2),sum)
    reseig  <- eigen(sumP)
    t       <- reseig$vectors[,1]
    optimalcrit[comp]    <- reseig$values[1]#/nind
    T[,comp]   <- t

  } else { # end if  (algo=="eigen") and start if  (algo=="nipals")

    critt.opt         <- 0                    # several starts

    for (i in seq_len(nstart)) {

      # Initialisations

      index       <- sample(1:ncolX,1)               # Choose randomly a number
      t           <- X00[,index]                     # Choosing a column of X as initialization of t and normalization (norm of t=1)
      t          <- normv(abs(t))


      critt       <- 0
      deltacrit   <- 1
      iNIPALS     <- 0                        # Find the number of iterations made to reach convergence

      # ---------------------------------------------------------------------------
      # 3.1 NIPALS loop until convergence
      # ---------------------------------------------------------------------------

      while(deltacrit>threshold)  {
        tb          <- matrix(unlist(lapply(seq_len(ntab),function(j){tcrossprod(X[[j]])%*%t})),nrow=nind,byrow=FALSE) # Block component
        t           <- rowSums(tb)
        t           <- normv(t)
        criterion   <- sum(sapply(seq_len(ntab),function(j){t(t)%*%tb[,j]}))
        deltacrit   <- criterion- critt
        critt       <- criterion
        iNIPALS     <- iNIPALS + 1
      } # end while

      if (critt > critt.opt) {      # To be used in case of several random initializations of t
        T[,comp]      <- t
        critt.opt  <- critt
      }

      critnstart[i,comp]  <- critt
      niteration[i,comp]  <- iNIPALS

    }   # end for (i in seq_len(nstart))

    optimalcrit[comp] <- max(critnstart[,comp])
  } # end if (algo==nipals)

    # ---------------------------------------------------------------------------
    # 3.2 Storage of the results associated with dimension h
    # ---------------------------------------------------------------------------

    for(j in 1:ntab) {
      W.b[[j]][,comp] <- t(X[[j]])%*%T[,comp]
      T.b[,comp,j]    <- X[[j]]%*%W.b[[j]][,comp]     # tcrossprod(X[[j]])%*%T[,comp]
    }

    NNLAMBDA[,comp]      <- matrix(sapply(seq_len(ntab),function(j){t(T[,comp] )%*%T.b[,comp,j]}),nrow=ntab,byrow=FALSE)

    W[,comp]   <- unlist(sapply(1:ntab,function(j){W.b[[j]][,comp]}))
    W[,comp]   <- normv(W[,comp])

    Px[,comp]                    <- unlist(sapply(1:ntab, function(j){W.b[[j]][,comp]}))    # Loadings


    # ---------------------------------------------------------------------------
    # 3.3 Deflation
    # ---------------------------------------------------------------------------

    X.exp                   <- lapply(X,function(Xj) {tcrossprod(T[,comp]) %*% Xj})                                                                             # ESSO
    X0.exp                  <- lapply(X0,function(Xj){tcrossprod(T[,comp]) %*% Xj})
    explained.X[1:ntab,comp]   <- sapply(X0.exp,function(x) {sum(x^2)})
    explained.X[ntab+1,comp]   <- sum(explained.X[1:ntab,comp])
    X                       <- lapply(seq_len(ntab),function(j) {X[[j]]-X.exp[[j]]}) # Deflation of X
  }# end for (comp in 1:ncomp)

  explained.X       <- sweep(explained.X[,1:ncomp,drop=FALSE] ,1,IT.X,"/")  # Explained inertia for X
  cumexplained[,1]  <- explained.X[ntab+1,1:ncomp]
  cumexplained[,2]  <- cumsum(cumexplained[,1])

  contrib           <- t(t(NNLAMBDA)/colSums(NNLAMBDA))

  Wm      <- W %*% solve(t(Px)%*%W,tol=1e-150)      # Weights that take into account the deflation procedure


  # Unnormed global components
  if (ncomp==1) {
    LambdaMoyen<-apply(NNLAMBDA,2,sum)
    C=T*LambdaMoyen
  }
  else {
    LambdaMoyen<-apply(NNLAMBDA,2,sum)
    C=T%*%sqrt(diag(LambdaMoyen))
  }

  dimnames(C) <- dimnames(T)    # Unnormed global components

  globalcor      <- cor(X00, C)


  for(j in 1:ntab) {
    cor.g.b[,,j]    <- cor(T, T.b[,,j])
    blockcor[[j]]   <- cor(X0[[j]],T.b[,1:ncompprint,j])
    if (is.null(rownames(blockcor[[j]]))) rownames(blockcor[[j]]) <- names(group[j])
  }

  # ---------------------------------------------------------------------------
  # 4.1 Preparation of the results Global
  # ---------------------------------------------------------------------------

  res$components          <- c(ncomp=ncomp, ncompprint=ncompprint)
  res$optimalcrit         <- optimalcrit[1:ncompprint]
  res$T                   <- T[,1:ncompprint,drop=FALSE]     # Storage of the normed global components associated with X
  res$C                   <- C[,1:ncompprint,drop=FALSE]     # Storage of the unnormed global components associated with X
  res$explained.X         <- round(100*explained.X[1:ntab,1:ncompprint],2)
  res$cumexplained        <- round(100*cumexplained[1:ncompprint,],2)
  res$contrib             <- round(100*contrib[1:ntab,1:ncompprint],2)
  res$globalcor            <- globalcor[,1:ncompprint]
  res$cor.g.b             <- cor.g.b[1:ncompprint,1:ncompprint,]

  # ---------------------------------------------------------------------------
  # 4.2 Preparation of the results Block
  # ---------------------------------------------------------------------------

  Block$T.b         <-  T.b[,1:ncompprint,]
  Block$blockcor    <-  blockcor
  res$Block         <-  Block


  # ---------------------------------------------------------------------------
  # Return res
  # ---------------------------------------------------------------------------

  res$Xscale <- Xscale
  res$call   <- match.call()

  class(res) <- c("MBPCA","list")

  if (plotgraph) {
    plot.MBPCA(res,graphtype="globalscores", axes=axes)
    plot.MBPCA(res,graphtype="globalcor", axes=axes)
    plot.MBPCA(res,graphtype="contrib", axes=axes)
  }
  return(res)
}

