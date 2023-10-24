#' Multiblock Principal Components Analysis (MB-PCA)
#'
#' Performs MB-PCA on a set of quantitative blocks of variables.
#'
#' @param X  Dataset obtained by horizontally merging all the blocks of variables.
#' @param block  Vector indicating the number of variables in each block.
#' @param name.block names of the blocks of variables (NULL by default).
#' @param ncomp  Number of dimensions to compute. By default (NULL), all the global components are extracted.
#' @param scale  Logical, if TRUE (by default) then variables are scaled to unit variance (all variables are centered anyway).
#' @param scale.block  Logical, if TRUE (by default) each block of variables is divided by the square root of its inertia (Frobenius norm).
#'
#' @return Returns a list of the following elements:\cr
#' @return \item{optimalcrit}{Numeric vector of the optimal value of the criterion (sum of saliences) obtained for each dimension.}
#' @return \item{saliences}{Matrix of the specific weights of each block of variables on the global components, for each dimension.}
#' @return \item{T.g}{Matrix of normed global components.}
#' @return \item{Scor.g}{Matrix of global components (scores of individuals).}
#' @return \item{W.g}{Matrix of global weights (normed) associated with deflated X.}
#' @return \item{Load.g}{Matrix of global loadings (normed) = W.g in the specific context of MB-PCA.}
#' @return \item{Proj.g}{Matrix of global projection (to compute scores from pretreated X) = W.g in the specific context of MB-PCA.}
#' @return \item{explained.X}{Matrix of percentages of inertia explained in each block of variables.}
#' @return \item{cumexplained}{Matrix giving the percentages, and cumulative percentages, of total inertia of X blocks explained by the global components.}
#' @return \item{Block}{A list containing block components (T.b) and block weights (W.b)}
#'
#' @references
#' S. Wold, S. Hellberg, T. Lundstedt, M. Sjostrom, H. Wold (1987). Hierarchical multiblock PLS and PC models for easier model interpretation and as an alternative to variable
#' selection, in: Proc. Symp. On PLS Model Building: Theory and Application, Frankfurt am Main.\cr
#'
#' E. Tchandao Mangamana, V. Cariou, E. Vigneau, R. Glèlè Kakaï, E.M. Qannari (2019). Unsupervised multiblock data analysis: A unified approach and extensions, Chemometrics and Intelligent Laboratory Systems, 194, 103856.
#'
#' @seealso \code{\link{summary.MBPCA}}    \code{\link{plot.MBPCA}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' block=ham$block
#' res.mbpca <- MBPCA(X,block, name.block=names(block))
#' summary(res.mbpca)
#' plot(res.mbpca)
#'
#' @export

MBPCA <- function(X, block, name.block= NULL, ncomp=NULL, scale=TRUE, scale.block=TRUE){
  # ---------------------------------------------------------------------------
  # 0. Preliminary tests
  # ---------------------------------------------------------------------------

  if (any(is.na(X))){     # to be modified if missing values allowed
    stop("No NA values are allowed")
  }
  if (sum(block) != ncol(X)){
    stop("The sum of block must be equal to the total number of variables of X")
  }
  # if (!algo %in% c("eigen", "nipals"))
  #   stop("algo must be either eigen or nipals")
  if (!is.logical(scale)){
    stop("scale must be must be logical")
  }
  if (!is.logical(scale.block)){
    stop("scale.block must be logical")
  }
  if (!is.null(name.block) & (length(name.block)!=length(block))){
    stop("name.block and block must be of same length")
  }


  X  <- as.matrix(X)


  # ---------------------------------------------------------------------------
  # 1. Output preparation
  # ---------------------------------------------------------------------------

  if (is.null(rownames(X))){
    rownames(X) <- paste("Ind.", seq_len(nrow(X)), sep="") # Give the names for rows if X does not have.
  }
  if (is.null(colnames(X))){
    colnames(X) <- paste("X", seq_len(ncol(X)), sep=".")   # Give the names for columns if X does not have.
  }
  if (is.null(name.block)){
    names(block) <- paste("Block", 1:length(block), sep="") # Give the names for the blocks if it was not defined.
  }else{
    names(block) <- name.block
  }



  ntab          <- length(block);                          # Number of blocks of variables
  nind          <- nrow(X)                                 # Number of individuals
  ncolX         <- sum(block)                              # Number of columns in all the blocks of variables
  if (is.null (ncomp)){
    ncomp   <- min(nind-1, ncolX)                          # Number of global components of the analysis
  }else{
    ncomp   <- min(ncomp,min(nind-1, ncolX))
  }

  J             <- rep(1:ntab,block)                  # Will be used to identify the variables of each dataset
  optimalcrit   <- vector("numeric",length=ncomp)     # Gives the optimal value of the criterion
  names(optimalcrit) <- paste("Dim.",1:ncomp,sep="")

  #niteration    <- matrix(0,nstart,ncomp)             # Gives the number of iteration made to reach convergence for each start and each dimension
  #critnstart    <- matrix(0,nstart,ncomp)            # Values of the criterion for each start and each dimension for NIPALS algorithms
  #dimnames(critnstart) <- dimnames(niteration) <- list(paste("Iter.",1:nstart,sep=""), paste("Dim.",1:ncomp,sep=""))


  P            <- array(0,dim=c(nind,nind,ntab));          # Cross product matrices
  criterion    <- vector("numeric",length=ntab)            # Storage of the criterion to be maximized
  LAMBDA     <- matrix(0,ntab,ncomp)                       # Specific weights for each dataset and each dimension
  dimnames(LAMBDA) <- list(names(block), paste("Dim.",1:ncomp,sep=""))
  T.g          <- matrix(0,nrow=nind,ncol=ncomp)           # Global components associated with X (normed)
  dimnames(T.g)  <- list(rownames(X), paste("Dim.",1:ncomp,sep=""))
  T.b          <- array(0,dim=c(nind,ncomp,ntab))          # Block components (not normed)
  dimnames(T.b)<- list(rownames(X), paste("Dim.",1:ncomp,sep=""), names(block))


  W.b          <- vector("list",length=ntab)   # Weights for the block components
  for (j in 1:ntab) {
    W.b[[j]]   <-  matrix(0,nrow=block[j],ncol=ncomp)
    dimnames(W.b[[j]]) <-list(colnames(X[,J==j]), paste("Dim.",1:ncomp,sep=""))
  }
  wb            <- matrix(0,nrow=ncolX,ncol=ncomp)         # W.b in matrix form
  tb            <- matrix(0,nrow=nind,ncol=ntab)           # Concatenated Xb block components at current dimension comp


  W.g           <- matrix(0,nrow=ncolX,ncol=ncomp)      # Weights for the global components
  Proj.g        <- matrix(0,nrow=ncolX,ncol=ncomp)      # Modified Weights to take account of deflation
  Load.g        <- matrix(0,nrow=ncolX,ncol=ncomp)
  dimnames(W.g)<- dimnames(Proj.g)<- dimnames(Load.g) <- list(colnames(X), paste("Dim.",1:ncomp,sep=""))
  IT.X          <- vector("numeric",length=ntab+1)
  explained.X   <- matrix(0,nrow=ntab+1,ncol=ncomp)        # Percentage of explained inertia of each Xb block and global
  dimnames(explained.X)<-  list(c(names(block),'Global'), paste("Dim.",1:ncomp, sep=""))
  cumexplained  <- matrix(0,nrow=ncomp,ncol=2)
  rownames(cumexplained) <- paste("Dim.",1:ncomp,sep="")
  colnames(cumexplained) <- c("%explX", "cum%explX")



  Xscale        <- NULL
  Block         <- NULL
  call          <- NULL
  res           <- NULL


  # ---------------------------------------------------------------------------
  # 2. Required parameters and data preparation
  # ---------------------------------------------------------------------------
  Xinput        <-X
  Xscale$mean   <- apply(X, 2, mean)
  X             <-sweep(X, 2, Xscale$mean, "-")              # Default centering

  if (scale==FALSE) {                                        # No scaling
    Xscale$scale <-rep(1,times=ncol(X))
  }   else {                                                 # scaling (division by sd) for each variable
    sd.tab    <- apply(X, 2, function (x) {return(sqrt(sum(x^2)/length(x)))})   # sd based on biased variance
    temp      <- sd.tab < 1e-14
    if (any(temp)) {
      warning("Variables with null variance will not be standardized.")
      sd.tab[temp] <- 1
    }
    X         <- sweep(X, 2, sd.tab, "/")
    Xscale$scale <-sd.tab
  }

  #  Pre-processing: block weighting to set each block inertia equal to 1
  if (scale.block) {
    inertia     <- sapply(1:ntab, function(j) inertie(X[, J==j]))

    w.tab       <- rep(sqrt(inertia) , times =  block )     # Weighting parameter applied to each variable
    X           <- sweep(X, 2, w.tab, "/")
    Xscale$scale<- Xscale$scale*w.tab
  }

  IT.X[1:ntab]  <- sapply(1:ntab, function(j) {inertie(X[,J==j]) })  # Inertia of each block of variable
  IT.X[ntab+1]  <- sum(IT.X[1:ntab])

  X00=X                                                         # Keep the matrix of the blocks of variables with the preprocessing
  X  <- lapply(seq_len(ntab),function(j) {as.matrix(X[,J==j])}) # X made as a list
  X0=X                                                          # Keep the blocks of variables with the pre-treatment made as a list

  Itot             <- 0



  # ---------------------------------------------------------------------------
  # 3. computation of T.g and LAMBDA for the various dimensions
  # ---------------------------------------------------------------------------
  #pb <- txtProgressBar(min = 0, max = ncomp, style = 3)

  for (comp in 1:ncomp)  {                                 # Iterative computation of the various components

    algo="eigen"  # fixed parameter
    if  (algo=="eigen"){                     # fixed parameter in this version
      for(j in 1:ntab) {
        P[,,j]      <- tcrossprod(X[[j]])    # Computation of the cross-product matrices among individuals (or association matrices)
      }
      sumP    <- apply(P,c(1,2),sum)
      ressvd  <- svd(sumP,nu=1,nv=1)
      optimalcrit[comp]    <- ressvd$d[1]               #/nind
      T.g[,comp]   <- ressvd$u
    }                          # end if (algo=="eigen")

    # ---------------------------------------------------------------------------
    # 3.2 Storage of the results associated with dimension h
    # ---------------------------------------------------------------------------
    for(j in 1:ntab) {
      W.b[[j]][,comp] <- t(X[[j]])%*%T.g[,comp]
      T.b[,comp,j]    <- X[[j]]%*%W.b[[j]][,comp]     # tcrossprod(X[[j]])%*%T[,comp]
    }
    wb[,comp]                    <- unlist(sapply(1:ntab, function(j){W.b[[j]][,comp]}))    # Weights in matrix form

    LAMBDA[,comp]      <- matrix(sapply(seq_len(ntab),function(j){t(T.g[,comp] )%*%tcrossprod(X[[j]])%*%T.g[,comp]}),nrow=ntab,byrow=FALSE)

    W.g[,comp]   <- unlist(sapply(1:ntab,function(j){W.b[[j]][,comp]}))
    W.g[,comp]   <- normv(W.g[,comp])                        # global weights are normed


    # ---------------------------------------------------------------------------
    # 3.3 Deflation
    # ---------------------------------------------------------------------------

    X.exp                   <- lapply(X,function(Xj) {tcrossprod(T.g[,comp]) %*% Xj})
    X0.exp                  <- lapply(X0,function(Xj){tcrossprod(T.g[,comp]) %*% Xj})
    explained.X[1:ntab,comp]   <- sapply(X0.exp,function(x) {sum(x^2)})
    explained.X[ntab+1,comp]   <- sum(explained.X[1:ntab,comp])
    X                       <- lapply(seq_len(ntab),function(j) {X[[j]]-X.exp[[j]]}) # Deflation of X

    #setTxtProgressBar(pb, comp)
  }# end for (comp in 1:ncomp)

  explained.X       <- sweep(explained.X[,1:ncomp,drop=FALSE] ,1,IT.X,"/")  # Explained inertia for X
  cumexplained[,1]  <- explained.X[ntab+1,1:ncomp]
  cumexplained[,2]  <- cumsum(cumexplained[,1])

  # Unnormed global components
  if (ncomp==1) {
    Scor.g=T.g*sqrt(colSums(LAMBDA))
  }  else {
    Scor.g=T.g%*%sqrt(diag(colSums(LAMBDA)))
  }
  dimnames(Scor.g) <- dimnames(T.g)    # Unnormed global components

  # Global Loadings
  Load.g=crossprod(X00,Scor.g)
  Load.g=t(t(Load.g)/colSums(Scor.g^2))

  # Global Projection
  Proj.g <- W.g %*% solve(crossprod(Load.g,W.g),tol=1e-300)      # Weights that take into account the deflation procedure



  # ---------------------------------------------------------------------------
  # 4.1 Preparation of the results Global
  # ---------------------------------------------------------------------------

  res$optimalcrit         <- optimalcrit[1:ncomp]
  res$saliences           <- LAMBDA[,1:ncomp,drop=FALSE]
  res$T.g                 <- T.g[,1:ncomp,drop=FALSE]     # Storage of the normed global components associated with X (pre-treated)
  res$Scor.g              <- Scor.g[,1:ncomp,drop=FALSE]  # Storage of the global components (unnormed) associated with X (pre-treated)
  res$W.g                 <- W.g[,1:ncomp,drop=FALSE]     # Storage of the normed global weights associated with deflated X (pre-treated)
  res$Load.g              <- Load.g[,1:ncomp,drop=FALSE]  # Storage of the normed global loadings associated with X (pre-treated)
  res$Proj.g              <- Proj.g[,1:ncomp,drop=FALSE]  # Storage of the global projection weights associated with X (pre-treated)
  res$explained.X         <- round(100*explained.X[1:ntab,1:ncomp,drop=FALSE],2)
  res$cumexplained        <- round(100*cumexplained[1:ncomp,],2)

  # ---------------------------------------------------------------------------
  # 4.2 Preparation of the results Block
  # ---------------------------------------------------------------------------

  Block$T.b               <-  T.b[,1:ncomp,]
  Block$W.b               <-  W.b
  res$Block               <-  Block


  # ---------------------------------------------------------------------------
  # Return res
  # ---------------------------------------------------------------------------

  call$X<-Xinput
  call$size.block<-block
  call$name.block<-names(block)
  call$ncomp<-ncomp
  call$scale<-scale
  call$scale.block<-scale.block
  call$Xscale<-Xscale
  call$call <- match.call()
  res$call<-call


  class(res) <- c("MBPCA","list")

  return(res)
}

