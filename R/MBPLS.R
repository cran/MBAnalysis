#' Multiblock Partial Least Squares (MB-PLS) regression
#'
#' MB-PLS regression applied to a set of quantitative blocks of variables.
#'
#' @param X  Block obtained by horizontally merging all the explanatory blocks of variables.
#' @param Y  Response block of variables.
#' @param group  Vector indicating the number of variables in each explanatory block.
#' @param algo  Type of algorithm to use. Either "eigen" (default) or "nipals".
#' @param ncompprint  Number of global components to print.
#' By default (NULL), all the global components of the analysis are printed.
#' @param scale  Type of standardization applied to the variables in the explanatory blocks. Either "none" (default) or "sd".
#' If scale="sd", each variable in the explanatory blocks is divided by its standard deviation.
#' @param scaleY  Type of standardization applied to the variables in the response block. Either "none" (default) or "sd".
#' If scaleY="sd", each variable in the response block is divided by its standard deviation.
#' @param option  Type of normalization applied to each explanatory block of variables (either "none" or "uniform").
#' If option="uniform" (default), each explanatory block of variables is divided by its Frobenius norm.
#' @param optionY  Type of normalization applied to the response block of variables (either "none" or "uniform").
#' If optionY="uniform" (default), the response block of variables is divided by its Frobenius norm.
#' @param nstart  Number of random initializations of the vector of Y loadings in case of nipals algorithm (by default 10).
#' @param threshold  Value used to break the iterative loop (by default 1e-8).
#' @param plotgraph  Boolean (TRUE/FALSE). If TRUE (default), graphs depicting scores of individuals, correlations of variables with the global components and contributions of blocks of variables to the determination of global components are displayed.
#' @param axes  Vector that indicates the plane in which graphs should be depicted (by default the plane formed by the first two global components).
#'
#' @return Returns a list of the following elements:\cr
#' @return components  :  Numeric vector of length two that gives the number of global components of the analysis and the number of global components to print.
#' @return optimalcrit  :  Numeric vector that gives the optimal value of the criterion to be maximized for each dimension.
#' @return cumexplained   :  Four columns matrix of percentages of total inertia of the explanatory blocks, percentages of inertia of the response block
#' explained by the successive global components and their cumulative values.
#' @return explained.X  :  Matrix of percentages of inertia explained for each Xb block.
#' @return explained.Y  :  Matrix of percentages of inertia explained for each Y variable.
#' @return contrib  :  Matrix of contribution of each Xb block to the determination of global components.
#' @return T  :  Matrix of global components (scores of individuals).
#' @return C  :  Compromise matrix (unnormed global components).
#' @return U :  Matrix of components associated with the response block of variables.
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
#' @references
#'
#' S. Wold (1984). Three PLS algorithms according to SW. In: Symposium MULDAST (Multivariate Analysis in
#' Science and Technology), Umea University, Sweden. pp. 26–30.\cr
#'
#' E. Tchandao Mangamana, R. Glèlè Kakaï, E.M. Qannari (2021). A general strategy for setting up supervised methods of multiblock data analysis.
#' Chemometrics and Intelligent Laboratory Systems, 217, 104388.
#'
#'
#' @seealso \code{\link{print.MBPLS}}    \code{\link{plot.MBPLS}}    \code{\link{summary.MBPLS}}
#'
#' @examples
#' data(ham)
#' X=ham$X
#' group=ham$group
#' Y=ham$Y
#' res.mbpls <- MBPLS(X, Y, group)
#' res.mbpls
#'
#' @export


# Create a class object MBPLS

MBPLS <- function(X, Y, group, algo="eigen", ncompprint=NULL, scale="none", scaleY="none", option="uniform", optionY="uniform", nstart=10, threshold=1e-8, plotgraph=TRUE, axes=c(1,2)){

  # ---------------------------------------------------------------------------
  # 0. Preliminary tests
  # ---------------------------------------------------------------------------

  if (any(is.na(X)) | any(is.na(Y)))
    stop("No NA values are allowed")
  if (sum(group) != ncol(X))
    stop("The sum of group must be equal to the total number of variables of in all the explanatory blocks")
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
  if (is.character(scaleY)) {
    if (!scaleY %in% c("none","sd"))
      stop("scaleY must be either none or sd")
  }
  else {
    if (!is.numeric(scaleY) | length(scaleY)!=ncol(Y))
      stop("Non convenient scaling parameter")
  }
  if (!option %in% c("none","uniform"))
    stop("option must be either none or uniform")
  if (!optionY %in% c("none","uniform"))
    stop("optionY must be either none or uniform")
  X             <- as.matrix(X)
  Y             <- as.matrix(Y)

  # ---------------------------------------------------------------------------
  # 1. Output preparation
  # ---------------------------------------------------------------------------

  if (is.null(rownames(X)) | is.null(rownames(Y)))
    rownames(X) <- rownames(Y) <- paste("Ind.", seq_len(nrow(X)), sep="") # Give the names for rows if X and Y do not have.
  if (is.null(colnames(X)))
    colnames(X) <- paste("X", seq_len(ncol(X)), sep=".")   # Give the names for columns of X if it does not have.
  if (is.null(colnames(Y)))
    colnames(Y) <- paste("Y", seq_len(ncol(Y)), sep=".")   # Give the names for columns of Y if it does not have.
  if (is.null(names(group)))
    names(group) <- paste("Block", 1:length(group), sep=" ") # Give the names for the groups if it was not defined.

  ntab          <- length(group);    # Number of blocks of variables
  nind          <- nrow(X)           # Number of individuals
  ncolX         <- sum(group)        # Number of columns in all the explanatory blocks of variables
  ncolY         <- ncol(Y)           # Number of variables in Y
  ncomp         <- min(nind-1, ncolX)  # Number of global components of the analysis
  if (is.null(ncompprint)) ncompprint=ncomp
  if (ncompprint > ncomp)  stop(cat("\n ncompprint should be less or equal to", ncomp,".\n"))
  J             <- rep(1:ntab,group) # Will be used to identify the variables of each dataset
  critnstart    <- matrix(0,nstart,ncomp)            # Values of the citerion for each start and each dimension for NIPALS algorithms
  niteration    <- matrix(0,nstart,ncomp)            # Gives the number of iteration made to reach convergence for each start and each dimension
  dimnames(critnstart) <- dimnames(niteration) <- list(paste("Iter.",1:nstart,sep=""), paste("Dim.",1:ncomp,sep=""))
  optimalcrit   <- vector("numeric",length=ncomp)    # Gives the optimal value of the criterion
  names(optimalcrit) <- paste("Dim.",1:ncomp,sep="")
  contrib     <- matrix(0,ntab,ncomp)
  dimnames(contrib) <- list(names(group), paste("Dim.",1:ncomp,sep=""))

  Q            <- array(0,dim=c(ncolY,ncolY,ntab))
  Trace        <- vector("numeric",length=ntab)      # Trace of (Y'XbXb'Y)
  criterion    <- vector("numeric",length=ntab)      # Storage of the criterion to be maximized
  NNLAMBDA     <- matrix(0,ntab,ncomp)   # Specific weights for each block and each dimension
  dimnames(NNLAMBDA) <- list(names(group), paste("Dim.",1:ncomp,sep=""))

  T            <- matrix(0,nrow=nind,ncol=ncomp)       # Global components associated with X
  dimnames(T)  <- list(rownames(X), paste("Dim.",1:ncomp,sep=""))
  U            <- matrix(0,nrow=nind,ncol=ncomp)       # Components associated with Y
  dimnames(U)  <- list(rownames(Y), paste("Dim.",1:ncomp,sep=""))
  T.b          <- array(0,dim=c(nind,ncomp,ntab))      # Block components
  dimnames(T.b)<- list(rownames(X), paste("Dim.",1:ncomp,sep=""), names(group))

  cor.g.b    <- array(0,dim=c(ncomp,ncomp,ntab))  # Correlations of global components with their respective block components
  dimnames(cor.g.b) <- list(paste("Dim.",1:ncomp,sep=""), paste("Dim.",1:ncomp,sep=""), names(group))

  W.b       <- vector("list",length=ntab)   # Weights for the explanatory block components
  blockcor  <- vector("list",length=ntab)   # Correlation coefficient between the original variables of each explanatory block and its block components

  for (j in 1:ntab) {
    W.b[[j]] <- blockcor[[j]]  <- matrix(0,nrow=group[j],ncol=ncomp)
    dimnames(W.b[[j]]) <- dimnames(blockcor[[j]]) <- list(colnames(X[,J==j]), paste("Dim.",1:ncomp,sep=""))

  }

  Px            <- matrix(0,nrow=ncolX,ncol=ncomp)      # Loadings for the global components
  Py            <- matrix(0,nrow=ncolY,ncol=ncomp)      # Y loadings
  W             <- matrix(0,nrow=ncolX,ncol=ncomp)      # Weights for the global components
  Wm            <- matrix(0,nrow=ncolX,ncol=ncomp)      # Modified weights for the global components (to take account of the deflation)
  dimnames(Px)  <- dimnames(W) <- dimnames(Wm) <- list(colnames(X), paste("Dim.",1:ncomp,sep=""))
  dimnames(Py)  <- list(colnames(Y), paste("Dim.",1:ncomp,sep=""))
  IT.X          <- vector("numeric",length=ntab+1)
  IT.Y          <- vector("numeric",length=ncolY+1)
  explained.X   <- matrix(0,nrow=ntab+1,ncol=ncomp)      # percentages of explained inertia of each Xb block and global
  explained.Y   <- matrix(0,nrow=ncolY+1,ncol=ncomp)     # percentages of explained inertia of each Y variable and global
  dimnames(explained.X)<-  list(c(names(group),'Global'), paste("Dim.",1:ncomp, sep=""))
  dimnames(explained.Y)<-  list(c(colnames(Y),'Global'), paste("Dim.",1:ncomp, sep=""))
  cumexplained  <- matrix(0,nrow=ncomp,ncol=4)
  rownames(cumexplained) <- paste("Dim.",1:ncomp,sep="")
  colnames(cumexplained) <- c("%explX", "cum%explX","%explY", "cum%explY") # "%oimp","cum%oimp",
  tb            <- matrix(0,nrow=nind,ncol=ntab)        # Concatenated Xb block components at current dimension comp
  betaY         <- array(0,dim=c(ncolX,ncolY,ncomp))    # Regression coefficients for the Y model (with the scale on Y and X defined in the option)
  dimnames(betaY) <- list(colnames(X),colnames(Y),paste("Dim.",1:ncomp))
  components    <- vector("numeric",length=2)

  Xscale        <- NULL
  Yscale        <- NULL
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

  # Centering and scaling Y

  Yscale$mean   <- apply(Y,2, mean)
  Y             <- scale(Y,center=Yscale$mean, scale=FALSE)   # Default centering

  if (scaleY=="none") {
    Yscale$scale <-rep(1,times=ncol(Y))
  }   else {
    if (scaleY=="sd") {
      sd.tab    <- apply(Y, 2, function (y) {return(sqrt(sum(y^2)/length(y)))})   #sd based on biased variance
      temp      <- sd.tab < 1e-14
      if (any(temp)) {
        warning("Variables with null variance not standardized.")
        sd.tab[temp] <- 1
      }
      Y         <- sweep(Y, 2, sd.tab, "/")
      Yscale$scale <-sd.tab
    }   else {     #specific scaling depending on blocks defined as a vector with a scaling parameter for each variable
      Y         <- sweep(Y, 2, scale, "/")
      Yscale$scale <-scale
    }
  }

  #  Pre-processing: block weighting to set each block inertia equal to 1
  if (optionY=="uniform") {
    inertia     <- inertie(Y)
    w.tab       <- rep(sqrt(inertia) , times =  ncol(Y) )     # weighting parameter applied to each variable
    Y           <- sweep(Y, 2, w.tab, "/")
    Yscale$scale<- Yscale$scale*w.tab
  }

  IT.X[1:ntab]     <- sapply(1:ntab, function(j) {inertie(X[,J==j]) })  # Inertia of each block of variable
  IT.X[ntab+1]     <- sum(IT.X[1:ntab])
  IT.Y[1:ncolY]       <- apply(Y,2, function(y) {return(sum(y^2))})
  IT.Y[ncolY+1]       <- sum(IT.Y[1:ncolY])

  X00=X           # Keep the blocks of variables with the pre-treatment made (as a matrix)

  # Computation of the cross-product matrices among individuals (or association matrices)
  X                <- lapply(seq_len(ntab),function(j) {as.matrix(X[,J==j])}) #mod1

  Trace            <- sapply(seq_len(ntab), function(j) {sum(diag(t(Y)%*%tcrossprod(X[[j]])%*%Y))})

  X0=X            # Keep the blocks of variables with the pre-treatment made (as a list).
  Y0=Y

  Itot             <- 0

  # ---------------------------------------------------------------------------
  # 3. computation of Q and LAMBDA for the various dimensions
  # ---------------------------------------------------------------------------

  for (comp in 1:ncomp)  {                           # Iterative computation of the various components

    if  (algo=="eigen"){
    for(j in 1:ntab) {
      Q[,,j]      <- t(Y)%*%tcrossprod(X[[j]])%*%Y
    }
    sumQ    <- apply(Q,c(1,2),sum)
    reseig  <- eigen(sumQ)
    optimalcrit[comp]    <- reseig$values[1]
    nu       <- reseig$vectors[,1]

    } else { # end if  (algo=="eigen") and start if  (algo=="nipals")

    critt.opt         <- 0                    # several starts

    for (i in seq_len(nstart)) {

      # Initialisations

      index       <- sample(1:nind,1)          # Choose randomly a number
      nu          <- Y0[index,]                # Choosing a row of Y as initialization of nu and normalization (norm of nu=1)
      nu          <- normv(abs(nu))

      critt       <- 0
      deltacrit   <- 1
      iNIPALS     <- 0                        # Find the number of iterations made to reach convergence

      # ---------------------------------------------------------------------------
      # 3.1 NIPALS loop until convergence
      # ---------------------------------------------------------------------------

      while(deltacrit>threshold)  {
        u           <- Y%*%nu
        tb          <- matrix(unlist(lapply(seq_len(ntab),function(j){tcrossprod(X[[j]])%*%u})),nrow=nind,byrow=FALSE) # Block component
        t           <- rowSums(tb)
        t           <- normv(t)
        nu          <- normv(crossprod(Y,t))
        criterion   <- sum(sapply(seq_len(ntab),function(j){t(u)%*%tb[,j]}))
        deltacrit   <- criterion- critt
        critt       <- criterion
        iNIPALS     <- iNIPALS + 1
      } # end while

      if (critt > critt.opt) {      # To be used in case of several random initialization of nu
        nu            <- nu
        critt.opt     <- critt
      }

      critnstart[i,comp]  <- critt
      niteration[i,comp]  <- iNIPALS

    }   # end for (i in seq_len(nstart))

    optimalcrit[comp] <- max(critnstart[,comp])

    } # end if (algo==nipals)

    # ---------------------------------------------------------------------------
    # 3.2 Storage of the results associated with dimension h
    # ---------------------------------------------------------------------------

    U[,comp] <- Y%*%nu

    for(j in 1:ntab) {
      W.b[[j]][,comp] <- t(X[[j]])%*%U[,comp]
      T.b[,comp,j]    <- X[[j]]%*%W.b[[j]][,comp]     # tcrossprod(X[[j]])%*%U[,comp]
      T[,comp]        <- T[,comp] + T.b[,comp,j]
    }
    T[,comp]           <- normv(T[,comp])
    NNLAMBDA[,comp]    <- matrix(sapply(seq_len(ntab),function(j){t(U[,comp])%*%T.b[,comp,j]}),nrow=ntab,byrow=FALSE)

    W[,comp]   <- unlist(sapply(1:ntab,function(j){W.b[[j]][,comp]}))
    W[,comp]   <- normv(W[,comp])

    Px[,comp]                    <- unlist(sapply(1:ntab, function(j){t(X[[j]])%*%T[,comp]}))    # Loadings
    Py[,comp]                    <- crossprod(Y,T[,comp])


    # ---------------------------------------------------------------------------
    # 3.3 Deflation
    # ---------------------------------------------------------------------------

    Y.exp       <- tcrossprod(T[,comp]) %*% Y
    Y0.exp      <- tcrossprod(T[,comp]) %*% Y0
    explained.Y[1:ncolY,comp]    <- apply(Y0.exp,2,function(y) {return(sum(y^2))})
    explained.Y[ncolY+1,comp]    <- sum(explained.Y[1:ncolY,comp])
    Y                     <- Y - Y.exp                                                  # Deflation of Y

    X.exp                   <- lapply(X,function(Xj) {tcrossprod(T[,comp]) %*% Xj})                                                                             # ESSO
    X0.exp                  <- lapply(X0,function(Xj){tcrossprod(T[,comp]) %*% Xj})
    explained.X[1:ntab,comp]   <- sapply(X0.exp,function(x) {sum(x^2)})
    explained.X[ntab+1,comp]   <- sum(explained.X[1:ntab,comp])
    X                          <- lapply(seq_len(ntab),function(j) {X[[j]]-X.exp[[j]]}) # Deflation of X
  }# end for (comp in 1:ncomp)

  explained.X       <- sweep(explained.X[,1:ncomp,drop=FALSE] ,1,IT.X,"/")  # Explained inertia for X
  explained.Y       <- sweep(explained.Y[,1:ncomp,drop=FALSE] ,1,IT.Y,"/")  # Explained inertia for Y

  cumexplained[,1]  <- explained.X[ntab+1,1:ncomp]
  cumexplained[,2]  <- cumsum(cumexplained[,1])
  cumexplained[,3]  <- explained.Y[ncolY+1,1:ncomp]
  cumexplained[,4]  <- cumsum(cumexplained[,3])

  contrib           <- t(t(NNLAMBDA)/colSums(NNLAMBDA))

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

  Wm      <- W %*% solve(t(Px)%*%W,tol=1e-150)      # Weights that take into account the deflation procedure

  # ---------------------------------------------------------------------------
  # 4.3 Computation of beta coefficients (used with the original data scaled)
  # ---------------------------------------------------------------------------

  for (comp in 1:ncomp) {
    betaY[,,comp]          <- tcrossprod(Wm[,1:comp,drop=FALSE],Py[,1:comp,drop=FALSE])
  }



  # ---------------------------------------------------------------------------
  # 4.1 Preparation of the results Global
  # ---------------------------------------------------------------------------

  res$components          <- c(ncomp=ncomp, ncompprint=ncompprint)
  res$optimalcrit         <- optimalcrit[1:ncompprint]
  res$T                   <- T[,1:ncompprint,drop=FALSE]     # Storage of the normed global components associated with X
  res$C                   <- C[,1:ncompprint,drop=FALSE]     # Storage of the unnormed global components associated with X
  res$U                   <- U[,1:ncompprint,drop=FALSE]     # Components associated with Y
  res$explained.X         <- round(100*explained.X[1:ntab,1:ncompprint],2)
  res$explained.Y         <- round(100*explained.Y[1:ncolY,1:ncompprint],2)
  res$cumexplained        <- round(100*cumexplained[1:ncompprint,],2)
  res$contrib             <- round(100*contrib[1:ntab,1:ncompprint],2)
  res$globalcor            <- globalcor[,1:ncompprint]
  res$cor.g.b             <- cor.g.b[1:ncompprint,1:ncompprint,]
  res$betaY               <- betaY[,,1:ncompprint]


  # ---------------------------------------------------------------------------
  # 4.2 Preparation of the results Block
  # ---------------------------------------------------------------------------

  Block$T.b         <-  T.b[,1:ncompprint,]
  Block$blockcor    <-  blockcor
  res$Block         <-  Block                         # Results for each block


  # ---------------------------------------------------------------------------
  # Return res
  # ---------------------------------------------------------------------------

  res$Xscale <- Xscale
  res$call   <- match.call()
  class(res) <- c("MBPLS","list")

  if (plotgraph) {
    plot.MBPLS(res,graphtype="globalscores", axes=axes)
    plot.MBPLS(res,graphtype="globalcor", axes=axes)
    plot.MBPLS(res,graphtype="contrib", axes=axes)
  }
  return(res)
}

