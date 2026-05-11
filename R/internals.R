# Functions that are only called on internally by MRFtools functions. Call these
# functions from other scripts AT YOUR OWN RISK, as the interface for these 
# functions may change without notice.


#' @keywords internal
#' @title Check delta parameter
#' @description Internal function to check whether the delta parameter is valid
#' when added to the diagonal of a precision matrix
#' 
`check_delta` <- function(delta) {
  if (length(delta) > 1) {
    stop("'delta' has to be a single value, either logical or numeric")
  }
  if (!(is.logical(delta) || is.numeric(delta))) {
    stop("'delta' has to be either logical or numeric")
  }
  if (is.numeric(delta) && delta < 0) {
    stop("'delta' has to be zero or a positive number")
  }
  as.numeric(delta)
}


#' @keywords internal
#' @title Generator for 1st order randm walk
#' @title Internal function for generating precision matrices for 1st order RWs
#'   for 1D data. This is the same as a Brownian motion stochastic process
#'   sampled at a finite number of points
#'
#' @param start Indices for the starting value of an arrow connecting two obs
#'
#' @param end Indices for the starting value of an arrow connecting two obs.
#'   Must be the same length as `start`.
#'
#' @param n Total number of values for the smoother
#'
#' @param dists Distances between pairs of observations. Must be a vector of
#'   positive values of the same length as `start`
#' 
`prec_rw1` <- function(start, end, n, dists){
  assertthat::assert_that(
    is.numeric(dists), 
    all(dists>0),
    length(start) == length(end),
    length(start) == length(dists),
    all(start < end),
    anyDuplicated(cbind(start,end))==0,
    all(dplyr::between(start, 1, n)),
    all(dplyr::between(end, 1, n))
  )
  
  values <- -1/dists
  
  #create the precision matrix
  prec <- Matrix::sparseMatrix(
    i = start, j = end, x = values,
    dims = c(n, n),
    symmetric = TRUE
    )
  
  diag(prec) <- -colSums(prec)
  
  prec
}

#' @keywords internal
#' @title Generator function for an Ornstein-Uhlenbeck process
#' @description Internal function that calculates the precision matrix for a 1d
#'   1st order continuous-time autoregressive random walk (I.e. an
#'   Ornstein-Uhlenbeck, or OU process). Takes two vectors of indices (start and
#'   end) saying which nodes out of the n total nodes are connected to each
#'   other. The `alpha` parameter controls the strength of the autoregressive
#'   component relative to the ends of function.
#'
#' @param start vector of starting indices. All values must be between 1 and n
#' @param end vector of ending indices (nodes that follow the start node in the
#'   list)
#' @param n Total number of nodes in the system
#' @param dists The distances between the pairs of nodes. Must be positive
#'   numbers.
#' @param alpha The autocorrelation strength of the random walk. Must range
#'   between 1e-5 and infinity, with the lower limit set to avoid numerical
#'   issues with dividing by very small numbers
#' 
`prec_ou` <- function(start, end, n, dists, alpha){ 
  assertthat::assert_that(
    is.numeric(dists), 
    all(dists>0),
    length(start) == length(end),
    length(start) == length(dists),
    all(start < end),
    is.numeric(alpha),
    length(alpha) == 1,
    alpha > 1e-5
    )
  #rescale alpha to the range 0-1 (more numerically stable this way)
  rho <- exp(-alpha)
  dist_exp <- rho^dists
  
  #scales covariances so that, in the limit of rho -> 1, results in a rw matrix
  rho_scale <- 2*(1-rho)
  
  values <- - dist_exp/(1-dist_exp^2) * rho_scale
  prec <- Matrix::sparseMatrix(
    i = start, j = end, x = values,
    dims = c(n, n),
    symmetric = TRUE)
  
  diag(prec) <- rho_scale
  for(m in 1:length(start)){
    i <- start[m]
    j <- end[m]
    #need to add the (negatives) of the values to both the start and end point in the diagonals
    #Since the matrix is symmetric, the same edge will not appear twice to allow for that addition
    #by just selecting an index for i
    diag(prec)[c(i,j)] <- diag(prec)[c(i,j)] - dist_exp[m]*values[m]
  }
  
  prec
}

#' @keywords internal
#' @title Generator function for a 1st order discrete-time autoregessive
#'   process.
#'
#' @description Internal function that calculates the precision matrix for a 1d
#'   1st order discrete-time autoregressive random walk. Takes two vectors of
#'   indices (start and end) saying which nodes out of the n total nodes are
#'   connected to each other. The `rho` parameter controls the strength of the
#'   autoregressive component relative to the ends of function.
#'
#' @param start vector of starting indices. All values must be between 1 and n
#' @param end vector of ending indices (nodes that follow the start node in the
#'   list)
#' @param n Total number of nodes in the system
#' @param dists The distances between the pairs of nodes. Must be positive
#'   integers.
#' @param rho The autocorrelation strength of the random walk. Must range
#'   between -1 and 1.
#' 
`prec_ar1` <- function(start, end, n, dists, rho){ 
  assertthat::assert_that(
    #is_integerish checks if the distances are "integer-like" to numerical
    #precision without requiring the user to specify distances as integers.
    #Since this is for a discrete-time random walk, all distances must be
    #integer-like
    rlang::is_integerish(dists), 
    all(dists>0),
    length(start) == length(end),
    length(start) == length(dists),
    all(start < end),
    is.numeric(rho),
    length(rho) == 1,
    abs(rho) <  1-1e-6
  )
  
  #The correlation between two observations distance 1 away is rho, and 
  #increasing this distance leads to scaling rho by the power of the distance
  dist_exp <- rho^dists
  
  #scales covariances so that, in the limit of rho -> 1, results in a rw matrix
  rho_scale <- 2*(1-abs(rho))
  
  #negative of partial autocovariance betwee pairs.
  values <- - dist_exp/(1-dist_exp^2) * rho_scale
  
  #create a sparse precision matrix. 
  prec <- Matrix::sparseMatrix(
    i = start, j = end, x = values,
    dims = c(n, n),
    symmetric = TRUE)
  
  diag(prec) <- rho_scale
  
  for(m in seq_along(start)){
    i <- start[m]
    j <- end[m]
    #need to add the (negatives) of the values to both the start and end point in the diagonals
    #Since the matrix is symmetric, the same edge will not appear twice to allow for that addition
    #by just selecting an index for i
    diag(prec)[c(i,j)] <- diag(prec)[c(i,j)] - dist_exp[m]*values[m]
  }
  
  prec
}

#' @keywords internal
#' @title Generator function for a 2nd order continuous-time random walk for
#'   irregularly sampled points
#'
#' @description Internal function that calculates the precision matrix for a
#'   1-dimensional 2nd order continuous time random walk. The underlying model
#'   is an integrated Wiener process (RW2 in Rue and Held, 2005) across possibly
#'   irregularly spaced points. Values are assumed to be in order, with
#'   distances indexing how far apart observations are. Can be made into a
#'   cyclic RW2 model by specifying a finite end distance (`end_dist`).
#'
#'   The sparse version of this process gives a 2n x 2n block matrix; the upper
#'   n x n block is the precision matrix for the values of the function at the
#'   specified points, the lower right n x n block is the precision matrix for
#'   the derivatives of the function at those points. It has rank 2n-2 (both the
#'   flat line and the linear function fall in the null space).
#'
#'   The dense version of this precision matrix is an n x n matrix for only the
#'   values of the function. It has rank n-1 (the flat line is in the null
#'   space)
#'
#' @param dists The distances between the pairs of nodes. Must be positive
#'   numbers
#' @param n The number of nodes in the series. Must be equal to 1 + the number
#'   of distances
#' @param end_dist The distance between the end points. If infinite, the random
#'   walk is non-cyclic.
#' @param derivs Whether to return the sparse matrix (with both derivatives and
#'   values of the function) or the dense matrix for the function values only.
#' 
`prec_rw2` <- function(dists, n, end_dist = Inf, derivs = FALSE){
  assertthat::assert_that(
    is.numeric(dists), 
    is.integer(n),
    is.logical(derivs), 
    all(dists>0),
    length(n) == 1,
    length(dists) == n-1,
    end_dist >0)
  #scaling to make the largest delta value equal to 1
  #min_dist <- min(c(dists, end_dist))
  #dists <- dists / min_dist
  #end_dist <- end_dist/min_dist
  
  #Values derived from matrices A, B, and C for the CRW2 model from Rue and Held
  #2005 (page 127) scaled by dividing all coefficients by 2
  coefs <- c(6,3,2)
  delta_f <- coefs[1]/dists^3
  delta_x <- coefs[2]/dists^2
  delta_d <- coefs[3]/dists
  
  #which diagonals need to be set for matrix f and d
  k1 <- c(0, 1)
  #which diagonals need to be set for matrix x (non-symmetric)
  k2 <- c(0, 1, -1)
  
  #lists of diagonal vectors for each of the submatrices of the precision matrix
  #function values:
  diags_f <- list(c(delta_f, 0) + c(0, delta_f), -delta_f)
  
  #cross-term values: (value X derivative precision terms)
  diags_x <- list(c(delta_x, 0) - c(0, delta_x), delta_x, -delta_x) 
  
  #derivative values
  diags_d <- list(c(delta_d, 0) + c(0, delta_d), delta_d/2)
  
  #if specifying cyclic smoothers, need the off-diagonal pieces connecting the 
  #first and last observation as well
  if(end_dist < Inf){
    k1 <- c(k1, n-1)
    k2 <- c(k2, n-1, 1-n)
    delta_f0 <- coefs[1]/end_dist^3
    delta_x0 <- coefs[2]/end_dist^2
    delta_d0 <- coefs[3]/end_dist 
    
    # add the connected values to the three main diagonals
    diags_f[[1]][c(1,n)] <- diags_f[[1]][c(1,n)] + c(delta_f0, delta_f0)
    diags_x[[1]][c(1,n)] <- diags_x[[1]][c(1,n)] + c(-delta_x0, delta_x0)
    diags_d[[1]][c(1,n)] <- diags_d[[1]][c(1,n)] + c(delta_d0, delta_d0)
    
    #add extra diagonals for cyclic terms
    diags_f <- c(diags_f, -delta_f0)
    diags_x <- c(diags_x, -delta_x0, delta_x0)
    diags_d <- c(diags_d, delta_d0/2)
  } 

  #create the submatrices 
  f <- Matrix::bandSparse(n=n, k = k1, diagonals = diags_f, symmetric = TRUE)
  x <- Matrix::bandSparse(n=n, k = k2, diagonals = diags_x, symmetric = FALSE)
  d <- Matrix::bandSparse(n=n, k = k1, diagonals = diags_d, symmetric = TRUE)
  
  #merge submatrix into the full penalty matrix and make it sparse symmetric
  pen <- Matrix::rbind2(Matrix::cbind2(f,x), Matrix::cbind2(Matrix::t(x), d))
  pen <- methods::as(pen, "symmetricMatrix")
  
  if(!derivs){
    pen <- calc_subprec(pen, indices = seq_len(n))
  }
  
  pen
}


#' @keywords internal
#' @title Path length distance between two nodes of a phylogenetic tree
#' @description Internal function to get the shortest phylogenetic distance
#'   between two nodes in a given tree
#' @param tree a phylo4 tree object
#' @param tip1 The starting tip (either named or as an index)
#' @param tip2 the ending tip for the path
`get_treedist` <- function(tree, tip1, tip2){
  path <- names(phylobase::shortestPath(tree, tip1, tip2))
  #drop the most recent common ancestor of the two nodes
  path <- path[-1]
  #add the two tips in for calculating the total path
  path <- c(path, tip1, tip2)
  sum(phylobase::edgeLength(tree, path))
}

#' @keywords internal
#' @title Calculate precision matrix for a subset of nodes
#' @description Internal function to find the precision matrix of a subset of
#'   values (specified via indices) using block-inversion block-inversion of
#'   sparse matrices to find the inverse of the submatrix for the selected
#'   indices. Note that, even when given a sparse matrix, the precision matrix
#'   for a subset of nodes will be in general dense.
#'
#' @param mat n x n positive semidefinite precision matrix, generated by the
#'   Matrix package
#' @param indices numeric indices for the rows/columns of the submatrix to be
#'   generated. All indices must be between 1 and n with no  repeated indices
`calc_subprec` <- function(mat, indices){
  assertthat::assert_that(
    Matrix::isSymmetric(mat),
    is.integer(indices),
    all(indices > 0),
    all(indices < nrow(mat)),
    !anyDuplicated(indices),
    length(indices)< nrow(mat)
  )
  
  xx <- mat[indices,indices]
  xy <- mat[indices, -indices]
  yy <- mat[-indices, -indices]
  if(nrow(yy) ==1){
    if(yy[1,1] == 0) {
      stop("submatrix is not invertable",call. = FALSE)
    } else{
      yy_inv <- 1/yy
    }
  } else if(Matrix::rankMatrix(yy,method = "qr") < nrow(yy)){
    stop("submatrix is not invertable",call. = FALSE)
  } else{
    yy_inv <- solve(yy)
  }
  
  xx_sub <- xx - xy %*% yy_inv %*% Matrix::t(xy)
  
  xx_sub
}

#' @title Add zeros onto numeric factor level names
#' @description add leading zeros to a vector of numbers to improve sorting. Not
#'   currently used in code; here if needed.
#' @keywords internal
`zero_pad` = function(x){
  stopifnot(is.numeric(x))
  x = format(x)
  x = gsub(pattern = " ", replacement = "0",x = x)
  x
}


#' @noRd
#' @description To-be implemented internal function to check validate that the
#' penalty has the right dimensions and is positive semidefinite
`check_penalty` <- function(...) {
  ## TODO: extend this function
  list()
}
