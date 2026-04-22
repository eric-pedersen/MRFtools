# Functions that are only called on internally by MRFtools functions. Call these
# functions from other scripts AT YOUR OWN RISK, as the interface for these 
# functions may change without notice.

## TODO: extend this function: 
##   Should validate that the penalty has the right  dimensions and is positive
##   semidefinite
`check_penalty` <- function(...) {
  list()
}


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



`prec_rw1` <- function(start, end, n, dists){
  assertthat::assert_that(
    is.numeric(dists), 
    all(dists>0),
    length(start) == length(end),
    length(start) == length(dists),
    all(start < end),
    anyDuplicated(cbind(start,end))==0)
  
  values <- -1/dists
  
  #create the precision matrix
  prec <- Matrix::sparseMatrix(
    i = start, j = end, x = values,
    dims = c(n, n),
    symmetric = TRUE)
  
  diag(prec) <- -colSums(prec)
  
  return(prec)
  
}


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
  
  #scales covariances so that, in the limit of rho -> 1, results in a rw matrixs
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
    diag(prec)[c(i,j)] <- diag(prec)[c(i,j)] - dist_exp[m]*values[m]
  }
  
  return(prec)
}


`prec_ar1` <- function(start, end, n, dists, rho){ 
  assertthat::assert_that(
    rlang::is_integerish(dists), 
    all(dists>0),
    length(start) == length(end),
    length(start) == length(dists),
    all(start < end),
    is.numeric(rho),
    length(rho) == 1,
    abs(rho) <  1-1e-6
  )
  
  dist_exp <- rho^dists
  
  #scales covariances so that, in the limit of rho -> 1, results in a rw matrixs
  rho_scale <- 2*(1-abs(rho))
  
  values <- - dist_exp/(1-dist_exp^2) * rho_scale
  prec <- Matrix::sparseMatrix(
    i = start, j = end, x = values,
    dims = c(n, n),
    symmetric = TRUE)
  
  diag(prec) <- rho_scale
  for(m in 1:length(start)){
    i <- start[m]
    j <- end[m]
    diag(prec)[c(i,j)] <- diag(prec)[c(i,j)] - dist_exp[m]*values[m]
  }
  
  return(prec)
}

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
    pen <- calc_subprec(pen, indices = 1:n)
  }
  
  return(pen)
}


# internal function to get the shortest phylogenetic distance between two nodes
# in a given tree
`get_treedist` <- function(tree, tip1, tip2){
  path <- names(phylobase::shortestPath(tree, tip1, tip2))
  #drop the most recent common ancestor of the two nodes
  path <- path[-1]
  #add the two tips in for calculating the total path
  path <- c(path, tip1, tip2)
  sum(phylobase::edgeLength(tree, path))
}

# function to find the precision matrix of a subset of values (specified via
# indices) using block-inversion block-inversion of sparse matrices to find the
# inverse of the submatrix specified by mat[indices, indices]
`calc_subprec` <- function(mat, indices){
  assertthat::assert_that(
    Matrix::isSymmetric(mat),
    is.integer(indices),
    all(indices > 0),
    all(indices <= nrow(mat)),
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
  
  return(xx_sub)
}


#add leading zeros to a vector of numbers to improve sorting
`zero_pad` = function(x){
  stopifnot(is.numeric(x))
  x = format(x)
  x = gsub(pattern = " ", replacement = "0",x = x)
  x
}


