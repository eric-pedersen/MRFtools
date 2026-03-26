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


`prec_ou` <- function(start, end, n, dists, rho){ 
  assertthat::assert_that(
    is.numeric(dists), 
    all(dists>0),
    length(start) == length(end),
    length(start) == length(dists),
    all(start < end),
    is.numeric(rho),
    length(rho) == 1,
    rho > 0,
    rho < 1 - 1e-6
    )
  
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
    is.integer(dists), 
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

