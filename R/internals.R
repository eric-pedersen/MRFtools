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
