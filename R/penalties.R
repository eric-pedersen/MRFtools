`create_mrf_penalty` <- function(data, ...) {
  UseMethod("create_mrf_penalty")
}

`create_mrf_penalty.factor` <- function(object,type = c("full","individual"), delta = 0, ...) {
  type <- match.arg(type)
  obj_levels <- levels(object)
  n_levels <- length(obj_levels)
  if(type == "full"){
    pen <- matrix(-1, n_levels, n_levels)
    diag(pen) <- n_levels - 1 + delta
  } else if(type =="individual"){
    pen <- diag(1, n_levels)
  }
  dimnames(pen) <- list(obj_levels,obj_levels)
  class(pen) <- "mrf_penalty"
  attr(pen,which = "mrf_config") <- mrf_config()
  pen
}
  


