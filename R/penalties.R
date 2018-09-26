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

`create_mrf_penalty.numeric` = function(object,type = c("linear","cyclic"), labels = NULL, delta = 0,
                                        end_points = NULL, ...){
  type <-  match.arg(type)
  object2 <- object
  object <- object[!duplicated(object)]
  n <-  length(object)
  
  if(is.null(labels)){
    labels = as.character(object)
  } else{
    if(n != length(labels)){
      stop("object and lables need to be the same length")
    }
    if(length(unique(labels))!=n){
      stop("all levels have to be unique")
    }
  }
  
  indices = match(object, sort(object))
  object = sort(object)
  loc_diff = diff(object)
  
  loc_diff_1 = c(Inf, loc_diff)
  loc_diff_2 = c(loc_diff, Inf)
  pen = diag(1/loc_diff_1 + 1/loc_diff_2 + delta) 
  diag(pen[-1,-n]) = diag(pen[-n,-1]) = -1/loc_diff
  if(type=="circular"){
    if(is.null(end_points)) {
      end_points = c(min(object) - 1e-6, max(object) + 1e-6)
    }
    dist_to_end = (object[1]-end_points[1]) + (end_points[2] - object[n])
    pen[1,1] = pen[1,1] + 1/dist_to_end
    pen[n,n] = pen[n,n] + 1/dist_to_end
    pen[n,1] = pen[1,n] = -1/dist_to_end
  }
  pen = pen[indices,indices]
  dimnames(pen) <- list(labels,labels)
  class(pen) <- "mrf_penalty"
  attr(pen,which = "mrf_config") <- mrf_config()
  pen
}


