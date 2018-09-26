`get_mrf` <- function(object, ...) {
  UseMethod("get_mrf")
}

`get_mrf.bam` <- function(object, ...) {
  NextMethod("get_mrf")
}

`get_mrf.gamm` <- function(object, ...) {
  object <- object[["gam"]]
  get_mrf(object, ...)
}

`get_mrf.list` <- function(object, ...) {
  if(!"gam" %in% names(object)){
    stop("Not a gamm or gamm4 object. No smooth object for get_mrf to use")
  }
  object <- object[["gam"]]
  get_mrf(object, ...)
}

`get_mrf.gam` <- function(object, term, ...) {
  ids <- gratia:::which_smooth(object, term)
  smooths <- gratia:::get_smooths_by_id(ids,object)
  mrfs <- vapply(smooths, FUN = gratia:::is_mrf_smooth, FUN.VALUE = logical(1))
  smooths <- smooths[[mrfs]]
  smooths
}





