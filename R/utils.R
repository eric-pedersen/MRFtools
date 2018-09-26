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
  ids <- which_smooth(object, term)
  smooths <- get_smooths_by_id(ids,object)
  mrfs <- vapply(smooths, FUN = gratia:::is_mrf_smooth, FUN.VALUE = logical(1))
  smooths <- smooths[[mrfs]]
  smooths
}



`mrf_config` <- function(...) {
  #could return:
    #unique factor levels associated with the data
    #MRF type
    #information for plotting (e.g. the graph, phylogeny, etc.)
    #geographic information
  list()
}

##' @export
`print.mrf_penalty` <- function(x, ...) {
  #placeholder till we implement a print function
  print.default(x, ...)
}

`check_penalty` <- function(...) {
  list()
}



