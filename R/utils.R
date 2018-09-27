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


##' @export
`print.mrf_penalty` <- function(x, ...) {
  #placeholder till we implement a print function
  print.default(x, ...)
}

`check_penalty` <- function(...) {
  list()
}

`mrf_config` <- function(type = NULL, 
                         node_labels = NULL, 
                         geometry = NULL, 
                         phylogeny = NULL,
                         random_walk = NULL,
                         graph  = NULL,
                         dissimiliarities = NULL) {
  #could return:
  #unique factor levels associated with the data
  #MRF type
  #information for plotting (e.g. the graph, phylogeny, etc.)
  #geographic information
  config <- list(type = type, 
       node_labels = node_labels, 
       geometry = geometry, 
       phylogeny = phylogeny,
       random_walk = random_walk,
       graph  = graph,
       dissimiliarities = dissimiliarities)
  class(config) <- "mrf_config" 
  config
}

`as_mrf_penalty` <- function(penalty, config){
  #need to check if penalty is a matrix
  dimnames(penalty) <- list(config[["node_labels"]],config[["node_labels"]])
  class(penalty) <-  c(paste0(config[["type"]],"_mrf_penalty"), "mrf_penalty","matrix")
  attr(penalty, which="mrf_config") <- config
  penalty
}

##' @title Extract MRF node labels from an MRF penalty
##'
##' @param penalty an object of class `"mrf_penalty"`
##'
##' @export
`get_labels` <- function(penalty) {
    config <- attr(penalty, which = "mrf_config")
    config[["node_labels"]]
}
