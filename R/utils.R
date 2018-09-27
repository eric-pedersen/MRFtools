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

##' @importFrom gratia which_smooth get_smooths_by_id is_mrf_smooth
`get_mrf.gam` <- function(object, term, ...) {
  ids <- which_smooth(object, term)
  smooths <- get_smooths_by_id(ids,object)
  mrfs <- vapply(smooths, FUN = is_mrf_smooth, FUN.VALUE = logical(1))
  smooths <- smooths[[mrfs]]
  smooths
}


##' @export
`print.mrf_penalty` <- function(x, ...) {
    ## grab the configuration of the MRF
    conf <- get_config(x)

    ## print out info on MRF
    writeLines("Markov Random Field penalty")
    writeLines(paste0("MRF type: ", get_type(conf)))
    writeLines(paste0("N:        ", nrow(x)))
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
    config <- get_config(penalty)
    config[["node_labels"]]
}

##' @title Extract configuration details of an MRF penalty
##'
##' @param penalty an object of class `"mrf_penalty"`
##'
##' @return An object of class `"mrf_config"`, a list.
##'
##' @export
`get_config` <- function(penalty) {
    attr(penalty, which = "mrf_config")
}

##' @title Extract the type of MRF from the penalty
##'
##' @param penalty an object of class `"mrf_penalty"`
##'
##' @return A length 1 character vector containing the type of MRF penalty.
##'
##' @export
`get_type` <- function(penalty) {
    get_config(penalty)[["type"]]
}


`check_delta` <- function(add_delta) {
    if (length(add_delta) > 1) {
        stop("'add_delta' has to be a single value, either logical or numeric")
    }
    if (!(is.logical(add_delta) || is.numeric(add_delta))) {
        stop("'add_delta' has to be either logical or numeric")
    }
    if (is.numeric(add_delta) && add_delta < 0) {
        stop("'add_delta' has to be zero or a positive number")
        }
    return()
}
  
