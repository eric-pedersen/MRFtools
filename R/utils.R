#' @title Extract a fitted MRF
#'
#' @param object An object from which to extract the fitted MRF. Currently only
#'   for objects of classes `gam`, `bam`, and `gamm`, and GAMMs fitted by
#'   [gamm4::gamm4()].
#' @param ... Arguments passed to other methods.
#'
#' @return A object representing the fitted MRF
#'
#' @export
`get_mrf` <- function(object, ...) {
  UseMethod("get_mrf")
}

#' @export
`get_mrf.default` <- function(object, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to extract an MRF penalty from <",
    class(object)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

#' @export
#' @rdname get_mrf
`get_mrf.bam` <- function(object, ...) {
  NextMethod("get_mrf")
}

#' @export
#' @rdname get_mrf
`get_mrf.gamm` <- function(object, ...) {
  object <- object[["gam"]]
  get_mrf(object, ...)
}

#' @export
#' @rdname get_mrf
`get_mrf.list` <- function(object, ...) {
  if (!"gam" %in% names(object)) {
    stop("Not a gamm or gamm4 object. No smooth object for get_mrf to use")
  }
  object <- object[["gam"]]
  get_mrf(object, ...)
}

#' @param term character; the MRF term to extract. Can be a partial match to a
#'   term, which is matched against the smooth label.
#'
#' @export
#' @rdname get_mrf
#'
#' @importFrom gratia which_smooths get_smooths_by_id is_mrf_smooth
`get_mrf.gam` <- function(object, term, ...) {
  ids <- which_smooths(object, term)
  smooths <- get_smooths_by_id(object, ids)
  mrfs <- vapply(smooths, FUN = is_mrf_smooth, FUN.VALUE = logical(1))
  smooths <- smooths[[mrfs]]
  smooths
}

#' @export
`print.mrf_penalty` <- function(x, ...) {
  ## grab the configuration of the MRF
  conf <- get_config(x)
  # type of MRF
  type <- get_type(conf)
  ## print out info on MRF
  writeLines("Markov Random Field penalty")
  writeLines(paste0("Type: ", gsub("_", " ", type)))
  writeLines(paste0("N   : ", nrow(x)))
}

`check_penalty` <- function(...) {
  list()
}

`mrf_config` <- function(
    type = NULL,
    model = NULL,
    params = NULL,
    node_labels = NULL,
    delta = NULL,
    obj = NULL){
    # type: type of MRF used
     # "categorical": factor values 
     # "interval": continuous or discrete 1d numeric
     # "lattice": 2- or more-dimensional regular array
     # "tree": 'tree-like' values, including phylogenies, dendrograms
     # "spatial": non-lattice spatial data
     # "dissimilarity": MRF based off of either a distance or dissimiliarity object
     # "network": General network (without special properties as above)
  
  #params should never be null, even if none are needed for the model
  if(is.null(params)){
    params <- list()
  }
  ## could return:
  ## unique factor levels associated with the data
  ## MRF type
  ## information for plotting (e.g. the graph, phylogeny, etc.) stored in obj
    config <- list(type = type,
                   model = model, 
                   params = params,
                   node_labels = node_labels,
                   delta = delta,
                   obj = obj)
    class(config) <- "mrf_config"
    config
}

`as_mrf_penalty` <- function(penalty, config) {
  #need to check if penalty is a matrix
  dimnames(penalty) <- list(config[["node_labels"]], config[["node_labels"]])
  class(penalty) <- c(
    paste0(config[["type"]], "_mrf_penalty"),
    "mrf_penalty",
    "matrix"
  )
  attr(penalty, which = "mrf_config") <- config
  penalty
}

#' @title Extract MRF node labels from an MRF penalty
#'
#' @param penalty an object of class `"mrf_penalty"`
#'
#' @export
`get_labels` <- function(penalty) {
  config <- get_config(penalty)
  config[["node_labels"]]
}

#' @title Extract the model type and parameters from an MRF penalty
#'
#' @param penalty an object of class `"mrf_penalty"`
#'
#' @export
`get_model` <- function(penalty){
  config <- get_config(penalty)
  out <- list()
  out[["model"]] <- config[["model"]]
  out[["parameters"]] <- config[["params"]]
  #return the delta parameter if one was specified
  if(config[["delta"]]){
    out[["delta"]]  <- config[["delta"]]
  }
  out
}

#' @title Extract configuration details of an MRF penalty
#'
#' @param penalty an object of class `"mrf_penalty"`
#'
#' @return An object of class `"mrf_config"`, a list.
#'
#' @export
`get_config` <- function(penalty) {
  attr(penalty, which = "mrf_config")
}

#' @title Extract the type of MRF from the penalty
#'
#' @param object an object of class `"mrf_penalty"` or `"mrf_config"`.
#'
#' @return A length 1 character vector containing the type of MRF penalty.
#'
#' @export
`get_type` <- function(object) {
  UseMethod("get_type")
}

#' @export
`get_type.default` <- function(object, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to identify the type MRF penalty from <",
    class(object)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

#' @export
#' @rdname get_type
`get_type.mrf_penalty` <- function(object) {
  get_type(get_config(object))
}

#' @export
#' @rdname get_type
`get_type.mrf_config` <- function(object) {
  object[["type"]]
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

#' @title Extract a MRF penalty matrix
#'
#' @param penalty an R object from which to extract the MRF penalty matrix.
#' @param ... arguments passed to other methods.
#'
#' @return A penalty matrix of class `"matrix"`.
#'
#' @export
`get_penalty` <- function(penalty, ...) {
  UseMethod("get_penalty")
}

#' @export
`get_penalty.default` <- function(penalty, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to extract a penalty matrix from <",
    class(penalty)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

#' @rdname get_penalty
#'
#' @export
`get_penalty.mrf_penalty` <- function(penalty, ...) {
  attr(penalty, "mrf_config") <- NULL
  class(penalty) <- "matrix"
  penalty
}

#' Convert a MRF penalty object to a matrix
#'
#' @param x an object inheriting from class `"mrf_penalty"`
#' @param ... arguments passed to other methods
#'
#' @export
#'
#' @examples
#' p <- mrf_penalty(1:10)
#' as.matrix(p)
`as.matrix.mrf_penalty` <- function(x, ...) {
  get_penalty(x)
}
