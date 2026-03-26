## Helper functions for extracting information from MRF penalty objects

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

## TODO: Add documentation
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

## TODO: Add documentation
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

## TODO: Add documentation
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

## TODO: Add documentation
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

## TODO: Add documentation
#' @export
#' @rdname get_type
`get_type.mrf_penalty` <- function(object) {
  get_type(get_config(object))
}

## TODO: Add documentation
#' @export
#' @rdname get_type
`get_type.mrf_config` <- function(object) {
  object[["type"]]
}

## TODO: Add documentation
#' @export
#' @rdname get_type
`get_type.mrf_penalty` <- function(object) {
  get_type(get_config(object))
}

#' @title Extract the original object used to construct an MRF penalty
#'
#' @param object an object of class `"mrf_penalty"` or `"mrf_config"`.
#'
#' @return A length 1 character vector containing the type of MRF penalty.
#'
#' @export
`get_obj` <- function(object) {
  UseMethod("get_obj")
}

## TODO: Add documentation
#' @export
`get_obj.default` <- function(object, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to extract objects from <",
    class(object)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}


## TODO: Add documentation
#' @export
#' @rdname get_obj
`get_obj.mrf_config` <- function(object) {
  object[["obj"]]
}

## TODO: Add documentation
#' @export
#' @rdname get_obj
`get_obj.mrf_penalty` <- function(object) {
  get_obj(get_config(object))
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

## TODO: Add documentation
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

## TODO: Add documentation
#' @rdname get_penalty
#'
#' @export
`get_penalty.mrf_penalty` <- function(penalty, ...) {
  attr(penalty, "mrf_config") <- NULL
  class(penalty) <- "matrix"
  penalty
}

## TODO: implement helper functions to construct penalties for tensor-product MRFs