# functions designed to specify attributes for MRF penalties, and to covert
# generic penalty matrices into MRFtools-style penalty matrices

## TODO: Add documentation
## TODO: Add tests
#' MRF penalty configuration data
#'
#' @param type character
#' @param model character
#' @param params list?
#' @param node_labels character
#' @param delta numeric
#' @param obj character
#'
#' @export
`mrf_config` <- function(
  type = NULL,
  model = NULL,
  params = NULL,
  node_labels = NULL,
  delta = NULL,
  obj = NULL
) {
  # type: type of MRF used
  # "categorical": factor values
  # "interval": continuous or discrete 1d numeric
  # "lattice": 2- or more-dimensional regular array
  # "tree": 'tree-like' values, including phylogenies, dendrograms
  # "spatial": non-lattice spatial data
  # "dissimilarity": MRF based off of either a distance or dissimiliarity object
  # "network": General network (without special properties as above)

  #params should never be null, even if none are needed for the model
  if (is.null(params)) {
    params <- list()
  }
  ## could return:
  ## unique factor levels associated with the data
  ## MRF type
  ## information for plotting (e.g. the graph, phylogeny, etc.) stored in obj
  config <- list(
    type = type,
    model = model,
    params = params,
    node_labels = node_labels,
    delta = delta,
    obj = obj
  )
  class(config) <- "mrf_config"
  config
}

## TODO: Add documentation
## TODO: Add tests
#' Create a `"mrf_penalty"` object from a matrix and a config
#'
#' @param penalty matrix
#' @param config list
#' @export
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


#' @title Convert a MRF penalty object to a matrix
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
