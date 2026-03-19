#' @importFrom generics visualize
#' @export
generics::visualize

#' Visualize a data set or object
#'
#' Synonyms of the [generics::visualize()] method. Alternatives are `vis()` and
#' `visualise()`.
#'
#' @param x A data frame or other object.
#' @param ... Other arguments passed to methods
#'
#' @export
`vis` <- function(x, ...) {
  visualize(x, ...)
}

#' @export
#'
#' @rdname vis
`visualise` <- function(x, ...) {
  visualize(x, ...)
}
