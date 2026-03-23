#' @title Markov Random Field Penalty
#'
#' @inheritParams mrf_penalty.factor
#'
#' @export
`mrf_penalty` <- function(object, ...) {
  UseMethod("mrf_penalty")
}

#' @export
`mrf_penalty.default` <- function(object, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Unable to create an MRF penalty from <",
    class(object)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

