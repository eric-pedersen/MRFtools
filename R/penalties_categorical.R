

#' @title Fully connected graph and random effect MRF penalties from a factor
#'
#' @param object an R object to create the MRF penalty from.
#' @param model character; one of `"full"` or `"individual"` indicating if a
#'   fully connected graph (`"full"`) or a random effect (random intercepts;
#'   `"individual"`) penalty is created.
#' @param node_labels character; a vector of alternative labels for the levels
#'   of the factor.
#' @param delta numeric or logical; either the numeric value to add to the
#'   diagonal of the MRF penalty matrix, or a logical value indicating if such
#'   an adjustment should be made. The default is to not alter the diagonal of
#'   the penalty matrix.
#' @param ... arguments passed to other methods.
#' @param type character; deprecated. Use, `model` instead.
#'
#' @export
#'
#' @examples
#' # a factor
#' fv <- factor(letters[1:10])
#'
#' # create the MRF penalty for a fully connected graph
#' p <- mrf_penalty(fv, model = "full")
#' p
#' as.matrix(p)
#'
#' # create the MRF penalty equivalent of random effects
#' p <- mrf_penalty(fv, model = "individual")
#' p
#' as.matrix(p)
`mrf_penalty.factor` <- function(
    object,
    model = c("full", "individual"),
    node_labels = NULL,
    delta = FALSE,
    ...,
    type
) {
  delta <- check_delta(delta)
  model <- match.arg(model)
  node_labels <- levels(object)
  n_levels <- length(node_labels)
  
  if (model == "full") {
    pen <- matrix(-1, n_levels, n_levels)
    diag(pen) <- n_levels - 1 + delta
  } else if (model == "individual") {
    pen <- diag(1, n_levels)
  }
  
  pen_config <- mrf_config(
    type = "categorical",
    model = model,
    node_labels = node_labels,
    delta = delta
  )
  pen <- as_mrf_penalty(pen, config = pen_config)
  pen
}
