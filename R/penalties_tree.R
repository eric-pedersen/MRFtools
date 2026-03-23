## functions for creating "tree-like" penalties (e.g. phylogenies, 


#' @title MRF penalty from a phylogeny
#'
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom ape vcv drop.tip
#' @param eps A value to add to the variance-covariance matrix diagonal to
#' make it positive definite
#' @export
`mrf_penalty.phylo` <- function(
    object,
    model = c("rw1", "Brownian"),
    node_labels = NULL,
    delta = FALSE,
    eps = 0,
    ...
) {
  model <- match.arg(model)
  delta <- check_delta(delta)
  
  tip_labs <- object[["tip.label"]]
  if (!is.null(node_labels)) {
    if (length(node_labels) > length(tip_labs)) {
      stop("There are more 'node_labels' than tips in the phylogeny.")
    } else if (length(node_labels) < length(tip_labs)) {
      if (!all(node_labels %in% tip_labs)) {
        stop("Not all node_labels are found in the phylogeny's tip labels.")
      } else {
        object <- drop.tip(object, tip_labs[!tip_labs %in% node_labels])
      }
    } else {
      object[["tip.labels"]] <- node_labels
    }
  } else {
    node_labels <- tip_labs
  }
  ## create penalty matrix
  pen <- chol2inv(chol(vcv(object) + eps * diag(length(object$tip.label)))) # faster/more robust than solve(vcv(object)) ??
  diag(pen) <- diag(pen) + delta
  
  pen <- as_mrf_penalty(
    pen,
    config = mrf_config(
      model = "phylo",
      node_labels = node_labels,
      obj = object,
      delta = delta
    )
  )
  
  pen
}


#' @title MRF penalty from a dendrogram
#'
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom stats cophenetic
#' @export
`mrf_penalty.dendrogram` <- function(
    object,
    model = NULL,
    node_labels = NULL,
    delta = FALSE,
    ...
) {
  delta <- check_delta(delta)
  
  pen <- as.matrix(cophenetic(object))
  pen <- pen - max(pen)
  diag(pen) <- -(rowSums(pen) - diag(pen)) + delta
  
  if (!is.null(node_labels)) {
    if (length(node_labels) != nrow(pen)) {
      stop(
        "'node_labels' is not the same length as the number of observations."
      )
    }
  } else {
    node_labels <- rownames(pen)
  }
  
  pen <- as_mrf_penalty(
    pen,
    config = mrf_config(
      type = "dendrogram",
      model = model,
      obj = object,
      node_labels = node_labels,
      delta = delta
    )
  )
  
  pen
}



#' @title MRF penalty from a hclust object
#'
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom stats as.dendrogram
#'
#' @export
`mrf_penalty.hclust` <- function(object, ...) {
  mrf_penalty(as.dendrogram(object), ...)
}
