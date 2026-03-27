## functions for creating "tree-like" penalties (e.g. phylogenies, dendrograms,
## hierarchical clusters)

#' @title MRF penalty from a phylogeny from a phylo4 object
#'
#' @param alpha numeric
#' @param at_tips TODO
#' @param internal_nodes logical; should the internal nodes of the tree be
#'   included in the penalty (`TRUE`), or just the tips (terminal nodes;
#'   `FALSE`)
#'
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom phylobase subset
#' @importFrom methods as
#' @importMethodsFrom phylobase phylo4
#' @importFrom Matrix Diagonal sparseMatrix Matrix diag
#' @importMethodsFrom Matrix t colMeans colSums
#' @examples
#' #loading the geospiza dataset from phylobase
#' library(phylobase)
#' data(geospiza)
#'
#' #Random-walk (rw1) penalty for both tips and nodes:
#' pen_rw <- mrf_penalty(geospiza, model = "rw1")
#'
#' #Same model, but for a reduced number of species
#' species <- c("fortis", "pauper", "fusca", "olivacea")
#' pen_rw_subset <- mrf_penalty(geospiza, model = "rw1", at_tips = species)
#' plot(get_obj(pen_rw_subset))
#'
#' #Random-walk penalty matrix for just the tips for all geospiza data:
#' pen_rw_tips <- mrf_penalty(geospiza, model = "rw1", internal_nodes = FALSE)
#' pen_rw_tips
#'
#' #Ornstein-Uhlenbeck ("ou") process penalty matrix, specifying alpha parameter (autocorrelation)
#' pen_ou <- mrf_penalty(geospiza, model = "ou", alpha = 1)
#'
#'
#' @export
`mrf_penalty.phylo4` <- function(
  object,
  model = c("rw1", "ou", "brownian"),
  alpha = NULL,
  at_tips = NULL,
  internal_nodes = TRUE,
  delta = FALSE,
  ...
) {
  assertthat::assert_that(is.logical(internal_nodes))

  model <- match.arg(model)

  if (model == "ou") {
    #ou model needs  'alpha' specified
    assertthat::assert_that(
      is.numeric(alpha),
      length(alpha) == 1,
      alpha > 1e-5
    )
  }

  delta <- check_delta(delta)

  if (!is.null(at_tips)) {
    object_tips <- as.character(phylobase::tipLabels(object))
    if (any(!at_tips %in% object_tips)) {
      stop(
        "One or more values listed in `at_tips` are not named tips in `object`"
      )
    }
    object <- object[at_tips] |>
      phylobase::extractTree()
  } else {
    #remove any additional data beyond the phylogeny object itself
    object <- phylobase::extractTree(object)
  }

  tip_labs <- phylobase::tipLabels(object)
  if (!phylobase::hasNodeLabels(object)) {
    phylobase::nodeLabels(object) <- paste0(
      "N",
      names(phylobase::nodeLabels(object))
    )
  }
  node_labs <- phylobase::labels(object)

  n_nodes <- length(unique(object@label))
  edgelist <- as(object, "data.frame")
  #removing any ancestor connection from the list
  edgelist <- edgelist[edgelist$ancestor != 0, ]

  i <- pmin(edgelist$node, edgelist$ancestor)
  j <- pmax(edgelist$node, edgelist$ancestor)

  edge_lengths <- edgelist$edge.length

  if (model %in% c("rw1", "brownian")) {
    pen <- prec_rw1(start = i, end = j, n = n_nodes, dists = edge_lengths)
  } else if (model == "ou") {
    pen <- prec_ou(
      start = i,
      end = j,
      n = n_nodes,
      dists = edge_lengths,
      alpha = alpha
    )
  }

  is_tip <- node_labs %in% tip_labs

  if (delta) {
    #add delta values on to the tips. This should be equivalent  to Pagel's
    #Lambda (need to confirm this)
    diag(pen)[is_tip] <- diag(pen)[is_tip] + delta
  }

  if (internal_nodes) {
    node_labels <- as.vector(node_labs)
  } else {
    node_labels <- as.vector(tip_labs)
    tip_nodes <- edgelist$node[is_tip]

    # Need to use a block-matrix inversion to get the precision matrix.
    internal_mat <- pen[-tip_nodes, -tip_nodes]
    cross_mat <- pen[tip_nodes, -tip_nodes]

    pen <- pen[tip_nodes, tip_nodes] -
      cross_mat %*% solve(internal_mat) %*% t(cross_mat)
  }

  pen <- as.matrix(pen)
  pen <- as_mrf_penalty(
    pen,
    config = mrf_config(
      type = "tree",
      model = model,
      params = list(alpha = alpha),
      node_labels = node_labels,
      obj = object,
      delta = delta
    )
  )

  pen
}


#' @title MRF penalty from a phylogeny
#'
#' @inheritParams mrf_penalty.phylo4
#'
#' @importFrom ape vcv drop.tip
#'
#' @examples
#' #Example code
#'
#' @export
`mrf_penalty.phylo` <- function(
  object,
  model = c("rw1", "ou", "brownian"),
  alpha = NULL,
  at_tips = NULL,
  internal_nodes = TRUE,
  delta = FALSE,
  ...
) {
  assertthat::assert_that(is.logical(internal_nodes))

  model <- match.arg(model)

  if (model == "ou") {
    #ou model needs to have 'alpha' specified
    assertthat::assert_that(
      is.numeric(alpha),
      length(alpha) == 1,
      alpha > 1e-5
    )
  }

  delta <- check_delta(delta)

  if (!is.null(at_tips)) {
    if (any(!at_tips %in% object$tip.label)) {
      stop(
        "One or more values listed in `at_tips` are not named tips in `object`"
      )
    }
    object <- ape::keep.tip(object, tip = at_tips)
  }

  tip_labs <- object$tip.label
  n_tips <- length(tip_labs)
  #we are defining "nodes" here as both internal nodes and tips
  n_nodes <- n_tips + object$Nnode
  node_labs <- c(tip_labs, paste0("N", (n_tips + 1):n_nodes))

  i <- pmin(object$edge[, 1], object$edge[, 2])
  j <- pmax(object$edge[, 1], object$edge[, 2])

  edge_lengths <- object$edge.length

  if (model %in% c("rw1", "brownian")) {
    pen <- prec_rw1(
      start = i,
      end = j,
      n = n_nodes,
      dists = edge_lengths
    )
  } else if (model == "ou") {
    pen <- prec_ou(
      start = i,
      end = j,
      n = n_nodes,
      dists = edge_lengths,
      alpha = alpha
    )
  }

  if (delta) {
    # add delta onto the diagonal if requested
    diag(pen) <- diag(pen) + delta
  }

  if (internal_nodes) {
    node_labels <- as.vector(node_labs)
  } else {
    is_tip <- node_labs %in% tip_labs
    tip_nodes <- (1:n_nodes)[is_tip]

    # Need to use a block-matrix inversion to get the precision matrix.
    internal_mat <- pen[-tip_nodes, -tip_nodes]
    cross_mat <- pen[tip_nodes, -tip_nodes]

    pen <- pen[tip_nodes, tip_nodes] -
      cross_mat %*% solve(internal_mat) %*% t(cross_mat)
    node_labels <- as.vector(tip_labs)
  }

  pen <- as.matrix(pen)
  pen <- as_mrf_penalty(
    pen,
    config = mrf_config(
      type = "tree",
      model = model,
      params = list(alpha = alpha),
      node_labels = node_labels,
      obj = object,
      delta = delta
    )
  )

  pen
}

#' @title MRF penalty from a dendrogram
#'
#' @inheritParams mrf_penalty.phylo4
#'
#' @importFrom ape as.phylo.hclust
#' @importFrom stats as.hclust
#' @export
`mrf_penalty.dendrogram` <- function(
  object,
  model = c("rw1", "ou", "brownian"),
  alpha = NULL,
  at_tips = NULL,
  internal_nodes = TRUE,
  delta = FALSE,
  ...
) {
  mrf_penalty(
    ape::as.phylo.hclust(stats::as.hclust(object)),
    model,
    alpha,
    at_tips,
    internal_nodes,
    delta,
    ...
  )
}


#' @title MRF penalty from a hclust object
#'
#' @inheritParams mrf_penalty.phylo4
#'
#' @importFrom ape as.phylo.hclust
#'
#' @export
`mrf_penalty.hclust` <- function(
  object,
  model = c("rw1", "ou", "brownian"),
  alpha = NULL,
  at_tips = NULL,
  internal_nodes = TRUE,
  delta = FALSE,
  ...
) {
  mrf_penalty(
    ape::as.phylo.hclust(object),
    model,
    alpha,
    at_tips,
    internal_nodes,
    delta,
    ...
  )
}
