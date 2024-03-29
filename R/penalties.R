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
  stop("Unable to create an MRF penalty from <",
    class(object)[[1L]], ">",
    call. = FALSE)           # don't show the call, simpler error
}

#' @title Fully connected graph and random effect MRF penalties from a factor
#'
#' @param object an R object to create the MRF penalty from.
#' @param type character; one of `"full"` or `"individual"` indicating if a
#'   fully connected graph (`"full"`) or a random effect (random intercepts;
#'   `"individual"`) penalty is created.
#' @param node_labels character; a vector of alternative labels for the levels
#'   of the factor.
#' @param add_delta numeric or logical; either the numeric value to add to the
#'   diagonal of the MRF penalty matrix, or a logical value indicating if such
#'   an adjustment should be made. The default is to not alter the diagonal of
#'   the penalty matrix.
#' @param ... arguments passed to other methods.
#'
#' @export
#'
#' @examples
#' # a factor
#' fv <- factor(letters[1:10])
#'
#' # create the MRF penalty for a fully connected graph
#' p <- mrf_penalty(fv, type = "full")
#' p
#' as.matrix(p)
#'
#' # create the MRF penalty equivalent of random effects
#' p <- mrf_penalty(fv, type = "individual")
#' p
#' as.matrix(p)
`mrf_penalty.factor` <- function(object, type = c("full", "individual"),
    node_labels = NULL, add_delta = FALSE, ...) {
  add_delta <- check_delta(add_delta)
  type <- match.arg(type)
  node_labels <- levels(object)
  n_levels <- length(node_labels)
  if (type == "full") {
    pen <- matrix(-1, n_levels, n_levels)
    diag(pen) <- n_levels - 1 + add_delta
  } else if (type == "individual") {
    pen <- diag(1, n_levels)
  }
  types <-  c("full", "individual")
  type_labels <- c("fully_connected_graph", "random_intercept")
  names(type_labels) <- types
  pen <- as_mrf_penalty(pen, config = mrf_config(type = type_labels[type],
    node_labels = node_labels,
    delta = add_delta))
  pen
}

#' @title First-order random walk MRF penalty from a numeric vector
#'
#' @param type character; one of `"linear"` or `"cyclic"` indicating if the
#'   observations form a cyclic series or not.
#' @param end_points numeric; an optional vector of length 2 providing the
#'   end points of the period of cycle.
#' @inheritParams mrf_penalty.factor
#'
#' @export
#'
#' @examples
#' # linear
#' p <- mrf_penalty(1:10)
#' as.matrix(p)
#' 
#' # cyclic
#' p <- mrf_penalty(1:10, type = "cyclic")
#' as.matrix(p)
#'
#' # cyclic with user end points
#' p <- mrf_penalty(1:10, type = "cyclic", end_points = c(0,11))
#' as.matrix(p)
`mrf_penalty.numeric` <- function(object, type = c("linear", "cyclic"),
    node_labels = NULL, add_delta = FALSE, end_points = NULL, ...){
  add_delta <- check_delta(add_delta)
  type <- match.arg(type)
  object2 <- object
  object <- object[!duplicated(object)]
  n <-  length(object)

  if (is.null(node_labels)) {
    node_labels <- as.character(object)
  } else {
    if (n != length(node_labels)) {
      stop("object and lables need to be the same length")
    }
    if (length(unique(node_labels)) != n) {
      stop("all levels have to be unique")
    }
  }

  indices <- match(object, sort(object))
  object <- sort(object)
  loc_diff <- diff(object)

  loc_diff_1 <- c(Inf, loc_diff)
  loc_diff_2 <- c(loc_diff, Inf)
  pen <- diag(1/loc_diff_1 + 1/loc_diff_2 + add_delta)
  diag(pen[-1, -n]) <- diag(pen[-n, -1]) <- -1 / loc_diff
  if (type == "cyclic") {
    if (is.null(end_points)) {
      end_points <- c(min(object) - 1e-6, max(object) + 1e-6)
    }
    dist_to_end <- (object[1] - end_points[1]) + (end_points[2] - object[n])
    pen[1, 1] <- pen[1, 1] + 1 / dist_to_end
    pen[n, n] <- pen[n, n] + 1 / dist_to_end
    pen[n, 1] <- pen[1, n] <- -1 / dist_to_end
  }
  pen <- pen[indices,indices]

  types <-  c("linear","cyclic")
  type_labels <- c("first_order_random_walk","cyclic_first_order_random_walk")
  names(type_labels) <- types
  pen <- as_mrf_penalty(pen,
    config = mrf_config(type = type_labels[type],
      node_labels = node_labels,
      delta = add_delta,
      random_walk = list(values = object, end_points = end_points)))
  pen
}

#' @title MRF penalty from polygon or multi-polygon simple features
#'
#' @param buffer numeric; buffer distance for all or for individual elements
#'   of the geometry. See argument `dist` in [sf::st_buffer] for details.
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom sf st_geometry_type st_geometry st_buffer st_sf st_intersects
#'
#' @export
`mrf_penalty.sf` <- function(object, node_labels = NULL, buffer = NULL,
    add_delta = FALSE, ...){
  if(!all(st_geometry_type(object) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("mrf_penalty.sf does not know how to handle geometry types besides 'POLYGON' and 'MULTIPOLYGON'")
  }
  add_delta <- check_delta(add_delta)

  n <- nrow(object)

  node_labels <- if (is.null(node_labels)) {
    seq_len(n)
  } else {
    if (is.character(node_labels) && length(node_labels) == 1) {
      if (!node_labels %in% names(object)) {
        stop("node_labels is not a variable that occurs in object")
      }
      object[[node_labels]]
    } else if(is.atomic(node_labels)) {
      if (length(node_labels) != n) {
        stop("node_labels either has to be length 1 or be the same length as the number of rows in object.")
      }
      node_labels
    } else {
      stop("node_labels is not an atomic vector or the name of a vector in object")
    }
  }
  node_labels <- as.character(node_labels)

  obj_geom <- st_sf(node_labels = node_labels, geometry = st_geometry(object))
  obj_geom <- obj_geom[!duplicated(st_geometry(obj_geom)), ]

  if (!is.null(buffer)) {
    obj_geom <- st_buffer(obj_geom, dist = buffer)
  }

  pen <- -st_intersects(obj_geom, sparse = FALSE)
  diag(pen) <- -(rowSums(pen) - diag(pen)) + add_delta

  pen <- as_mrf_penalty(pen, config = mrf_config(type = "sf",
    node_labels = node_labels,
    geometry = obj_geom,
    delta = add_delta))
  pen
}

#' @title MRF penalty from a dendrogram
#'
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom stats cophenetic
#' @export
`mrf_penalty.dendrogram` <- function(object, node_labels = NULL,
    add_delta = FALSE, ...) {
  add_delta <- check_delta(add_delta)
  pen <- as.matrix(cophenetic(object))
  pen <- pen - max(pen)
  diag(pen) <- -(rowSums(pen) - diag(pen)) + add_delta
  if (!is.null(node_labels)) {
      if (length(node_labels) != nrow(pen)) {
          stop("'node_labels' is not the same length as the number of observations.")
      }
  } else {
      node_labels <- rownames(pen)
  }
  pen <- as_mrf_penalty(pen, config = mrf_config(type = "dendrogram",
    dendrogram = object,
    node_labels = node_labels,
    delta = add_delta))
  pen
}

#' @title MRF penalty from a phylogeny
#'
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom ape vcv drop.tip
#' @param eps A value to add to the variance-covariance matrix diagonal to
#' make it positive definite
#' @export
`mrf_penalty.phylo` <- function(object, node_labels = NULL, add_delta = FALSE,
    eps = 0, ...) {
  add_delta <- check_delta(add_delta)
  tip_labs <- object[["tip.label"]]
  if (!is.null(node_labels)) {
      if (length(node_labels) > length(tip_labs)) {
          stop("There are more 'node_labels' than tips in the phylogeny.")
      } else if(length(node_labels) < length(tip_labs)){
          if (!all(node_labels %in% tip_labs)){
              stop("Not all node_labels are found in the phylogeny's tip labels.")
          } else{
              object <- drop.tip(object, tip_labs[!tip_labs %in% node_labels])
          }
      } else {
          object[["tip.labels"]] <- node_labels
      }
  } else {
      node_labels <- tip_labs
  }
  ## create penalty matrix
  pen <- chol2inv(chol(vcv(object) + eps*diag(length(object$tip.label))))  # faster/more robust than solve(vcv(object)) ??
  diag(pen) <- diag(pen) + add_delta
  pen <- as_mrf_penalty(pen, config = mrf_config(type = "phylo",
    node_labels = node_labels,
    phylogeny = object,
    delta = add_delta))
  pen
}

#' @title MRF penalty from a SpatialPoylgonsDataFrame
#'
#' @param buffer numeric; buffer distance for all or for individual elements
#'   of the geometry. See argument `dist` in [sf::st_buffer] for details.
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom sf st_as_sf st_geometry
#'
#' @export
`mrf_penalty.SpatialPolygonsDataFrame` <- function(object, node_labels = NULL,
    buffer = NULL, add_delta = FALSE, ...){
  add_delta <- check_delta(add_delta)

  n <- nrow(object)

  node_labels <- if (is.null(node_labels)) {
    seq_len(n)
  } else{
    if (is.character(node_labels) && length(node_labels) == 1){
      if (!node_labels %in% names(object)) {
        stop("node_labels is not a variable that occurs in object")
      }
      object[[node_labels]]
    } else if (is.atomic(node_labels)) {
      if (length(node_labels) != n) {
        stop("node_labels either has to be length 1 or be the same length as the number of rows in object.")
      }
      node_labels
    } else {
      stop("node_labels is not an atomic vector or the name of a vector in object")
    }
  }
  node_labels <- as.character(node_labels)

  obj_geom <- st_as_sf(object)
  obj_geom[["node_labels"]] <- node_labels
  obj_geom <- obj_geom[!duplicated(st_geometry(obj_geom)),]
  mrf_penalty(obj_geom, node_labels = node_labels, buffer = buffer,
    add_delta = add_delta, ...)
}

#' @title MRF penalty from a SpatialPolygons
#'
#' @param buffer numeric; buffer distance for all or for individual elements
#'   of the geometry. See argument `dist` in [sf::st_buffer] for details.
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom sf st_as_sf st_geometry
#'
#' @export
`mrf_penalty.SpatialPolygons` <- function(object, node_labels = NULL,
    buffer = NULL, add_delta = FALSE, ...) {
  add_delta <- check_delta(add_delta)

  n <- length(object)

  node_labels <- if (is.null(node_labels)) {
    seq_len(n)
  } else{
    if (is.atomic(node_labels)) {
      if (length(node_labels) != n) {
        stop("node_labels either has to the same length as the number of rows in object.")
      }
      node_labels
    } else {
      stop("node_labels is not an atomic vector.")
    }
  }
  node_labels <- as.character(node_labels)

  obj_geom <- st_as_sf(object)
  obj_geom[["node_labels"]] <- node_labels
  obj_geom <- obj_geom[!duplicated(st_geometry(obj_geom)), ]
  mrf_penalty(obj_geom, node_labels = node_labels, buffer = buffer,
              delta = add_delta, ...)

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
