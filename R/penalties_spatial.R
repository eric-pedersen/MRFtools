
#' @title MRF penalty from polygon or multi-polygon simple features
#'
#' @param buffer numeric; buffer distance for all or for individual elements
#'   of the geometry. See argument `dist` in [sf::st_buffer] for details.
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom sf st_geometry_type st_geometry st_buffer st_sf st_intersects
#'
#' @export
`mrf_penalty.sf` <- function(
    object,
    model = "icar",
    node_labels = NULL,
    buffer = NULL,
    delta = FALSE,
    ...
) {
  if (!all(st_geometry_type(object) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop(
      "mrf_penalty.sf does not know how to handle geometry types besides 'POLYGON' and 'MULTIPOLYGON'"
    )
  }
  delta <- check_delta(delta)
  
  n <- nrow(object)
  
  node_labels <- if (is.null(node_labels)) {
    seq_len(n)
  } else {
    if (is.character(node_labels) && length(node_labels) == 1) {
      if (!node_labels %in% names(object)) {
        stop("node_labels is not a variable that occurs in object")
      }
      object[[node_labels]]
    } else if (is.atomic(node_labels)) {
      if (length(node_labels) != n) {
        stop(
          "node_labels either has to be length 1 or be the same length as the number of rows in object."
        )
      }
      node_labels
    } else {
      stop(
        "node_labels is not an atomic vector or the name of a vector in object"
      )
    }
  }
  node_labels <- as.character(node_labels)
  
  obj_geom <- st_sf(node_labels = node_labels, geometry = st_geometry(object))
  obj_geom <- obj_geom[!duplicated(st_geometry(obj_geom)), ]
  
  if (!is.null(buffer)) {
    obj_geom <- st_buffer(obj_geom, dist = buffer)
  }
  
  pen <- -st_intersects(obj_geom, sparse = FALSE)
  diag(pen) <- -(rowSums(pen) - diag(pen)) + delta
  
  pen <- as_mrf_penalty(
    pen,
    config = mrf_config(
      type = "spatial",
      node_labels = node_labels,
      obj = obj_geom,
      delta = delta
    )
  )
  
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
`mrf_penalty.SpatialPolygonsDataFrame` <- function(
    object,
    model = "icar",
    node_labels = NULL,
    buffer = NULL,
    delta = FALSE,
    ...
) {
  model <- match.arg(model)
  delta <- check_delta(delta)
  
  n <- nrow(object)
  
  node_labels <- if (is.null(node_labels)) {
    seq_len(n)
  } else {
    if (is.character(node_labels) && length(node_labels) == 1) {
      if (!node_labels %in% names(object)) {
        stop("node_labels is not a variable that occurs in object")
      }
      object[[node_labels]]
    } else if (is.atomic(node_labels)) {
      if (length(node_labels) != n) {
        stop(
          "node_labels either has to be length 1 or be the same length as the number of rows in object."
        )
      }
      node_labels
    } else {
      stop(
        "node_labels is not an atomic vector or the name of a vector in object"
      )
    }
  }
  node_labels <- as.character(node_labels)
  
  obj_geom <- st_as_sf(object)
  obj_geom[["node_labels"]] <- node_labels
  obj_geom <- obj_geom[!duplicated(st_geometry(obj_geom)), ]
  
  mrf_penalty(
    obj_geom,
    model = model,
    node_labels = node_labels,
    buffer = buffer,
    delta = delta,
    ...
  )
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
`mrf_penalty.SpatialPolygons` <- function(
    object,
    model = "icar",
    node_labels = NULL,
    buffer = NULL,
    delta = FALSE,
    ...
) {
  model <- match.arg(model)
  delta <- check_delta(delta)
  
  n <- length(object)
  
  node_labels <- if (is.null(node_labels)) {
    seq_len(n)
  } else {
    if (is.atomic(node_labels)) {
      if (length(node_labels) != n) {
        stop(
          "node_labels either has to the same length as the number of rows in object."
        )
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
  
  mrf_penalty(
    obj_geom,
    model = model,
    node_labels = node_labels,
    buffer = buffer,
    delta = delta,
    ...
  )
}
