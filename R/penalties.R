`create_mrf_penalty` <- function(data, ...) {
  UseMethod("create_mrf_penalty")
}

`create_mrf_penalty.factor` <- function(object,type = c("full","individual"), delta = 0, ...) {
  type <- match.arg(type)
  node_labels <- levels(object)
  n_levels <- length(node_labels)
  if(type == "full"){
    pen <- matrix(-1, n_levels, n_levels)
    diag(pen) <- n_levels - 1 + delta
  } else if(type =="individual"){
    pen <- diag(1, n_levels)
  }
  types <-  c("full","individual")
  type_labels <- c("fully_connected_graph","random_intercept")
  names(type_labels) <- types
  pen <- as_mrf_penalty(pen, config = mrf_config(type = type_labels[type],
                                                 node_labels = node_labels))
  pen
}

`create_mrf_penalty.numeric` <- function(object,type = c("linear","cyclic"), node_labels = NULL, delta = 0,
                                        end_points = NULL, ...){
  type <-  match.arg(type)
  object2 <- object
  object <- object[!duplicated(object)]
  n <-  length(object)

  if(is.null(node_labels)){
    node_labels = as.character(object)
  } else{
    if(n != length(node_labels)){
      stop("object and lables need to be the same length")
    }
    if(length(unique(node_labels))!=n){
      stop("all levels have to be unique")
    }
  }

  indices = match(object, sort(object))
  object = sort(object)
  loc_diff = diff(object)

  loc_diff_1 = c(Inf, loc_diff)
  loc_diff_2 = c(loc_diff, Inf)
  pen = diag(1/loc_diff_1 + 1/loc_diff_2 + delta)
  diag(pen[-1,-n]) = diag(pen[-n,-1]) = -1/loc_diff
  if(type=="circular"){
    if(is.null(end_points)) {
      end_points = c(min(object) - 1e-6, max(object) + 1e-6)
    }
    dist_to_end = (object[1]-end_points[1]) + (end_points[2] - object[n])
    pen[1,1] = pen[1,1] + 1/dist_to_end
    pen[n,n] = pen[n,n] + 1/dist_to_end
    pen[n,1] = pen[1,n] = -1/dist_to_end
  }
  pen = pen[indices,indices]

  types <-  c("linear","cyclic")
  type_labels <- c("first_order_random_walk","cyclic_first_order_random_walk")
  names(type_labels) <- types
  pen <- as_mrf_penalty(pen, config = mrf_config(type = type_labels[type],
                                                 node_labels = node_labels,
                                                 random_walk = list(values = object, end_points =end_points,
                                                                    delta = delta)))
  pen
}

##' @importFrom sf st_geometry_type st_geometry st_buffer st_sf st_intersects
##' @importFrom dplyr bind_cols
`create_mrf_penalty.sf` <- function(object, node_labels = NULL, buffer =NULL, ...){
  if(!all(st_geometry_type(object) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("create_mrf_penalty.sf does not know how to handle geometry types besides 'POLYGON' and 'MULTIPOLYGON'")
  }

  n <- nrow(object)

  node_labels <- if(is.null(node_labels)){
    seq_len(n)
  } else{
    if(is.character(node_labels) && length(node_labels) == 1){
      if(!node_labels %in% names(object)) {
        stop("node_labels is not a variable that occurs in object")
      }
      object[[node_labels]]
    } else if(is.atomic(node_labels)){
      if(length(node_labels)!=n){
        stop("node_labels either has to be length 1 or be the same length as the number of rows in object.")
      }
      node_labels
    } else {
      stop("node_labels is not an atomic vector or the name of a vector in object")
    }
  }
  node_labels <- as.character(node_labels)

  obj_geom <- st_sf(node_labels = node_labels,geometry = st_geometry(object))
  obj_geom <- obj_geom[!duplicated(st_geometry(obj_geom)),]

  if(!is.null(buffer)){
    obj_geom <- st_buffer(obj_geom, dist = buffer)
  }

  pen <- st_intersects(obj_geom, sparse = FALSE)
  diag(pen) <- rowSums(pen) - diag(pen)
  pen <- -pen

  pen <- as_mrf_penalty(pen, config = mrf_config(type = "sf",
                                                 node_labels = node_labels,
                                                 geometry = obj_geom))
  pen
}
