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
    type) {
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
  
  pen_config <- mrf_config(type = "categorical",
                           model = model,
                           node_labels = node_labels,
                           delta = delta)
  pen <- as_mrf_penalty(pen, 
                        config = pen_config)
  pen
}


#' @title Continuous-time random walk MRF penalty from a numeric vector
#'
#' @param model character; one of "rw1", "rw2", "ar1", or "ou". See Description
#'   for details on the models
#' @param cyclic logical; If TRUE, the end points are treated as neighbouring
#'   each other. See Description for details
#' @param end_points numeric; an optional vector of length 2 providing the end
#'   points of the period of cycle.
#' @inheritParams mrf_penalty.factor
#'
#' @export
#' 
#' @description
#' Models one-dimensional numeric vectors as random-walk models. **STUB**
#' 
#' @examples
#' # rw1: 1st order continuous-time random walk
#' p <- mrf_penalty(1:10, model = "rw1")
#'
#' @examples
#' # linear rw1:
#' p <- mrf_penalty(1:10)
#' as.matrix(p)
#'
#' # cyclic rw1:
#' p <- mrf_penalty(1:10, model = "rw1", cyclic = TRUE)
#' as.matrix(p)
#'
#' # cyclic with user-specified end points
#' p <- mrf_penalty(1:10, model = "rw1", cyclic = TRUE, end_points = c(0,11))
#' as.matrix(p)
`mrf_penalty.numeric` <- function(
    object, 
    model = c("rw1", "rw2","ar1", "ou"),
    cyclic = FALSE,
    rho = NULL,
    at_nodes = NULL, 
    node_labels = NULL, 
    add_missing = NULL, 
    end_points = NULL,
    end_dist = NULL, 
    delta = FALSE, 
    ...
    type
){
  
  delta <- check_delta(delta)
  model <- match.arg(model)
  #Not all model options are implemented as of yet
  if(model %in% c("ou", "ar1", "rw2")){
    stop(paste0('model class "', model, '" is not yet implemented'),
         class(object)[[1L]], ">",
         call. = FALSE)
  }
  #remove duplicated object values
  object <- object[!duplicated(object)]
  
  #If users want to interpolate values to levels not observed in the data, these
  #levels need to be added back in
  if(!is.null(at_nodes)){
    if(any(!object %in% at_nodes)){
      stop("`at_nodes` must include all values included in `object`",
           class(object)[[1L]], ">",
           call. = FALSE)
    }
    new_nodes <- at_nodes[!at_nodes %in% object]
    object <- c(object, new_nodes)
  }

  #make sure the values are sorted
  object <- sort(object)
  n <- length(object)
  
  #dealing with possible misspecification of end_dist, end_points for cyclic
  #penalties
  if(cyclic){
    
    if(!is.null(end_dist)){
      if(!(length(end_dist)==1 & end_dist[1] > 0)){
      stop("`end_dist` should be a single numeric value greater than zero if specified",
           class(object)[[1L]], ">",
           call. = FALSE)
      }
    }
    if(!is.null(end_points)){
      if(!(length(end_points)==2 & is.numeric(end_points))) {
      stop("`end_points` should be a numeric vector with 2 elements",
           class(object)[[1L]], ">",
           call. = FALSE)
      }
      if(end_points[1] > object[1] | end_points[2] < object[n]){
        stop("The range of the evaluated points can not be larger than range of `end_points`",
             class(object)[[1L]], ">",
             call. = FALSE)
      }
    } else{
      #if end points not declared, set them to the end points of the object
      end_points <- range(object)
      }
  }

  #figuring out how to label the axes of the penalty matrix
  if (is.null(node_labels)) {
    node_labels <- as.character(object)
  } else {
    if (length(at_nodes) != length(node_labels)) {
      stop("at_nodes and node_labels need to be the same length")
    }
    if (length(unique(node_labels)) != length(node_labels)) {
      stop("all levels have to be unique")
    }
  }
  
  #create an empty penalty matrix
  pen <- matrix(0,nrow = n,ncol = n)
  
  if(model %in% c("rw1","ou")){
    #calculate distances between values
    loc_diff <- diff(object)
    
    #indices to identify pairwise differences
    i = 1:(n-1)
    j = 2:n
    
    if(cyclic){
      if (is.null(end_dist)) {
        end_dist <- min(loc_diff)
      }
      # set the distance between the first and last observed value to the
      # distance between them, accounting for potentially excluded end points
      # in the observed data
      obs_end_diff <- end_dist + sum(abs(range(object) - end_points))
      i <- c(i, 1)
      j <- c(j, n)
      loc_diff <- c(loc_diff, obs_end_diff)
    }
    
    if(model == "rw1"){
      for(k in 1:length(i)){
        pen[i[k],j[k]] <- -1/loc_diff[k]
      }
      pen <- pen + t(pen)
      diag(pen) <- -colSums(pen) + delta
    } else{
      #ou process still to be implemented
    }
  }
    
  if(cyclic){
    type = c("cyclic","sequential")
  } else{
    type = "sequential"
  }
  
  if(cyclic){
    params <- list(rho = rho, 
                  cyclic = cyclic, 
                  end_points = end_points, 
                  end_dist = end_dist)
  } else{
    #no need to return end points or distances if not cyclic
    params <- list(rho = rho, 
                   cyclic = cyclic)
  }

  pen_config <- mrf_config(
    type = type,
    model = model,
    params = params,
    node_labels = node_labels,
    delta = delta,
    obj = NULL
  )
    
  pen <- as_mrf_penalty(pen, config = pen_config)

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
`mrf_penalty.sf` <- function(
    object, 
    model = "icar",
    node_labels = NULL, 
    buffer = NULL,
    delta = FALSE, 
    ...){
  if(!all(st_geometry_type(object) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("mrf_penalty.sf does not know how to handle geometry types besides 'POLYGON' and 'MULTIPOLYGON'")
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
    config = mrf_config(type = "spatial",
    node_labels = node_labels,
    obj = obj_geom, 
    delta = delta))
  
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
    ...) {
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
      dendrogram = object,
      node_labels = node_labels,
      delta = delta
      )
    )

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
`mrf_penalty.phylo` <- function(
    object, 
    model = c("rw1","Brownian"),
    node_labels = NULL, 
    delta = FALSE,
    eps = 0, 
    ...) {
  
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
  pen <- chol2inv(chol(vcv(object) + eps*diag(length(object$tip.label))))  # faster/more robust than solve(vcv(object)) ??
  diag(pen) <- diag(pen) + delta
  
  pen <- as_mrf_penalty(
    pen, 
    config = mrf_config(
      model = "phylo",
      node_labels = node_labels,
      obj = object,
    delta = delta))

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
    ...){
  
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
    ...) {
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
