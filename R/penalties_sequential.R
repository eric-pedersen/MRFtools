

#' @title Continuous-time random walk MRF penalty from a numeric vector
#'
#' @param model character; one of "rw1", "rw2", "ar1", or "ou". See Description
#'   for details on the models
#' @param cyclic logical; If TRUE, the end points are treated as neighbouring
#'   each other. See Description for details
#' @param end_points numeric; an optional vector of length 2 providing the end
#'   points of the period of cycle.
#' @param rho numeric;
#' @param at_nodes numeric;
#' @param add_missing logical;
#' @param end_dist numeric;
#'
#' @inheritParams mrf_penalty.factor
#'
#' @export
#'
#' @description
#' Models one-dimensional numeric vectors as random-walk models.
#'
#' @examples
#' # linear rw1: 1st order continuous-time random walk
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
    model = c("rw1", "rw2", "ar1", "ou"),
    cyclic = FALSE,
    rho = NULL,
    at_nodes = NULL,
    node_labels = NULL,
    add_missing = NULL,
    end_points = NULL,
    end_dist = NULL,
    delta = FALSE,
    ...,
    type
) {
  delta <- check_delta(delta)
  model <- match.arg(model)
  #Not all model options are implemented as of yet
  if (model %in% c("ou", "ar1", "rw2")) {
    stop(
      paste0('model class "', model, '" is not yet implemented'),
      class(object)[[1L]],
      ">",
      call. = FALSE
    )
  }
  #remove duplicated object values
  object <- object[!duplicated(object)]
  
  #If users want to interpolate values to levels not observed in the data, these
  #levels need to be added back in
  if (!is.null(at_nodes)) {
    if (any(!object %in% at_nodes)) {
      stop(
        "`at_nodes` must include all values included in `object`",
        class(object)[[1L]],
        ">",
        call. = FALSE
      )
    }
    new_nodes <- at_nodes[!at_nodes %in% object]
    object <- c(object, new_nodes)
  }
  
  #make sure the values are sorted
  object <- sort(object)
  n <- length(object)
  
  #dealing with possible misspecification of end_dist, end_points for cyclic
  #penalties
  if (cyclic) {
    if (!is.null(end_dist)) {
      if (!(length(end_dist) == 1 & end_dist[1] > 0)) {
        stop(
          "`end_dist` should be a single numeric value greater than zero if specified",
          class(object)[[1L]],
          ">",
          call. = FALSE
        )
      }
    }
    if (!is.null(end_points)) {
      if (!(length(end_points) == 2 & is.numeric(end_points))) {
        stop(
          "`end_points` should be a numeric vector with 2 elements",
          class(object)[[1L]],
          ">",
          call. = FALSE
        )
      }
      if (end_points[1] > object[1] | end_points[2] < object[n]) {
        stop(
          "The range of the evaluated points can not be larger than range of `end_points`",
          class(object)[[1L]],
          ">",
          call. = FALSE
        )
      }
    } else {
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
  pen <- matrix(0, nrow = n, ncol = n)
  
  if (model %in% c("rw1", "ou")) {
    #calculate distances between values
    loc_diff <- diff(object)
    
    #indices to identify pairwise differences
    i = 1:(n - 1)
    j = 2:n
    
    if (cyclic) {
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
    
    if (model == "rw1") {
      for (k in 1:length(i)) {
        pen[i[k], j[k]] <- -1 / loc_diff[k]
      }
      pen <- pen + t(pen)
      diag(pen) <- -colSums(pen) + delta
    } else {
      #ou process still to be implemented
    }
  }
  
  if (cyclic) {
    type = c("cyclic", "sequential")
  } else {
    type = "sequential"
  }
  
  if (cyclic) {
    params <- list(
      rho = rho,
      cyclic = cyclic,
      end_points = end_points,
      end_dist = end_dist
    )
  } else {
    #no need to return end points or distances if not cyclic
    params <- list(rho = rho, cyclic = cyclic)
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
