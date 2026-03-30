

#' @title Continuous-time random walk MRF penalty from a numeric vector
#'
#' @param model character; one of "rw1", "rw2", "ar1", or "ou". See Description
#'   for details on the models
#' @param cyclic logical; If TRUE, the end points are treated as neighbouring
#'   each other. See Description for details
#' @param rho numeric; autoregression parameter for a discrete-time random walk ("ar1"). abs(rho) must be < 1 if specified.
#' @param alpha numeric; autoregression parameter for a continuous-time random walk ("ou"). rho must be >0. 
#' @param at_nodes numeric; what nodes (I.e. object values) that you want to evaluate the penalty at. Must include all values levels specified in `object`, but can also include additional levels that you want to evaluate the penalty at (see details).
#' @param end_points numeric; an optional vector of length 2 providing the end
#'   points of the period of cycle.
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
#' # create some test data
#' x_cont <- seq(0,5, length = 10)
#' x_disc <- 1:10 
#' 
#' # linear rw1: 1st order continuous-time random walk
#' p1 <- mrf_penalty(x_cont)
#' p1
#'
#' # cyclic rw1:
#' p2 <- mrf_penalty(x_cont, model = "rw1", cyclic = TRUE)
#' p2
#'
#' # cyclic with user-specified end points
#' p3 <- mrf_penalty(x_cont, model = "rw1", cyclic = TRUE, end_points = c(0,6))
#' p3
#' 
#' # Continuous-time auto-regressive (I.e. "ou") model:
#' p4 <- mrf_penalty(x_cont, model = "ou",alpha = 2)
#' p4
#' 
#' # Discrete-time autoregressive model with negative 1st order autocorrelation
#' p5 <- mrf_penalty(x_disc, model = "ar1", rho = -0.5)
#' 
`mrf_penalty.numeric` <- function(
    object,
    model = c("rw1", "rw2", "ar1", "ou"),
    cyclic = FALSE,
    alpha = NULL,
    rho = NULL,
    at_nodes = NULL,
    node_labels = NULL,
    end_points = NULL,
    end_dist = NULL,
    delta = FALSE,
    ...,
    type
) {
  delta <- check_delta(delta)
  model <- match.arg(model)
  
  if (model == "ou") {
    #ou model needs to have 'alpha' specified
    assertthat::assert_that(
      is.numeric(alpha),
      length(alpha) == 1,
      alpha > 1e-5
    )
  }
  
  if (model == "ar1") {
    #ou model needs to have 'rho' specified
    assertthat::assert_that(
      is.numeric(rho),
      length(rho) == 1,
      abs(rho) < (1-1e-6),
      #data and end distances should be (to numerical precision) integers for OU models
      rlang::is_integerish(object),
      is_null(end_dist) || rlang::is_integerish(end_dist)
    )
  }
  
  #Not all model options are implemented as of yet
  if (model %in% c( "rw2")) {
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
  
  #indices to identify pairwise differences
  i <- 1:(n - 1)
  j <- 2:n
  
  #calculate distances between values
  diffs <- diff(object)
  
  if (cyclic) {
    if (is.null(end_dist)) {
      #find the smallest observed distance in the data set. 
      end_diff <- min(diffs)
    }
    # set the distance between the first and last observed value to the
    # distance between them, accounting for potentially excluded end points
    # in the observed data
    obs_end_diff <- end_diff + sum(abs(range(object) - end_points))
    i <- c(i, 1)
    j <- c(j, n)
    diffs <- c(diffs, obs_end_diff)
  }
  
  if (model == "rw1"){
    pen <- prec_rw1(start = i, end = j, n = n, dists = diffs)
  } else if(model == "ou"){
    pen <- prec_ou(start = i, end = j, n = n_nodes, dists = diffs, alpha = alpha)
  } else if(model == "ar1"){
    pen <- prec_ar1(start = i, end = j, n = n_nodes, dists = diffs, rho = rho)
  } else{
   message(paste0("Model type '", model, "' is not yet implemented.")) 
  }
    
  if (cyclic) {
    type <- c("cyclic", "sequential")
    params <- list(
      rho = rho,
      alpha  = alpha,
      cyclic = cyclic,
      end_points = end_points,
      end_dist = end_dist
    )
  } else {
    type = "sequential"
    params <- list(rho = rho, alpha = alpha)
  }
  
  pen_config <- mrf_config(
    type = type,
    model = model,
    params = params,
    node_labels = node_labels,
    delta = delta,
    obj = NULL
  )
  
  pen <- as_mrf_penalty(as.matrix(pen), config = pen_config)
  
  pen
}
