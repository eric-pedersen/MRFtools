## Helper functions for extracting information from MRF penalty objects

#' @title Extract a fitted MRF
#'
#' @param object An object from which to extract the fitted MRF. Currently only
#'   for objects of classes `gam`, `bam`, and `gamm`, and GAMMs fitted by
#'   [gamm4::gamm4()].
#' @param ... Arguments passed to other methods.
#'
#' @return A object representing the fitted MRF
#'
#' @export
`get_mrf` <- function(object, ...) {
  UseMethod("get_mrf")
}

## TODO: Add documentation
#' @export
`get_mrf.default` <- function(object, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to extract an MRF penalty from <",
    class(object)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

#' @export
#' @rdname get_mrf
`get_mrf.bam` <- function(object, ...) {
  NextMethod("get_mrf")
}

#' @export
#' @rdname get_mrf
`get_mrf.gamm` <- function(object, ...) {
  object <- object[["gam"]]
  get_mrf(object, ...)
}

#' @export
#' @rdname get_mrf
`get_mrf.gamm4` <- function(object, ...) {
  if (!"gam" %in% names(object)) {
    stop("Not a gamm or gamm4 object. No smooth object for get_mrf to use")
  }
  object <- object[["gam"]]
  get_mrf(object, ...)
}

## TODO: Add documentation
#' @param term character; the MRF term to extract. Can be a partial match to a
#'   term, which is matched against the smooth label.
#'
#' @export
#' @rdname get_mrf
#'
#' @importFrom gratia which_smooths get_smooths_by_id is_mrf_smooth
`get_mrf.gam` <- function(object, term, ...) {
  ids <- which_smooths(object, term)
  smooths <- get_smooths_by_id(object, ids)
  mrfs <- vapply(smooths, FUN = is_mrf_smooth, FUN.VALUE = logical(1))
  smooths <- smooths[[mrfs]]
  smooths
}

## TODO: Add documentation
#' @export
`print.mrf_penalty` <- function(x, ...) {
  ## grab the configuration of the MRF
  conf <- get_config(x)
  # type of MRF
  type <- get_type(conf)
  ## print out info on MRF
  writeLines("Markov Random Field penalty")
  writeLines(paste0("Type: ", gsub("_", " ", type)))
  writeLines(paste0("N   : ", nrow(x)))
}

#' @title Extract MRF node labels from an MRF penalty
#'
#' @param penalty an object of class `"mrf_penalty"`
#'
#' @export
`get_labels` <- function(penalty) {
  config <- get_config(penalty)
  config[["node_labels"]]
}

#' @title Extract the model type and parameters from an MRF penalty
#'
#' @param penalty an object of class `"mrf_penalty"`
#'
#' @export
`get_model` <- function(penalty) {
  config <- get_config(penalty)
  out <- list()
  out[["model"]] <- config[["model"]]
  out[["parameters"]] <- config[["params"]]
  #return the delta parameter if one was specified
  if (config[["delta"]]) {
    out[["delta"]] <- config[["delta"]]
  }
  out
}

#' @title Extract configuration details of an MRF penalty
#'
#' @param penalty an object of class `"mrf_penalty"`
#'
#' @return An object of class `"mrf_config"`, a list.
#'
#' @export
`get_config` <- function(penalty) {
  attr(penalty, which = "mrf_config")
}

#' @title Extract the type of MRF from the penalty
#'
#' @param object an object of class `"mrf_penalty"` or `"mrf_config"`.
#'
#' @return A length 1 character vector containing the type of MRF penalty.
#'
#' @export
`get_type` <- function(object) {
  UseMethod("get_type")
}

## TODO: Add documentation
#' @export
`get_type.default` <- function(object, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to identify the type MRF penalty from <",
    class(object)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

## TODO: Add documentation
#' @export
#' @rdname get_type
`get_type.mrf_penalty` <- function(object) {
  get_type(get_config(object))
}

## TODO: Add documentation
#' @export
#' @rdname get_type
`get_type.mrf_config` <- function(object) {
  object[["type"]]
}

## TODO: Add documentation
#' @export
#' @rdname get_type
`get_type.mrf_penalty` <- function(object) {
  get_type(get_config(object))
}

#' @title Extract the original object used to construct an MRF penalty
#'
#' @param object an object of class `"mrf_penalty"` or `"mrf_config"`.
#'
#' @return A length 1 character vector containing the type of MRF penalty.
#'
#' @export
`get_obj` <- function(object) {
  UseMethod("get_obj")
}

## TODO: Add documentation
#' @export
`get_obj.default` <- function(object, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to extract objects from <",
    class(object)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}


## TODO: Add documentation
#' @export
#' @rdname get_obj
`get_obj.mrf_config` <- function(object) {
  object[["obj"]]
}

## TODO: Add documentation
#' @export
#' @rdname get_obj
`get_obj.mrf_penalty` <- function(object) {
  get_obj(get_config(object))
}

#' @title Extract a MRF penalty matrix
#'
#' @param penalty an R object from which to extract the MRF penalty matrix.
#' @param ... arguments passed to other methods.
#'
#' @return A penalty matrix of class `"matrix"`.
#'
#' @export
`get_penalty` <- function(penalty, ...) {
  UseMethod("get_penalty")
}

## TODO: Add documentation
#' @export
`get_penalty.default` <- function(penalty, ...) {
  ## want to bail with a useful error;
  ## see Jenny Bryan's Code Smells UseR 2018 talk: rstd.io/code-smells
  stop(
    "Don't know how to extract a penalty matrix from <",
    class(penalty)[[1L]],
    ">",
    call. = FALSE
  ) # don't show the call, simpler error
}

## TODO: Add documentation
#' @rdname get_penalty
#'
#' @export
`get_penalty.mrf_penalty` <- function(penalty, ...) {
  attr(penalty, "mrf_config") <- NULL
  class(penalty) <- "matrix"
  penalty
}

#' @rdname sim_func1d
#' 
#' @title Simulate 1D nonlinear functions
#' 
#' @description
#' Simulates nonlinear functions across a range of values. 
#' 
#' 
`sim_func1D` <- function(
    n_obs = 100, n_funcs = 1,
    trend_type = c("linear","none", "exponential", "saturating", "sigmoid"),
    discrete = FALSE, irregular = FALSE,
    n_sines = 5, spec_power = 1, 
    cycle_strength = 1, trend_strength = 1,
    intercept_sd = 1, 
    trend_scale = 1, 
    derivs = FALSE
    ){
  assertthat::assert_that(is.numeric(n_obs),
                          n_obs > 2,
                          n_funcs > 0)
  trend_type <- match.arg(trend_type)
  
  
  if(discrete){
    x_min <- 1
    if(irregular){
      x_max <- 2*n_obs
      x <- matrix(0,nrow = n_obs, ncol = n_funcs)
      for(i in 1:n_funcs){
        samp <- sample((x_min+1):(x_max-1), size = n_obs - 2, replace = FALSE)
        x[,i] <- c(x_min,sort(samp), x_max)
      }
    }else{
      x_max <- n_obs
      x <- matrix(1:n_obs,nrow = n_obs, ncol = n_funcs)
      
    }

  } else{
    x_min <- 0
    x_max <- 1
    if(irregular){
      x <- matrix(0,nrow = n_obs, ncol = n_funcs)
      for(i in 1:n_funcs){
        samp <- runif(n_obs-2,x_min, x_max)
        x[,i] <- c(x_min,sort(samp), x_max)
      }
    } else{
      x <- matrix(seq(x_min, x_max,length = n_obs), nrow = n_obs, ncol = n_funcs) 
    }
  }
  
  intercepts <- rnorm(n_funcs,0,sd = intercept_sd)
  f <- matrix(intercepts, nrow = n_obs, ncol = n_funcs,byrow = TRUE)
  if(derivs){
    dfdx <- matrix(0, nrow = n_obs, ncol = n_funcs)
  }
  if(n_sines>0){
    freqs <- 1:n_sines
    
    amp_sigma <- freqs^(-spec_power)
    
    amplitudes <- matrix(0, nrow = n_sines, ncol = n_funcs)
    phase_coefs <- matrix(0, nrow = n_sines, ncol = n_funcs)
    
    for(j in 1:n_funcs){
      amplitudes[,j] <- rnorm(n_sines, 0, amp_sigma)
      phase_coefs[,j] <-  runif(n_sines, 0,2*pi)
    }
    
    cyclic <- matrix(0, nrow = n_obs, ncol = n_funcs) 
    if(derivs){
      dcyclic <- matrix(0, nrow = n_obs, ncol = n_funcs) 
    }
    
    for(j in 1:n_funcs){
      for(i in 1:n_sines){
        cyclic[,j] <-cyclic[,j] + amplitudes[i,j]*sin(freqs[i]*(2*pi/x_max*x[,j] + phase_coefs[i,j]))
        if(derivs){
          dcyclic[,j] <- dcyclic[,j] + freqs[i]*(2*pi/x_max)*amplitudes[i,j]*cos(freqs[i]*(2*pi/x_max*x[,j] + phase_coefs[i,j]))
        }
      }
      range_size <- max(cyclic[,j]) - min(cyclic[,j])
      cyclic[,j] <- cycle_strength*cyclic[,j]/range_size
      if(derivs){
        dcyclic[,j] <- cycle_strength*dcyclic[,j]/range_size
      }
    }
    f <- f + cyclic
    if(derivs){
      dfdx <- dfdx + dcyclic
    }
  }
  
  trend_func_list <- list(
    linear = function(x) x/x_max,
    exponential = function(x) (trend_scale+1)^(x/x_max),
    saturating = function(x) trend_scale*x/(trend_scale*x+x_max),
    sigmoid = function(x) stats::plogis(trend_scale*(x/x_max-0.5))
    )
  
  trend_deriv_list <- list(
    linear = function(x) 1/x_max,
    exponential = function(x)((trend_scale+1)^(x/x_max)*ln(trend_scale+1))/x_max,
    saturating = function(x) trend_scale*x_max/(trend_scale*x+x_max)^2,
    sigmoid = function(x) trend_scale/x_max*stats::dlogis(trend_scale*(x/x_max-0.5))
  )
  if(trend_type != "none"){
    trend_func <- trend_func_list[[trend_type]]
    trend_mult <- trend_strength/(trend_func(x_max) - trend_func(x_min))
    trends <- apply(
      x,
      MARGIN = 2, 
      FUN = function(x){
        f <- trend_func(x)
        f <- trend_mult*(f-trend_func(0))
        f
      }
    )
    
    f <- f + trends
    if(derivs){
      
      dtrend_func <- trend_deriv_list[[trend_type]]
      dtrends <- apply(
        x,
        MARGIN = 2, 
        FUN = function(x){
          d <- dtrend_func(x)
          d <- trend_mult*d
          d
        }
      )
      dfdx <- dfdx + dtrends
    }
  }
  
  dat <-  dplyr::tibble(
    x = as.vector(x),
    func = rep(1:n_funcs, each = n_obs),
    intercept = rep(intercepts, each = n_obs),
    trend = as.vector(trends),
    f = as.vector(f)) 
  
  if(derivs){
    dat <- dat |>
      dplyr::mutate(dfdx = as.vector(dfdx))
  }
  
  return(dat)
}

## TODO: Add documentation
#' @rdname rmrf
#' 
#' @description
#' Simulates random draws from a (possibly low-rank) MRF smoother
#' 
#' 
#' @export
`rmrf` = function(
    n, Q, mu, 
    at_nodes = NULL,
    null_rank = NULL, 
    null_vecs = NULL, 
    null_vars = NULL){s
  assertthat::assert_that(rlang::is_integerish(n),
                          length(n) ==1,
                          methods::is(Q, "mrf_penalty"))
  dim <- nrow(Q)
  
}
## TODO: implement helper functions to construct penalties for tensor-product MRFs
