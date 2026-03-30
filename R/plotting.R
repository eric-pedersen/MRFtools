#' Plot a thing
#'
#' @param x an object of class `"sequential_mrf_penalty"`
#' @param graph logical;
#' @param layout character;
#' @param circular logical;
#' @param xlab,ylab,title,subtitle,caption character; labels for plots. If
#'   `xlab` or `ylab` are not supplied, a suitable default is used.
#' @param fill_scale a suitable fill scale to use if plotting the penalty
#'   matrix
#' @param ... arguments passed to other methods and ultimately on to
#'   [ggraph::create_layout()] if `graph = TRUE`.
#'
#' @export
#'
#' @examples
#' # example code
#' mrf_penalty(1:10, type = "linear") |>
#'   visualize()
`visualize.sequential_mrf_penalty` <- function(
  x,
  graph = TRUE,
  layout = "linear",
  circular = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_scale = NULL,
  ...
) {
  # check arguments
  assertthat::assert_that(is.logical(graph))
  assertthat::assert_that(is.logical(circular))
  assertthat::assert_that(is.character(layout))

  # what to plot
  plt <- if (isTRUE(graph)) {
    # penalty as a graph
    plot_penalty_graph(
      x,
      layout = layout,
      circular = circular,
      ...
    )
  } else {
    # penalty as a matrix
    plot_penalty_matrix(
      x,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill_scale = fill_scale,
      ...
    )
  }

  # return
  plt
}

#' Plot a thing
#'
#' @param x an object of class `"fully_connected_graph_mrf_penalty"`
#'
#' @inheritParams visualize.sequential_mrf_penalty
#'
#' @export
#'
#' @examples
#' # example code
#' mrf_penalty(1:10, type = "linear") |>
#'   visualize()
`visualize.fully_connected_graph_mrf_penalty` <- function(
  x,
  graph = TRUE,
  layout = "stress",
  circular = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_scale = NULL,
  ...
) {
  # check arguments
  assertthat::assert_that(is.logical(graph))
  assertthat::assert_that(is.logical(circular))
  assertthat::assert_that(is.character(layout))

  # what to plot
  plt <- if (isTRUE(graph)) {
    # penalty as a graph
    plot_penalty_graph(
      x,
      layout = layout,
      circular = circular,
      ...
    )
  } else {
    # penalty as a matrix
    plot_penalty_matrix(
      x,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill_scale = fill_scale,
      ...
    )
  }

  # return
  plt
}

#' Plot a thing
#'
#' @param x an object of class `"dendrogram_mrf_penalty"`
#'
#' @inheritParams visualize.sequential_mrf_penalty
#'
#' @export
#'
#' @examples
#' # example code
#' hc <- hclust(dist(USArrests), "complete")
#' mrf_penalty(hc, internal_nodes = FALSE) |>
#'   visualize()
`visualize.tree_mrf_penalty` <- function(
  x,
  graph = TRUE,
  layout = "stress",
  circular = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_scale = NULL,
  ...
) {
  # check arguments
  assertthat::assert_that(is.logical(graph))
  assertthat::assert_that(is.logical(circular))
  assertthat::assert_that(is.character(layout))

  # what to plot
  plt <- if (isTRUE(graph)) {
    # penalty as a graph
    plot_penalty_graph(
      x,
      layout = layout,
      circular = circular,
      ...
    )
  } else {
    # penalty as a matrix
    plot_penalty_matrix(
      x,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill_scale = fill_scale,
      ...
    )
  }

  # return
  plt
}

#' @title Visualizing penalty matrix or graph object for a cyclic 1D MRF
#'
#' @param x an object of class `"cyclic_mrf_penalty"`
#'
#' @inheritParams visualize.sequential_mrf_penalty
#'
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph create_layout geom_edge_link geom_node_label
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate row_number
#' @importFrom ggplot2 ggplot aes geom_raster labs scale_color_manual
#' @importFrom colorspace scale_fill_continuous_divergingx
#'
#' @export
#'
#' @examples
#' # example code
#' mrf_penalty(1:10, type = "linear") |>
#'   visualize()
`visualize.cyclic_mrf_penalty` <- function(
  x,
  graph = TRUE,
  layout = "linear",
  circular = TRUE,
  ...
) {
  assertthat::assert_that(is.logical(graph))
  assertthat::assert_that(is.character(layout))

  v <- visualize.sequential_mrf_penalty(
    x,
    graph = graph,
    layout = layout,
    circular = circular,
    ...
  )

  if (isTRUE(graph)) {
    #remove the previous node label layer
    v@layers <- v@layers[1]
    #add colours denoting the start and end points of the graph
    n <- nrow(x)
    node_id <- rep(c("start", NA, "end"), times = c(1, n - 2, 1))
    node_id <- factor(node_id, levels = c("start", "end"))
    v@data$node_id <- node_id
    rm(node_id)

    v <- v +
      ggraph::geom_node_label(
        ggplot2::aes(label = .data$name, color = .data$node_id)
      ) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c("red", "blue"),
        labels = c("start", "end"),
        breaks = c("start", "end"),
        na.value = "black"
      )
  }

  v
}


#internal functions for plotting specific components

#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate row_number
#' @importFrom ggplot2 ggplot aes geom_raster labs
#' @importFrom colorspace scale_fill_continuous_divergingx
`plot_penalty_matrix` <- function(
    x,
    xlab = NULL,
    ylab = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    fill_scale = NULL
) {
  # convert penalty matrix to long form for plotting
  r_levs <- rownames(x)
  c_levs <- colnames(x)
  mtrx <- x |>
    as.data.frame() |>
    dplyr::as_tibble(rownames = ".row") |>
    pivot_longer(
      cols = -.row,
      names_to = ".col", # pivoting produces a column id
      values_to = ".penalty"
    ) |>
    mutate(
      # set levels to data order from penalty matrix
      .row = factor(.row, labels = r_levs, levels = r_levs),
      .col = factor(.col, labels = r_levs, levels = c_levs)
    )
  
  # set up default labels if none supplied
  if (is.null(xlab)) {
    xlab <- "col"
  }
  if (is.null(ylab)) {
    ylab <- "row"
  }
  
  # set up the default fill scale if user didn't supply anything
  if (is.null(fill_scale)) {
    fill_scale <- colorspace::scale_fill_continuous_divergingx(
      palette = 'RdBu',
      mid = 0,
      rev = TRUE
    )
  }
  
  # plot
  mtrx |>
    ggplot(
      aes(x = .data$.col, y = .data$.row, fill = .data$.penalty)
    ) +
    geom_raster() +
    fill_scale +
    labs(
      x = xlab,
      y = ylab,
      fill = "penalty",
      title = title,
      subtitle = subtitle,
      caption = caption
    )
}

#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph create_layout geom_edge_link geom_node_label
#' @importFrom ggplot2 .data aes
#' @importFrom rlang abort caller_env
#'
`plot_penalty_graph` <- function(
    x,
    layout,
    circular = FALSE,
    edge = "link",
    label_nodes = TRUE,
    ...
) {
  edge_fun <- get_edge_fun(edge, call = rlang::caller_env())
  
  # convert to adjacency matrix and thence to a tbl_graph
  grph <- x |>
    abs() |>
    tidygraph::as_tbl_graph(directed = FALSE) |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(node_index = 1:dplyr::n())
  
  # start from a layout base don the graph
  lyt <- grph |>
    ggraph::create_layout(layout = layout, circular = circular, ...)
  
  # start the plot
  plt <- lyt |>
    ggraph::ggraph() +
    edge_fun()
  
  # should we label the nodes
  if (label_nodes) {
    plt <- plt +
      ggraph::geom_node_label(
        ggplot2::aes(label = .data$name)
      )
  }
  
  # return
  plt
}

stop_edge_fun_not_found <- function(
    msg,
    fun,
    call = rlang::caller_env()
) {
  rlang::abort(
    msg,
    class = "edge_not_found",
    fun = fun,
    call = call
  )
}

get_edge_fun <- function(x, call = rlang::caller_env()) {
  x <- paste0("geom_edge_", x)
  fun <- try(match.fun(x), silent = TRUE)
  if (inherits(fun, "try-error")) {
    msg <- c(
      "Problem with 'edge'",
      "x" = paste0("Function '", x, "()' was not found."),
      "i" = "Did you forget to load 'tidygraph'?",
      "i" = "Run 'library(\"tidygraphy\")' and try again."
    )
    stop_edge_fun_not_found(
      msg,
      fun = x,
      call = call
    )
  } else {
    fun
  }
}
