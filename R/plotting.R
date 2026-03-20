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
#' @param ... arguments passed to other methods
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
      circular = circular
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
      fill_scale = fill_scale
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
      circular = circular
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
      fill_scale = fill_scale
    )
  }

  # return
  plt
}

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
    dplyr::as_tibble() |>
    mutate(
      # want a row id for matrix
      .row = row_number() |> as.character()
    ) |>
    pivot_longer(
      cols = -.row,
      names_to = ".col", # pivoting produces a column id
      values_to = ".penalty"
    ) |>
    mutate(
      # set levels to data order from penalty matrix
      .row = factor(.row, levels = r_levs),
      .col = factor(.col, levels = c_levs)
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
#'
`plot_penalty_graph` <- function(
  x,
  layout,
  circular = FALSE
) {
  grph <- x |>
    abs() |>
    tidygraph::as_tbl_graph(directed = FALSE)

  lyt <- grph |>
    ggraph::create_layout(layout = layout, circular = circular)

  lyt |>
    ggraph::ggraph() +
    ggraph::geom_edge_link() +
    ggraph::geom_node_label(ggplot2::aes(label = .data$name))
}


#' @title Visualizing penalty matrix or graph object for a cyclic 1D MRF
#' 
#' @param x an object of class `"cyclic_mrf_penalty"`
#' @param graph logical;
#' @param layout character;
#' @param circular logical;
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
    ...
) {
  assertthat::assert_that(is.logical(graph))
  assertthat::assert_that(is.character(layout))

  v <- visualize.sequential_mrf_penalty(
    x, 
    graph = graph,   
    layout = layout,
    circular = TRUE, 
    ...)
  
  if(isTRUE(graph)){
    #remove the previous node label layer
    v@layers = v@layers[1]
    #add colours denoting the start and end points of the graph
    v <- v +
      ggraph::geom_node_label(ggplot2::aes(label = name,
                                             color = node_id)) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c("red","blue","black"), 
        breaks = c("start","end"),
        na.value = "black"
        )
  }
  
  v
  
}
  