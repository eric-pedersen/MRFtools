#' Plot a thing
#'
#' @param x an object of class `"first_order_random_walk_mrf_penalty"`
#' @param graph logical;
#' @param layout character;
#' @param circular logical;
#'
#' @export
#'
#' @examples
#' # example code
#' mrf_penalty(1:10, type = "linear") |>
#'   visualize()
`visualize.first_order_random_walk_mrf_penalty` <- function(
  x,
  graph = TRUE,
  layout = "linear",
  circular = FALSE,
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_scale = NULL
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
#' @param graph logical;
#' @param layout character;
#' @param circular logical;
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
  fill_scale = NULL
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
      aes(x = .col, y = .row, fill = .penalty)
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
