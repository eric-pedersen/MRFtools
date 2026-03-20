#' Plot a thing
#'
#' @param x an object of class `"sequential_mrf_penalty"`
#' @param graph logical;
#' @param layout character;
#' @param circular logical;
#'
#' @importFrom tidygraph as_tbl_graph
#' @importFrom ggraph ggraph create_layout geom_edge_link geom_node_label
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate row_number
#' @importFrom ggplot2 ggplot aes geom_raster labs
#' @importFrom colorspace scale_fill_continuous_divergingx
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
  circular = FALSE
) {
  # check arguments
  assertthat::assert_that(is.logical(graph))
  assertthat::assert_that(is.logical(circular))
  assertthat::assert_that(is.character(layout))

  # what to plot
  if (isTRUE(graph)) { # penalty as a graph
    n <- nrow(x)
    
    grph <- x |>
      abs() |>
      tidygraph::as_tbl_graph(directed = FALSE) |>
      tidygraph::activate("nodes") |>
      dplyr::mutate(
        index = 1:n,
        node_id = rep(c("start","middle","end"), 
                      times = c(1,n-2,1)),
        node_id = factor(node_id, levels = c("start","middle","end"))
      ) 

    lyt <- grph |>
      ggraph::create_layout(layout = layout, circular = circular)

    v <- lyt |>
      ggraph::ggraph() +
      ggraph::geom_edge_link() +
      ggraph::geom_node_label(ggplot2::aes(label = name))
  } else { # penalty as a matrix
    r_levs <- rownames(x)
    c_levs <- colnames(x)
    mtrx <- x |>
      as.data.frame() |>
      dplyr::as_tibble(rownames = ".row") |>
      pivot_longer(
        cols = -.row,
        names_to = ".col",
        values_to = ".penalty"
      ) |>
      mutate(
        .row = factor(.row, levels = r_levs),
        .col = factor(.col, levels = c_levs)
      )
    
    v <- mtrx |>
      ggplot(
        aes(
          x = .col,
          y = .row,
          fill = .penalty
        )
      ) +
      geom_raster() +
      colorspace::scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, rev = TRUE) +
      labs(
        x = "col",
        y = "row",
        fill = "penalty"
      )
  }
  
  # return
  v
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
    layout = "linear"
) {
  assertthat::assert_that(is.logical(graph))
  assertthat::assert_that(is.character(layout))

  v <- visualize.sequential_mrf_penalty(x, 
                                        graph = graph,   
                                        layout = layout,
                                        circular = TRUE)
  if(isTRUE(graph)){
    #remove the previous node label layer
    v@layers = v@layers[1]
    #add colours denoting the start and end points of the graph
    v <- v +
      ggraph::geom_node_label(ggplot2::aes(label = name,
                                             color = node_id)) +
      ggplot2::scale_color_manual(name = NULL,
                                  values = c("red","blue","black"), 
                                  breaks = c("start","end"),
                                  na.value = "black")
  }
  
  v
  
}
  