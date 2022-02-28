#' Plots Plate of Data
#'
#' @param data Data frame with row, col, and column for fill data.
#' @param fill Column name for fill data.
#' @param log10_fill Logical, whether or not to log10 transform fill data.
#'
#' @return
#' @export
#'
#' @examples
plot_wells <- function(data, fill, log10_fill = TRUE) {
  plt <- data %>%
    ggplot2::ggplot(ggplot2::aes(col, row))

  if (log10_fill) {
    plt <- plt +
      ggplot2::geom_tile(
        ggplot2::aes(fill = log10( {{ fill }})),
        alpha = 0.9,
        colour = "gray30"
      )

  } else {
    plt <- plt +
      ggplot2::geom_tile(
        ggplot2::aes(fill = {{ fill }}),
        alpha = 0.9,
        colour = "gray30"
      )
  }

  plt +
    ggplot2::scale_fill_viridis_c(breaks = scales::pretty_breaks())  +
    ggplot2::scale_x_continuous(
      name = NULL,
      expand = ggplot2::expansion(),
      breaks = 1:23,
      position = "top"
    ) +
    ggplot2::scale_y_reverse(
      name = NULL,
      expand = ggplot2::expansion(),
      breaks = 1:15,
      labels = LETTERS[1:15]
    ) +
    ggplot2::theme_linedraw() +
    ggplot2::theme(
      aspect.ratio = 15 / 23,
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_text(size = 10),
      axis.ticks = ggplot2::element_blank()
    )
}

#' Adds plate overlay onto well plot.
#'
#' @param colour Colour of overlay.
#' @param size Size of overlay line.
#' @param fill Fill of overlay (default NA).
#'
#' @return
#' @export
#'
#' @examples
overlay_plate <- function(colour = "gray10",
                          size = 1,
                          fill = "NA") {
  ggplot2::annotate(
    geom = "rect",
    ymin = 3.5,
    ymax = 3.5 + 8,
    xmin = 7.5,
    xmax = 7.5 + 12,
    colour = colour,
    size = size,
    fill = fill
  )
}
