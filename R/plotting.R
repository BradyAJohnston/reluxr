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
        ggplot2::aes(
          fill = log10({{ fill }}),
          alpha = log10({{ fill }})
          ),
        colour = "gray30"
      )

  } else {
    plt <- plt +
      ggplot2::geom_tile(
        ggplot2::aes(
          fill = {{ fill }},
          alpha = {{ fill }}
          ),
        colour = "gray30"
      )
  }

  plt +
    ggplot2::guides(alpha = "none") +
    ggplot2::scale_fill_viridis_c(
      breaks = scales::pretty_breaks(),
      na.value = "white"
      )  +
    ggplot2::scale_x_continuous(
      name = NULL,
      expand = ggplot2::expansion(),
      breaks = 1:23,
      position = "top"
    ) +
    ggplot2::scale_alpha_continuous(
      limits = c(0, NA),
      range = c(0.1, 1)
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
      panel.background = ggplot2::element_rect(fill = "white"),
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


#' Compare Lum and Adjusted
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_wells_comparison <- function(data) {
  data %>%
    pivot_longer(c(lum, adjusted)) %>%
    mutate(
      name = if_else(name == "lum", "Raw", "Deconvoluted"),
      name = factor(name, levels = c("Raw", "Deconvoluted"))
    ) %>%
    plot_wells(value) +
    facet_wrap(~name, ncol = 2, strip.position = "bottom") +
    theme(
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill = "gray40")
    ) +
    labs(title = "Deconvoluted with Average Kernal D")
}

#' Plot Well Values
#'
#' Compare plotted well values over time.
#'
#' @param data Tibble containing `cycle_nr`, `lum`, and `adjusted` columns.
#'
#' @return
#' @export
#'
#' @examples
plot_wells_time <- function(data,
                            instrument_sensitivity = 20) {
  data %>%
    ggplot(aes(cycle_nr, value, colour = name, group = well)) +
    geom_line(aes()) +
    scale_y_log10() +
    facet_wrap(~name, ncol = 1) +
    geom_hline(
      yintercept = instrument_sensitivity,
      linetype = "dashed"
      ) +
    theme_linedraw() +
    theme(
      strip.background = element_rect(fill = "gray30")
    )
}
