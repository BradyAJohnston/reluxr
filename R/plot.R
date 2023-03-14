#' Plot a Plate-Layout of the Value Column
#'
#' @param data Dataframe with the value column and a column specifying the well
#'   ID.
#' @param value Name of the column containing the value information to be
#'   displayed.
#' @param well Name of the column with the well ID information for formatting
#'   the plate layout.
#' @param trans Name of the transformation to apply to the data. Defaults to
#'   `log10`.
#'
#' @return a `ggplot2::ggplot()` plot.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' fl <- system.file(
#'   "extdata",
#'   "calibrate_tecan",
#'   "calTecan1.xlsx",
#'   package = "reluxr"
#' )
#'
#' dat <- plate_read_tecan(fl)
#'
#' mat_d_best <- dat |>
#'   dplyr::filter(signal == "LUMI") |>
#'   dplyr::filter(time_s > 500) |>
#'   rl_calc_decon_matrix("value", "time_s", ref_well = "E05", b_noise = 30)
#'
#' dat |>
#'   ungroup() |>
#'   dplyr::filter(signal == "LUMI") |>
#'   dplyr::filter(time_s > 500) |>
#'   rl_adjust_plate(value, mat_d_best, time = time_s) |>
#'   summarise(value = mean(value), .by = well) |>
#'   rl_plot_plate(value, trans = log10) +
#'   scale_fill_viridis_c(
#'     limits = c(1, NA)
#'   )
#'
rl_plot_plate <- function(data, value, well = "well", trans = log10) {
  data <- dplyr::mutate(
    data,
    col = well_to_col_num(well),
    row = well_to_row_num(well)
  )

  plt <- ggplot2::ggplot(
    data,
    mapping = ggplot2::aes(
      x = col,
      y = row,
      fill = trans({{ value }})
    )
  ) +
    ggplot2::geom_tile(
      colour = "gray90"
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:24,
      expand = ggplot2::expansion(),
      position = "top"
    ) +
    ggplot2::scale_y_reverse(
      breaks = 1:16,
      labels = LETTERS[1:16],
      expand = ggplot2::expansion()
    ) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = "transparent"
      )
    )

  plt
}
