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
#' mat_d_best <-
#'   rl_calc_decon_matrix(lumi, time, ref_well = "E05", b_noise = 30)
#'
#' dat |>
#'   dplyr::filter(time > 30) |>
#'   rl_adjust_plate(value, mat_d_best) |>
#'   rl_plot_plate(value, trans = log10) +
#'   ggplot2::scale_fill_viridis_c(
#'     limits = c(1, NA)
#'   )
#'
rl_plot_plate <- function(data, value, well = "well", trans = "log10") {
  .check_columns_exist(data, rlang::enquos(value, well))

  data <- dplyr::mutate(
    data,
    group = dplyr::pull(data, {{ well }})
  )

  data <- dplyr::summarise(
    dplyr::group_by(data, group),
    value = mean({{ value }}, na.rm = TRUE)
  )

  data <- dplyr::mutate(
    data,
    col = well_to_col_num(group),
    row = well_to_row_num(group)
  )
print(data)
  plt <- ggplot2::ggplot(
    data,
    mapping = ggplot2::aes(
      x = col,
      y = row,
      fill = value
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


  if (trans == "log10") {
    plt +
      ggplot2::scale_fill_viridis_c(
        trans = "log10"
      )
  } else {
    plt
  }
}

#' Plot Plate Readings Over Time
#'
#' @param data
#' @param time
#' @param value
#' @param group
#'
#' @return
#' @export
#'
#' @examples
rl_plot_time <- function(data, value, time = "time", group = "well") {
  .check_columns_exist(data, rlang::enquos(time, value, group))

  data <- dplyr::select(
    data,
    time = {{ time }},
    value = {{ value }},
    group = {{ group }}
  )

  plt <- ggplot2::ggplot(
    data,
    mapping = ggplot2::aes(
      x = .data$time,
      y = .data$value,
      group = .data$group
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10() +
    ggplot2::theme_bw()

  plt
}
