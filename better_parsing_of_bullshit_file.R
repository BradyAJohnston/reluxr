library(reluxr)
library(tidyverse)

fl <- "inst/newfiles/20220323_DeconvoPlate_384.csv"


read_plate <- function(csv, skip = 0) {
  last_line_details <- "End Kinetic"

  find_end_of_metadata <- function(file) {
    max(which(readLines(file) %>% stringr::str_detect(last_line_details)))
  }
  end_met <- find_end_of_metadata(fl)
  raw_readings <- readr::read_csv(
    file = fl,
    name_repair = "unique",
    skip = find_end_of_metadata(fl)
    )

  df <-
    raw_readings |>
    tidyr::fill(1) |>
    dplyr::mutate(group = cumsum(is.na(...2))) |>
    tidyr::drop_na(2) |>
    dplyr::rename(signal = 1) |>
    dplyr::group_by(group, signal) |>
    tidyr::nest() |>
    dplyr::filter(signal != "Results") |>

    dplyr::mutate(
      data = map(data, janitor::row_to_names, row_number = 1),
      data = map(data, janitor::clean_names),
      data = map(data, ~ dplyr::rename_with(
        .x, .fn = ~ dplyr::if_else(str_detect(.x, "t_"), "temp", .x)
      )),
      data = map(data, tidyr::pivot_longer, cols = c(-1, -2), names_to = "well")
    ) |>
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    dplyr::select(-group) |>
    dplyr::mutate(id = stringr::str_extract(signal, "(?<=_)\\d"),
           signal = stringr::str_extract(signal, ".+(?=_)")) |>
    dplyr::group_by(signal, id) |>
    # drop_na() #|>
    dplyr::mutate(time = lubridate::hms(time),
           value = as.numeric(value))


  df <-
    df |>
    dplyr::group_by(signal, id) |>
    tidyr::nest() |>
    dplyr::mutate(max_time = map_dbl(data, ~ max(as.numeric(.x$time)))) |>
    dplyr::group_by(signal) |>
    dplyr::arrange(signal, id) |>
    dplyr::mutate(
      time_adjust = dplyr::lag(max_time),
      time_adjust = dplyr::if_else(is.na(time_adjust), 0, time_adjust)
    ) |>
    tidyr::unnest(data) |>
    dplyr::mutate(time = as.numeric(time) + time_adjust) |>
    dplyr::select(-max_time, -time_adjust) |>
    dplyr::mutate(id = forcats::fct_relabel(
      .f = id,
      .fun = ~paste0("plate_", .x))
      )
  df
}

df <- read_plate(fl)
# stop()

# df |>
#   filter(value == max(value))

mat <- df |>
  filter(signal == "LUMI", time > 2.5 * 60 * 60) |>
  wellr::well_df_to_mat_frames("value", "time")


D_best <- rl_mat_decon_best(
  mat = mat,
  ref_row = wellr::well_to_rownum("I12"),
  ref_col = wellr::well_to_colnum("I12"),
  b_noise = 5
)

mat_whole <- df |>
  filter(signal == "LUMI") |>
  wellr::well_df_to_mat_frames("value", "time")


mat_whole_dec <- mat_whole |>
  rl_decon_frames(D_best)

(
  mat_whole
  # mat_whole_dec
  |> as_tibble()
  |> mutate(frames = row_number())
  |> pivot_longer(-frames)
  # start the plot
  |> ggplot(aes(frames, value, group = name))
  + geom_line()
  + scale_y_log10(limits = c(0.1, 1e5)) +
    scale_x_continuous(labels = \(x) x * 15 / 60)
) -> base_plot

(
  # mat_whole
  mat_whole_dec
  |> as_tibble()
  |> mutate(frames = row_number())
  |> pivot_longer(-frames)
  # start the plot
  |> ggplot(aes(frames, value, group = name))
  + geom_line()
  + scale_y_log10(limits = c(0.1, 1e5)) +
    scale_x_continuous(labels = \(x) x * 15 / 60)
) -> decon_plot


patchwork::wrap_plots(base_plot, decon_plot)

#
#
#
#
# fluor <- df |>
#   filter(signal == "LUMI") |>
#   mutate(
#     well = wellr::well_format(well),
#     row = wellr::well_to_rownum(well),
#     col = wellr::well_to_colnum(well)
#   )
#
# background <- fluor |>
#   filter(wellr::well_to_colnum(well) == 1) |>
#   summarise(
#     mean = mean(value),
#     sd = sd(value)
#   )
#
# fluor |>
#   filter(
#     value == max(value)
#   )
#
# calc_matrix_D_best(fluor, "time", "value", "I12", instrument_sensitivity = 3 * background$sd)
#
# df  |>
#   ggplot(aes(as.numeric(time) / 60 / 60, value, group = well)) +
#   geom_line(aes(colour = id)) +
#   # geom_point(aes(colour = id), alpha = 0.3) +
#   facet_grid(# cols = vars(id),
#     rows = vars(signal),
#     scales = "free") +
#   scale_y_log10()
