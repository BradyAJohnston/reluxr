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

df  |>
  ggplot(aes(as.numeric(time) / 60 / 60, value, group = well)) +
  geom_line(aes(colour = id)) +
  # geom_point(aes(colour = id), alpha = 0.3) +
  facet_grid(# cols = vars(id),
    rows = vars(signal),
    scales = "free") +
  scale_y_log10()
