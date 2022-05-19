library(reluxr)
library(tidyverse)

fl <- "inst/newfiles/20220323_DeconvoPlate_384.csv"
counter <- 0
out_list <- list()

last_line_details <- "End Kinetic"

find_end_of_metadata <- function(file) {
  max(which(readLines(file) %>% str_detect(last_line_details)))
}
end_met <- find_end_of_metadata(fl)
raw_readings <- read_csv(fl, skip = end_met)

df <-
  raw_readings |>
  fill(1) |>
  mutate(
    group = cumsum(is.na(...2))
  ) |>
  drop_na(2) |>
  rename(signal = 1) |>
  group_by(group, signal) |>
  nest() |>
  filter(signal != "Results") |>

  mutate(
    data = map(data, janitor::row_to_names, row_number = 1),
    data = map(data, janitor::clean_names),
    data = map(data, ~dplyr::rename_with(.x, .fn = ~if_else(str_detect(.x, "t_"), "temp", .x))),
    data = map(data, tidyr::pivot_longer, cols = c(-1, -2), names_to = "well"),
    # data = map(data, lubridate::time_length())
    ) |>
  unnest(data) |>
  ungroup() |>
  select(-group) |>
  mutate(
    id = str_extract(signal, "(?<=_)\\d"),
    signal = str_extract(signal, ".+(?=_)")
  ) |>
  group_by(
    signal, id
  ) |>
  # drop_na() #|>
  mutate(
    time = lubridate::hms(time),
    value = as.numeric(value)
  )


df <-
  df |>
  group_by(signal, id) |>
  nest() |>
  mutate(
    max_time = map_dbl(data, ~max(as.numeric(.x$time)))
  ) |>
  group_by(signal) |>
  arrange(signal, id) |>
  mutate(
    time_adjust = lag(max_time),
    time_adjust = if_else(is.na(time_adjust), 0, time_adjust)
  ) |>
  unnest(data) |>
  mutate(
    time = as.numeric(time) + time_adjust
  ) |>
    select(-max_time, -time_adjust) |>
    mutate(
      id = factor(id, labels = c("before", "after"))
    )

df  |>
  ggplot(aes(as.numeric(time)/ 60 / 60, value, group = well)) +
  geom_line(aes(colour = id)) +
  facet_grid(
    # cols = vars(id),
    rows = vars(signal),
    scales = "free"
  ) +
  scale_y_log10()



