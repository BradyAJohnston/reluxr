library(reluxr)
library(tidyverse)

fl <- "inst/newfiles/20220323_DeconvoPlate_384.csv"
counter <- 0
out_list <- list()

last_line_details <- "End Kinetic"

find_end_of_metadata <- function(file) {
  max(which(readLines(file) %>% str_detect(last_line_details)))
}

# find_end_of_metadata(fl)
end_met <- find_end_of_metadata(fl)
raw_readings <- read_csv(fl, skip = end_met)
raw_rows <- readLines(fl)
raw_rows <- raw_rows[seq(end_met, length(raw_rows))]
raw_rows <- raw_rows %>%
  str_remove_all("T\\xb0 ")
col_1 <- raw_readings %>%
  pull(1)
unique_headings <- raw_readings %>%
  drop_na(1) %>%
  pull(1) %>%
  unique()

which(!is.na(col_1))

df <- tibble(
  raw = raw_rows
) %>%
  slice(-c(1,2)) %>%
  ungroup() %>%
  mutate(
    row = seq(nrow(.)),
    group = cut(row, breaks = which(!is.na(col_1)) - 1,
                include.lowest = FALSE, labels = FALSE)
  ) %>%
  group_by(group) %>%
  nest()

# df %>%
#   mutate(
#     data = map(data, function(x) {
#       str_replace_all(x, fixed("T\xb0 "), "")
#     })
#   ) %>%
#   unnest(data)

df <- df %>%
  mutate(
    group = if_else(is.na(group), 17L, group)
  )

df <- df %>%
  mutate(
    chunk = map_chr(data, function(x) {
      str_extract(x$raw[1], "^[^,]+")
    })
  ) %>%

  mutate(
    values = map(data, function(x) {
      y <- x %>%
        slice(-c(1, 2)) %>%
        .$raw


      fl1 <- tempfile()
      writeLines(y, print(fl1))
      read_csv(fl1) %>%
        select(-1)
    })
  )




df <- df %>%
  select(chunk, values) %>%
  mutate(
    values = map(values, function(x) {
      x %>%
        pivot_longer(cols = matches("^\\w\\d{1,2}$")) %>%
        janitor::clean_names()
    })
  ) %>%
  ungroup() %>%
  filter(chunk != "Results") %>%
  unnest(values) %>%
  select(!matches("t_"))

df %>%
  filter(time == max(time, na.rm = TRUE)) %>%
  mutate(
    well = name,
    row = wellr::well_to_rownum(well),
    col = wellr::well_to_colnum(well)
  ) %>%
  reluxr::plot_wells(value) +
  facet_wrap(~chunk) +
  theme(
    strip.placement = "outside"
  )

df <- df %>%
  filter(
    str_detect(chunk, fixed("lum", TRUE))
  ) %>%
  select(group, chunk, time, name, value) %>%
  rename(
    well = name
    ) %>%
  mutate(
    row = wellr::well_to_rownum(well),
    col = wellr::well_to_colnum(well),
    well = wellr::well_join(row, col)
  )



time_to_hours <- function(time) {
  lubridate::hour(time) + lubridate::minute(time) / 60 + lubridate::second(time) / 360
}

df <- df %>%
  mutate(
    time = time_to_hours(time),
    chunk = factor(chunk),
    time = time + 2.25 * (as.numeric(chunk) - 1)
  )

df %>%
  filter(time == max(time, na.rm = TRUE)) %>%
  filter(value == max(value, na.rm = TRUE))
df %>%
  pull(col) %>%
  unique


df %>% pull(col) %>% max

# create_blank_plate(16 * 2 - 1, 24 * 2 - 1) %>%
#   mutate(
#     well = wellr::well_join(row, col),
#     id = wellr::well_to_index(well)
#   ) %>%
#   plot_wells(id)

decon_mat <- df %>%
  calc_bleed_df(
    time,
    value,
    time_cutoff = 2.5,
    calibrate_row = wellr::well_to_rownum("I5"),
    calibrate_col = wellr::well_to_rownum("I5")
  ) %>%
  df_to_matrix("ratio_mean") %>%
  make_decon_matrix()


# df %>%
#   drop_na(value)
#
# df %>% pull(time) %>% unique() %>% length()
#   drop_na(value) %>%
  # frames_to_matrix(value, time)



new_frames_to_matrix <- function(data, value, time) {
  data <- data %>%
    dplyr::arrange(row, col)

  frames <- data %>%
    dplyr::pull({{ time }}) %>%
    unique()

  purrr::map(frames, function(x) {
    data %>%
      dplyr::filter(
        {{ time }} == x
      ) %>%
      dplyr::pull({{ value }})
  }) %>%
    purrr::reduce(rbind) %>%
    as.matrix()
}

# df %>%
#   new_frames_to_matrix(value, time)
#
# df %>%
#   frames_to_matrix(value, time) %>%
#   matrix_to_frames_df()
#   deconvolute_matrix_frames(decon_mat)




mat_D_best <- calc_matrix_D_best(
  data = drop_na(df, value),
  col_value = value,
  col_time = time,
  calibration_well = "I12",
  time_cutoff = 2
  )


lapply(seq(11), function(x) {
  beepr::beep(x)
  # Sys.sleep(2)
})

stop()
df %>%
  ggplot(aes(time, value, group = well)) +
  geom_line() +
  # geom_point() +
  scale_y_log10()# +
  # facet_wrap(~chunk)

df %>%
  ggplot(aes(time, value, group = name)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~chunk, scales = "free") +
  theme_light()


df %>%
  filter(time == max(time, na.rm = TRUE)) %>%
  reluxr::plot_wells(value)
