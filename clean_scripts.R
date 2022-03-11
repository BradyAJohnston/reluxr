library(tidyverse)


# Setup Functions ---------------------------------------------------------


# convert letter to it's numeric representation
source("functions.R")

# Read in Calibration Data ------------------------------------------------

df_od <-
  readxl::read_excel(
    path = "inst/Xfiles/tecan/calibration/calTecan1.xlsx",
    skip = 45,
    n_max = 122
  ) %>%
  pivot_longer(
    cols = matches("\\w\\d{1,2}"),
    names_to = "well",
    values_to = "od"
  ) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(
    row = well_to_rowlet(well) %>% let_to_num(),
    col = well_to_colnum(well),
    well = join_well(LETTERS[row], col)
  )

df_lum <-
  readxl::read_excel(
    path = "inst/Xfiles/tecan/calibration/calTecan3.xlsx",
    skip = 168
    ) %>%
  pivot_longer(
    cols = matches("\\w\\d{1,2}"),
    names_to = "well",
    values_to = "lum"
  ) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(
    row = well_to_rowlet(well) %>% let_to_num(),
    col = well_to_colnum(well),
    well = join_well(LETTERS[row], col),
    lum = as.numeric(lum),
    cycle_nr = as.numeric(cycle_nr)
  ) %>%
  group_by(cycle_nr) %>%
  nest() %>%
  mutate(
    max_signal = purrr::map_dbl(data, ~max(.x$lum, na.rm = TRUE))
  ) %>%
    unnest(data) %>%
    ungroup() %>%
    mutate(
      ratio = if_else(lum < 0, 0, lum / max_signal)
    )






# Time Average Bleedthrough -----------------------------------------------

bleed_through <- df_lum %>%
  select(well, row, col, lum, ratio) %>%
  group_by(well, row, col) %>%
  summarise(
    lum_mean = mean(lum, na.rm = TRUE),
    lum_sd   = sd(lum, na.rm = TRUE),
    ratio_mean = mean(ratio, na.rm = TRUE),
    ratio_sd   = sd(ratio, na.rm = TRUE)
  )


# Create Extended Tibble --------------------------------------------------


create_blank_plate <- function(n_cols, n_rows) {
  expand.grid(col = seq(n_cols), row = seq(n_rows)) %>%
    as_tibble()
}



create_extended_tibble <- function(data,
                                   calibrate_row = 5,
                                   calibrate_col = 5,
                                   blank_id_col
                                   ) {

  n_rows <- max(data$row)
  n_cols <- max(data$col)

  row_adjustment <- n_rows - calibrate_row
  col_adjustment <- n_cols - calibrate_col

  blank_av <- data %>%
    filter({{ blank_id_col }}) %>%
    ungroup() %>%
    summarise(
      .groups = "keep",
      b_average_ratio = mean(ratio_mean, na.rm = TRUE),
      b_average_sd    =   sd(ratio_mean, na.rm = TRUE)
    )

  data %>%
    mutate(
      row = row + row_adjustment,
      col = col + col_adjustment
    ) %>%
    right_join(
      create_blank_plate(23, 15)
    ) %>%
    mutate(
      dis = well_dis(
        row = row,
        col = col,
        calibrate_row = calibrate_row + row_adjustment,
        calibrate_col = calibrate_col + col_adjustment
      )
    ) %>%
    group_by(dis) %>%
    mutate(
      average_ratio_mean = mean(ratio_mean, na.rm = TRUE),
      average_ratio_sd    = mean(ratio_sd, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      ratio_mean = if_else(
        is.na(ratio_mean),
        average_ratio_mean,
        ratio_mean
      ),
      ratio_sd = if_else(
        is.na(ratio_sd),
        average_ratio_sd,
        ratio_sd
      ),
      ratio_mean = if_else(
        is.na(ratio_mean),
        blank_av$b_average_ratio,
        ratio_mean
      ),
      ratio_sd = if_else(
        is.na(ratio_sd),
        blank_av$b_average_sd,
        ratio_mean
      )
    ) %>%
    select(well, row, col, lum_mean, lum_sd, ratio_mean, ratio_sd, dis)
}

extended_tibble <- bleed_through %>%
  mutate(
    blank = if_else(col == 12, TRUE, FALSE)
  ) %>%
  create_extended_tibble(blank_id_col = blank)

extended_tibble %>%
  plot_wells(ratio_mean) +
  overlay_plate()


# Create Decon Matrix -----------------------------------------------------



deconvolute_data <- function(data, decon_mat, col) {
  vec_data <- data %>%
    arrange(row, col) %>%
    pull({{ col }})

  vec_adjusted <- solve(decon_mat) %*% vec_data

  data %>%
    mutate(
      adjusted = vec_adjusted
    )
}

extended_tibble %>%
  tibble_to_matrix(ratio_mean) %>%
  make_decon_matrix() %>%
  deconvolute_data(
    data = filter(df_lum, cycle_nr == 100),
    decon_mat = .,
    col = lum
  ) %>%
  plot_wells(adjusted, log10_fill = FALSE)



create_decon_matrix <- function(data,
                                calibrate_row = 5,
                                calibrate_col = 5,
                                blank_col = 12) {
  bleed_through <- data %>%
    select(well, row, col, lum, ratio) %>%
    group_by(well, row, col) %>%
    summarise(
      .groups = "keep",
      lum_mean = mean(lum, na.rm = TRUE),
      lum_sd   = sd(lum, na.rm = TRUE),
      ratio_mean = mean(ratio, na.rm = TRUE),
      ratio_sd   = sd(ratio, na.rm = TRUE)
    )

  extended_tibble <- bleed_through %>%
    mutate(
      blank = if_else(col == blank_col, TRUE, FALSE)
    ) %>%
    create_extended_tibble(
      calibrate_row = calibrate_row,
      calibrate_col = calibrate_col,
      blank_id_col = blank
      )

  extended_tibble %>%
    tibble_to_matrix(ratio_mean) %>%
    make_decon_matrix()
}

# Make Ideal Matrix -------------------------------------------------------

random_extended_matrix <- function(data) {
  matrix_e <- tibble_to_matrix(data, ratio_mean)

  matrix_sd <- tibble_to_matrix(data, ratio_sd)

  matrix_rand <- matrix(rnorm(15 * 23, 0, 1), ncol = 23)

  matrix_e + matrix_rand * matrix_sd
}

create_random_decon_matrix <- function(data,
                                calibrate_row = 5,
                                calibrate_col = 5,
                                blank_col = 12) {
  bleed_through <- data %>%
    select(well, row, col, lum, ratio) %>%
    group_by(well, row, col) %>%
    summarise(
      .groups = "keep",
      lum_mean = mean(lum, na.rm = TRUE),
      lum_sd   = sd(lum, na.rm = TRUE),
      ratio_mean = mean(ratio, na.rm = TRUE),
      ratio_sd   = sd(ratio, na.rm = TRUE)
    )

  extended_tibble <- bleed_through %>%
    mutate(
      blank = if_else(col == blank_col, TRUE, FALSE)
    ) %>%
    create_extended_tibble(
      calibrate_row = calibrate_row,
      calibrate_col = calibrate_col,
      blank_id_col = blank
    )

  extended_tibble %>%
    random_extended_matrix() %>%
    make_decon_matrix()
}


decon_each_frame <- function(data, decon_mat, col) {
  data %>%
    group_by(cycle_nr) %>%
    nest() %>%
    mutate(
      adjusted = purrr::map(
        data,
        deconvolute_data,
        decon_mat = decon_mat,
        col = {{ col }}
        )
    ) %>%
    select(cycle_nr, adjusted) %>%
    unnest(adjusted)
}


create_random_decon_matrix(df_lum)


working_df <- df_lum

instrument_sensitivity <- df_lum %>%
  filter(col == 12) %>%
  summarise(sd(lum)) %>%
  unlist

instrument_sensitivity <- instrument_sensitivity * 3

counter <- 0

found_best <- FALSE

while (!found_best) {
  counter <- counter + 1


  matrix_D_working <- create_random_decon_matrix(working_df)

  deconned_working_data <- working_df %>%
    decon_each_frame(matrix_D_working, lum)

  compared_df <- deconned_working_data %>%
    filter(well != "E05") %>%
    mutate(
      diff = adjusted - instrument_sensitivity,
      lower = diff <= 0
      )

  if (FALSE %in% compared_df$lower) {
    print(counter)
    if(counter == 1) {
      matrix_D_best <<- matrix_D_working

    } else {
      matrix_D_best <<- matrix_D_working %*% matrix_D_best
    }
    print(image(matrix_D_best))

    working_df <<- deconned_working_data

    # plot(working_df$cycle_nr, working_df$adjusted, type = "l")

    print(nrow(deconned_working_data) - sum(compared_df$lower))

  } else {
    found_best <<- TRUE

    matrix_D_best <<- matrix_D_working

  }

}

compared_df %>%
  ggplot(aes(cycle_nr, adjusted)) +
  geom_line(aes(group = well))



# Process The Data --------------------------------------------------------



matrix_D <- create_decon_matrix(df_lum)






adjusted_df <- df_lum %>%
  group_by(cycle_nr) %>%
  nest() %>%
  mutate(
    adjusted = purrr::map(
      data,
      deconvolute_data,
      decon_mat = matrix_D,
      col = lum)
  ) %>%
  select(cycle_nr, adjusted) %>%
  unnest(adjusted)

adjusted_df %>%
  filter(cycle_nr == 100) %>%
  plot_wells(adjusted, log10_fill = FALSE) +
  geom_text(aes(label = round(adjusted, 2)), colour = "white")


adjusted_df %>%
  pivot_longer(
    cols = c(lum, adjusted),
    names_to = "sample",
    values_to = "value"
  ) %>%


  ggplot(aes(cycle_nr, value, group = well, colour = sample)) +
  geom_line() + geom_point()  +

  scale_y_log10()





sample_df <-
  readxl::read_excel(
    path = "inst/Xfiles/tecan/tecanOFF2.xlsx",
    skip = 168
  ) %>%
  pivot_longer(
    cols = matches("\\w\\d{1,2}"),
    names_to = "well",
    values_to = "lum"
  ) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(
    row = well_to_rowlet(well) %>% let_to_num(),
    col = well_to_colnum(well),
    well = join_well(LETTERS[row], col),
    lum = as.numeric(lum),
    cycle_nr = as.numeric(cycle_nr)
  ) %>%
  group_by(cycle_nr) %>%
  nest() %>%
  mutate(
    max_signal = purrr::map_dbl(data, ~max(.x$lum, na.rm = TRUE))
  ) %>%
  unnest(data) %>%
  ungroup() %>%
  mutate(
    ratio = if_else(lum < 0, 0, lum / max_signal)
  )

sample_df %>%
  filter(cycle_nr == 100) %>%
  deconvolute_data(matrix_D, lum) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells(value) +
  facet_wrap(~name, ncol = 1)











