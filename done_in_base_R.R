library(reluxr)
library(tidyverse)

input_lum <- read_plate("inst/Xfiles/tecan/calibration/calTecan1.xlsx")

input_lum <-
  lapply(list.files("inst/Xfiles/tecan/calibration/", full.names = TRUE),
         read_plate) %>%
  do.call(rbind, .) %>%
  group_by(cycle_nr, well, col, row) %>%
  summarise(sd = sd(lum, na.rm = TRUE),
            lum = mean(lum, na.rm = TRUE))


# Compute Background and Sensitivity --------------------------------------


raw_bg <- input_lum %>%
  filter(col == 12)


lum_bg_mean <- mean(raw_bg$lum)
lum_bg_sd <- sd(raw_bg$lum)

instrument_sensitivity <- lum_bg_sd * 3

# background subtract for O -----------------------------------------------

df_observed_values <- input_lum %>%
  mutate(lum = lum - lum_bg_mean,
         lum = if_else(lum < 0, 0, lum))


# -------------------------------------------------------------------------



calc_dis_matrix <- function(mat, calibrate_row = 5, calibrate_col = 5) {
  dimensions <- dim(mat)
  df <- make_empty_plate(dimensions[1], dimensions[2])
  df$dis = well_dis(df$row, df$col, calibrate_row = calibrate_row,
                    calibrate_col = calibrate_col)
  df$value <- matrix_to_vec(mat)

  df
}
matrix_to_df <- function(mat, calibrate_row = 5, calibrate_col = 5) {
  dimensions <- dim(mat)
  df <- make_empty_plate(dimensions[1], dimensions[2])
  df$value <- matrix_to_vec(mat)
  df
}

df_to_matrix <- function(data, column) {
  ncol = max(data$col)
  as.data.frame(data) %>%
    reorder_df_by_wells() %>%
    .[, column] %>%
    vec_to_matrix(ncol = ncol)
}

calc_dis_df <- function(data, calibrate_row = 5, calibrate_col = 5) {
  data$dis = well_dis(
    row = data$row,
    col = data$col,
    calibrate_row = calibrate_row,
    calibrate_col = calibrate_col
    )
  data
}

reorder_df_by_wells <- function(data) {
  data <- data[order(data$col), ]
  data <- data[order(data$row), ]
  data
}

make_empty_plate <- function(nrow = 8, ncol = 12) {
  plate <- expand.grid(
    col = seq(ncol),
    row = seq(nrow)
  )[, c("row", "col")]
  plate$well <- join_well(plate$row, plate$col)
  plate
}


matrix_to_vec <- function(mat) {
  lapply(seq(nrow(mat)), function(x) {
    mat[x, ]
  }) %>%
    do.call(`c`, .)
}

vec_to_matrix <- function(vec, ncol = 12) {
  lapply(seq(length(vec) / ncol), function(x) {
    vec[seq((x - 1) * ncol + 1, x * ncol)]
  }) %>%
    do.call(rbind, .)
}

deconvolute_vector <- function(vec, mat_decon) {
  t(solve(mat_decon) %*% as.vector(vec))
}

deconvolute_df_col <- function(data, mat_decon, column) {
  data <- data[order(data$col), ]
  data <- data[order(data$row), ]

  vec <- as.data.frame(data)[, column]
  data$deconvoluted <- deconvolute_vector(vec, mat_decon)
  data
}


# df_observed_values %>%
#   filter(cycle_nr == 100) %>%
#   deconvolute_df_col(matrix_D_best, "lum") %>%
#   rename(adjusted = deconvoluted)


frames_to_matrix <- function(data, value_col = "lum", time_col = "cycle_nr") {
  data <- as.data.frame(data)
  frame_numbers <- order(unique(data[, time_col]))
  data <- data[order(data$col), ]
  data <- data[order(data$row), ]

  lapply(frame_numbers, function(x) {
    data[data[, time_col] == x, value_col]
  }) %>%
     do.call(rbind, .) %>%
     as.matrix()
}

# mat_values <- frames_to_matrix(df_observed_values)

deconvolute_matrix_frames <- function(mat, mat_decon) {
  lapply(seq(nrow(mat_values)), function(x) {
    deconvolute_vector(
      vec = mat_values[x, ],
      mat_decon = mat_decon
        )
  }) %>%
    do.call(rbind, .) %>%
    as.matrix()
}

matrix_to_frames_df <- function(mat) {
  nframes <- nrow(mat)
  nwells <- ncol(mat)
  dimensions <- dim(mat)

  df <- make_empty_plate(n_rows_from_wells(nwells),
                         n_cols_from_wells(nwells))

  lapply(seq(nframes), function(x) {
    df$value <- mat[x, ]
    df$time <- x
    df
  }) %>%
  do.call(rbind, .)
}

n_cols_from_wells <- function(x) {
  switch(as.character(x),
         "24" = 6,
         "96" = 12,
         "384" = 24)
}

n_rows_from_wells <- function(x) {
  switch(as.character(x),
         "24" = 4,
         "96" = 8,
         "384" = 16)
}

# df_observed_values %>%
#   frames_to_matrix() %>%
#   deconvolute_matrix_frames(matrix_D_best) %>%
#   matrix_to_frames_df() %>%
#   ggplot(aes(time, value, group = well)) +
#   geom_line() +
#   scale_y_log10()





# dis_mat <- calc_dis_matrix(mat)

mean_dis_mat <- function(data) {
  unique_dis <- unique(data$dis)


  df <- lapply(seq(nrow(data)), function(x) {
    data_row <- data[x, ]
    dis <- data$dis[data$well == data$well[x]]
    mean_dis <- mean(data$value[data$dis == dis], na.rm = TRUE)

    if (is.na(data$value[x])) {
      data_row$value <- mean_dis
    }

    if (is.na(data_row$value)) {
      data_row$value <- 1
    }

    data_row
  }) %>%
    do.call(rbind, .)

  reorder_df_by_wells(df)
}

translate_df_wells <- function(data,
                               calibrate_row = 5,
                               calibrate_col = 5) {
  n_rows <- max(data$row)
  n_cols <- max(data$col)

  row_adjustment <- n_rows - calibrate_row
  col_adjustment <- n_cols - calibrate_col

  data$row <- data$row + row_adjustment
  data$col <- data$col + col_adjustment
  data$well <- join_well(row = data$row, col = data$col)

  empty_plate <- make_empty_plate(
    nrow = 2 * n_rows - 1,
    ncol = 2 * n_cols - 1
  )
  empty_plate$value <- NA


  lapply(seq(nrow(empty_plate)), function(x) {
    plate_row <- empty_plate[x, ]
    well <- plate_row$well

    if (well %in% data$well) {
      plate_row$value <- data$value[data$well == well]
    }



    plate_row
  }) %>%
    do.call(rbind, .) %>%
    reorder_df_by_wells()
}

df_observed_values %>%
  filter(cycle_nr == 120) %>%
  # print(n = 100)

  rename(value = lum) %>%
  # df_to_matrix("lum") %>%
  # matrix_to_df() %>%
  translate_df_wells() %>%
  calc_dis_df(calibrate_row = 8, calibrate_col = 12) %>%
  mean_dis_mat() %>%
  plot_wells(value)
#
# mat %>%
#   matrix_to_df() %>%
#   translate_df_wells() %>%
#   calc_dis_df() %>%
#   # mean_dis_mat() %>%
#   plot_wells(value)


test_fun <- function() {
  data <- expand.grid(1:10, 2:20)

  lapply(seq(nrow(data)), function(x) {
    data[x, 1] <<- data[x, 1] ^ 2
  })
  data
}
test_fun()


deconvolute_df_frames <- function(data, column, mat_decon) {
  deconvoluted_data <- data %>%
    frames_to_matrix("lum") %>%
    deconvolute_matrix_frames(mat_decon) %>%
    matrix_to_frames_df()

  data$adjusted <- deconvoluted_data$value
  data
}



looking_for_best <- TRUE

counter <- 0

working_df <- df_observed_values

matrix_log <- list()
lower_log <- c()

while (looking_for_best) {
  counter <- counter + 1
  bleed_df <- calc_bleed_df(working_df, time_cutoff = 50)
  mean_mat <- bleed_df %>%
    tibble_to_matrix(ratio_mean)
  sd_mat <- bleed_df %>%
    tibble_to_matrix(ratio_sd)
  # rand_mat <- matrix(rnorm(23 * 15, 0, 1), ncol = 23)
  # rand_mat <- rnorm(1, mean = 0, sd = 1)

  mean_rand_mat <- mean_mat + 0.1 * rnorm(1, 0, 1) * sd_mat

  matrix_D_working <- make_decon_matrix(mean_rand_mat)

  adjusted_df <- working_df %>%
    deconvolute_df_frames("lum", matrix_D_working)

  adjusted_values <- adjusted_df[adjusted_df$well != join_well(5, 5), ]$adjusted

  compared_values <- adjusted_values - instrument_sensitivity < 0

  perc_correct <- sum(compared_values) / max(adjusted_df$cycle_nr) / 95 * 100


  if (perc_correct < 100) {
    if (counter == 1) {

      matrix_D_best <- matrix_D_working %*% diag(96)
      matrix_log[[counter]] <- matrix_D_working
      old_perc_correct <- perc_correct


    } else {

      if (perc_correct > old_perc_correct) {

        matrix_D_best <- matrix_D_working %*% matrix_D_best
        lower_log <- c(lower_log, counter)
        old_perc_correct <- perc_correct

      }

      matrix_log[[counter]] <- matrix_D_working

    }
  } else {
    looking_for_best <- FALSE
    beepr::beep()
  }

  # working_df$lum <- ifelse(
  #   adjusted_df$adjusted < adjusted_df$lum | adjusted_df$adjusted <= 0,
  #   adjusted_df$adjusted,
  #   adjusted_df$lum
  # )
  working_df <- adjusted_df %>%
    mutate(
      lum = if_else(
        adjusted < lum | adjusted <= 0,
        adjusted,
        lum
      )
    ) %>%
    select(-adjusted)



  print(paste("Iteration", counter,
              ", percent:",
              round(perc_correct, 2)))
}





working_df %>%
  mutate(lum = if_else(lum < 0, 0, lum)) %>%
  ggplot(aes(cycle_nr, lum , group = well)) +
  geom_point(alpha = 0.2, colour = "gray50") +
  geom_line(alpha = 0.3, colour = "gray50") +
  geom_hline(yintercept = instrument_sensitivity, linetype = "dashed",
             colour = "black") +
  geom_line(
    data = filter(working_df, well == join_well(5, 5)),
    colour = "tomato",
    size = 1
  ) +

  scale_y_log10(limits = c(1e-1, NA)) +
  theme_classic() -> plt1

df_observed_values %>%
  ggplot(aes(cycle_nr, lum, group = well)) +
  geom_point(alpha = 0.2, colour = "gray50") +
  geom_line(alpha = 0.3, colour = "gray50") +
  geom_hline(yintercept = instrument_sensitivity, linetype = "dashed",
             colour = "black") +
  geom_line(
    data = filter(df_observed_values, well == join_well(5, 5)),
    colour = "tomato",
    size = 1
  ) +
  scale_y_log10(limits = c(1e-1, NA)) +
  theme_classic() -> plt2

df_observed_values %>%
  decon_frames(matrix_D_best) %>%
  transmute(well, cycle_nr, lum = adjusted) %>%
  ggplot(aes(cycle_nr, lum, group = well)) +
  geom_line(colour = "gray50", alpha = 0.3) +
  geom_point(colour = "gray50", alpha = 0.3) +
  scale_y_log10() +



  geom_hline(yintercept = instrument_sensitivity, linetype = "dashed",
             colour = "black") +
  geom_line(
    data = filter(df_observed_values, well == join_well(5, 5)),
    colour = "tomato",
    size = 1
  ) +
  scale_y_log10(limits = c(1e-1, NA)) +
  theme_classic() -> plt3


patchwork::wrap_plots(
  plt2 + labs(title = "Raw"),
  plt1 + labs(title = "Data After Optimisation"),
  plt3 + labs(title = "Raw Data Deconvoluted with Best Kernal D")
)

stop()


working_df %>%
  filter(well != join_well(5, 5)) %>%
  ggplot(aes(cycle_nr, lum)) +
  geom_line(aes(group = well)) +
  geom_hline(yintercept = instrument_sensitivity, colour = "tomato",
             linetype = "dashed")


read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  decon_frames(matrix_D) %>%
  filter(cycle_nr == 100) %>%
  plot_wells_comparison() +
  scale_fill_viridis_c(limits = c(0,NA))


read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  decon_frames(matrix_D_best) %>%
  filter(cycle_nr == 100) %>%
  plot_wells_comparison() +
  scale_fill_viridis_c(limits = c(0,NA)) +
  labs(title = "Deconvoluted with Best Kernal D Best")

target_wells <- c(join_well(1:8, 5), join_well(1:8, 7), join_well(c(2, 7), 6))

read_plate("inst/Xfiles/tecan/calibration/calTecan1.xlsx") %>%
  decon_frames(matrix_D_best) %>%
  filter(cycle_nr == 107) %>%
  mutate(adjusted = if_else(adjusted < instrument_sensitivity, 0, adjusted)) %>%
  plot_wells(adjusted, log10_fill = TRUE) +
  geom_text(aes(label = round(log10(adjusted), 2))) +
  scale_fill_viridis_c(limits = c(0, NA))

read_plate("inst/Xfiles/tecan/calibration/calTecan1.xlsx") %>%
  decon_frames(matrix_D_best) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()

read_plate("inst/Xfiles/tecan/tecanOFF1.xlsx") %>%
  decon_frames(matrix_D_best) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()

read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  decon_frames(matrix_D) %>%
  mutate(target = if_else(well %in% target_wells, "target", "background")) %>%
  pivot_longer(c(lum, adjusted)) %>%
  ggplot(aes(cycle_nr, value, colour = target, group = well)) +
  geom_line(size = 0.7) +
  facet_wrap(~name, ncol = 1) +
  scale_y_log10() +
  theme_classic() +
  geom_hline(yintercept = instrument_sensitivity, linetype = "dashed")

lapply(list.files("inst/Xfiles/tecan/",
                  pattern = "ON", full.names = TRUE),
       read_plate) %>%
  do.call(rbind, .) %>%
  group_by(cycle_nr, well, col, row) %>%
  summarise(
    sd = sd(lum, na.rm = TRUE),
    lum = mean(lum, na.rm = TRUE)
  ) %>%
  decon_frames(matrix_D_best) %>%
  mutate(target = if_else(well %in% target_wells, "target", "background")) %>%
  pivot_longer(c(lum, adjusted)) %>%
  ggplot(aes(cycle_nr, value, colour = target, group = well)) +
  geom_line() +
  geom_linerange(aes(ymin = value - sd, ymax = value + sd), ) +
  # geom_point() +
  scale_y_log10() +
  facet_wrap(~name, ncol = 1)

lapply(list.files("inst/Xfiles/tecan/",
                  pattern = "ON", full.names = TRUE),
       read_plate) %>%
  do.call(rbind, .) %>%
  group_by(cycle_nr, well, col, row) %>%
  summarise(
    sd = sd(lum, na.rm = TRUE),
    lum = mean(lum, na.rm = TRUE)
  ) %>%
  decon_frames(matrix_D) %>%
  mutate(target = if_else(well %in% target_wells, "target", "background")) %>%
  pivot_longer(c(lum, adjusted)) %>%
  mutate(name = factor(name, levels = c("lum", "adjusted"),
                       labels = c("Raw", "Deconvoluted"))) %>%
  filter(row == let_to_num("G")) %>%
  ggplot(aes(cycle_nr, value, colour = target, group = well)) +
  geom_line() +
  geom_hline(yintercept = instrument_sensitivity,
             linetype = "dashed",
             colour = "black") +
  geom_linerange(aes(ymin = value - sd, ymax = value + sd), alpha = 0.4) +
  scale_y_log10() +
  facet_wrap(~name, ncol = 2) +
  scale_x_continuous(expand = expansion()) +
  theme_linedraw() +
  theme(
    strip.background = element_rect(fill = "gray40")
  )

lapply(list.files("inst/Xfiles/tecan/",
                  pattern = "ON", full.names = TRUE),
       read_plate) %>%
  do.call(rbind, .) %>%
  group_by(cycle_nr, well, col, row) %>%
  summarise(
    sd = sd(lum, na.rm = TRUE),
    lum = mean(lum, na.rm = TRUE)
  ) %>%
  decon_frames(matrix_D_best) %>%
  filter(cycle_nr == 120) %>%
  plot_wells_comparison()




lapply(list.files("inst/Xfiles/tecan/",
                  pattern = "ON", full.names = TRUE),
       read_plate) %>%
  do.call(rbind, .) %>%
  group_by(cycle_nr, well, col, row) %>%
  summarise(
    sd = sd(lum, na.rm = TRUE),
    lum = mean(lum, na.rm = TRUE)
  ) %>%
  decon_frames(matrix_D_best) %>%
  mutate(target = if_else(well %in% target_wells, "Signal", "Background")) %>%
  pivot_longer(c(lum, adjusted)) %>%
  mutate(name = factor(name, levels = c("lum", "adjusted"),
                       labels = c("Raw", "Deconvoluted"))) %>%
  filter(row == let_to_num("G")) %>%
  ggplot(aes(cycle_nr, value, colour = target, group = well)) +
  geom_line() +
  geom_hline(yintercept = instrument_sensitivity,
             linetype = "dashed",
             colour = "black") +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd), alpha = 0.4) +
  scale_y_log10() +
  facet_wrap(~name, ncol = 2) +
  scale_x_continuous(expand = expansion()) +
  theme_linedraw() +
  theme(
    strip.background = element_rect(fill = "gray40")
  )


