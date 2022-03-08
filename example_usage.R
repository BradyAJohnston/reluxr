library(tidyverse)
library(reluxr)

input_lum <- read_plate("inst/Xfiles/tecan/calibration/calTecan1.xlsx")

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


# Compute Extended Matrix -------------------------------------------------

matrix_D <- calc_bleed_df(df_observed_values) %>%
  tibble_to_matrix(ratio_mean) %>%
  make_decon_matrix()


df_observed_values %>%
  decon_frames(matrix_D) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()

df_observed_values %>%
  filter(cycle_nr == 100) %>%
  decon_frames(matrix_D) %>%
  plot_wells_comparison()

df_observed_values %>%
  decon_frames(matrix_D) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()

read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  decon_frames(matrix_D) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()

read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  decon_frames(matrix_D) %>%
  filter(cycle_nr == 100) %>%
  plot_wells(adjusted, log10_fill = TRUE) +
  geom_text(aes(label = round(log10(adjusted), 2)))


# iterative approach ------------------------------------------------------

looking_for_best <- TRUE

counter <- 0

working_df <- df_observed_values

while (looking_for_best) {
  counter <- counter + 1

  bleed_df <- calc_bleed_df(working_df, time_cutoff = 40)

  mean_mat <- bleed_df %>%
    tibble_to_matrix(ratio_mean)

  sd_mat <- bleed_df %>%
    tibble_to_matrix(ratio_sd)

  rand_mat <- matrix(rnorm(n = 23 * 15, mean = 0, sd = 1), ncol = 23)

  mean_rand_mat <- mean_mat + rand_mat * sd_mat

  matrix_D_working <- make_decon_matrix(mean_rand_mat)

  df_adjusted <- working_df %>%
    decon_frames(matrix_D_working)

  df_compared <- df_adjusted %>%
    filter(well != join_well(5, 5)) %>%
    mutate(
      compare = adjusted - instrument_sensitivity < 0
    )

  if (FALSE %in% df_compared$compare) {
    if (counter == 1) {
      matrix_D_best <- matrix_D_working %*% diag(96)
      matrix_D_prev <- matrix_D_working
    } else {
      # matrix_D_best <- matrix_D_working %*% matrix_D_best
      matrix_D_best <- matrix_D_working %*% matrix_D_prev
      matrix_D_prev <- matrix_D_working
    }
  } else {
    looking_for_best <- FALSE
  }

  working_df <- df_adjusted %>%
    mutate(
      lum = case_when(
        adjusted < instrument_sensitivity ~ adjusted,
        adjusted < lum                    ~ adjusted,
        TRUE                              ~ lum
      )
    )

  # print(
  #   df_observed_values %>%
  #     filter(cycle_nr == 100) %>%
  #     decon_frames(matrix_D_best) %>%
  #     plot_wells_comparison() +
  #     labs(title = str_glue("Iteration {counter}")) +
  #     scale_fill_viridis_c(limits = c(1, NA))
  # )
  #
  print(
    working_df %>%
      filter(cycle_nr == 100) %>%
      plot_wells_comparison() +
      labs(title = str_glue("Iteration {counter}")) +
      scale_fill_viridis_c(limits = c(0, NA))
  )

  # print(
  #   working_df %>%
  #     decon_frames(matrix_D_best) %>%
  #     pivot_longer(c(lum, adjusted)) %>%
  #     plot_wells_time(instrument_sensitivity) +
  #     labs(title = str_glue("Iteration {counter}"))
  # )
}
