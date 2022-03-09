library(tidyverse)
library(reluxr)

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

matrix_log <- list()

while (looking_for_best) {
  counter <- counter + 1

  bleed_df <- calc_bleed_df(working_df, time_cutoff = 60)

  mean_mat <- bleed_df %>%
    tibble_to_matrix(ratio_mean)

  sd_mat <- bleed_df %>%
    tibble_to_matrix(ratio_sd)

  rand_mat <- matrix(rnorm(n = 23 * 15, mean = 0, sd = 1), ncol = 23)

  mean_rand_mat <- mean_mat + 0.1 * rand_mat * sd_mat

  matrix_D_working <- make_decon_matrix(mean_rand_mat)

  df_adjusted <- working_df %>%
    decon_frames(matrix_D_working)

  df_compared <- df_adjusted %>%
    filter(well != join_well(5, 5)) %>%
    mutate(
      compare = adjusted - instrument_sensitivity <= 0
    )

  perc_correct <- sum(df_compared$compare) / max(working_df$cycle_nr) / 95 * 100

  if (FALSE %in% df_compared$compare) {
    if (counter == 1) {
      matrix_D_best <- matrix_D_working %*% diag(96)

      matrix_log[[counter]] <- matrix_D_working
      old_perc_correct <- perc_correct
    } else {
      if (perc_correct > old_perc_correct) {
        matrix_D_best <- matrix_D_working %*% matrix_D_best

        matrix_log[[counter]] <- matrix_D_working

        old_perc_correct <- perc_correct
      }
    }
  } else {
    looking_for_best <- FALSE
    beepr::beep()

    # matrix_D_best <- purrr::reduce(matrix_log, `%*%`)
  }

  working_df <- df_adjusted %>%
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
              round(old_perc_correct, 2)))

}

stop()


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

test_data <- read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  filter(cycle_nr == 100)

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

