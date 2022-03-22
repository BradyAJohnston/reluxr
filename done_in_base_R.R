library(reluxr)
library(tidyverse)


path <- "inst/Xfiles/tecan/calibration/calTecan1.xlsx"

starting_details <- readxl::read_excel(path) %>%
  select(1) %>%
  rename(col = 1)

starting_numbers <- starting_details %>%
  mutate(
    check = if_else(col == 1 & lead(col) == 2, TRUE, FALSE)
  ) %>%
  pull(check) %>%
    which()
ending_numbers <- starting_details %>%
  mutate(
    check = if_else(str_detect(col, "^\\d+$") & is.na(lead(col)), TRUE, FALSE)
  ) %>%
  pull(check) %>%
    which()



starting_details %>% print(n = 300)


starting_details[starting_numbers - 2, 1]


is_well_id <- function(x) {
  stringr::str_detect(x, "^\\w\\d{1,3}$")
}

readxl::read_excel(path, skip = starting_numbers[1]-1, n_max = ending_numbers[1] - starting_numbers[1] + 1) %>%
  janitor::clean_names() %>%
  rename_with(
    well_format,
    .cols = matches("^\\w\\d{1,3}$")
  ) %>%
  pivot_longer(
    cols = matches("^\\w\\d{1,3}$"),
    names_to = "well",
    values_to = "value"
  )


data_list <- lapply(seq_along(starting_numbers), function(x) {
  readxl::read_excel(path, skip = starting_numbers[x] - 1, n_max = ending_numbers[x] - starting_numbers[x] + 1) %>%
    janitor::clean_names() %>%
    rename_with(
      well_format,
      .cols = matches("^\\w\\d{1,3}$")
    ) %>%
    pivot_longer(
      cols = matches("^\\w\\d{1,3}$"),
      names_to = "well",
      values_to = starting_details[starting_numbers[x] - 2, 1] %>% pull()
    )
})



stringr::str_detect("a5", "^\\w\\d")


input_lum <- read_plate("inst/Xfiles/tecan/calibration/calTecan1.xlsx")


input_lum <-
  lapply(list.files("inst/Xfiles/tecan/calibration/", full.names = TRUE),
         read_plate) %>%
  do.call(rbind, .) %>%
  group_by(cycle_nr, well, col, row) %>%
  summarise(sd = sd(lum, na.rm = TRUE),
            lum = mean(lum, na.rm = TRUE),
            .groups = "keep")


# Compute Background and Sensitivity --------------------------------------


raw_bg <- input_lum %>%
  filter(well %in% well_join(1:8, 12))


lum_bg_mean <- mean(raw_bg$lum)
lum_bg_sd <- sd(raw_bg$lum)

instrument_sensitivity <- lum_bg_sd * 3

# background subtract for O -----------------------------------------------

df_observed_values <- input_lum %>%
  mutate(lum = lum - lum_bg_mean,
         lum = if_else(lum < 0, 0, lum))


# -------------------------------------------------------------------------
matrix_D_best <- calc_matrix_D_best(
  data = df_observed_values,
  col_time = cycle_nr,
  col_value = lum,
  instrument_sensitivity = instrument_sensitivity
  )


read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  deconvolute_df_frames(lum, matrix_D_best) %>%
  filter(cycle_nr == 100) %>%
  plot_wells_comparison() +
  scale_fill_viridis_c(limits = c(0,NA))


read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  deconvolute_df_frames(lum, matrix_D_best) %>%
  filter(cycle_nr == 100) %>%
  plot_wells_comparison() +
  scale_fill_viridis_c(limits = c(0,NA)) +
  labs(title = "Deconvoluted with Best Kernal D Best")


read_plate("inst/Xfiles/tecan/tecanON1.xlsx") %>%
  deconvolute_df_frames(lum, matrix_D_best) %>%
  filter(cycle_nr == 100) %>%
  pivot_longer(c(lum, adjusted), values_to = "lum") %>%

  ggplot(aes(col, row)) +
  geom_tile(
    aes(fill = log10(lum),
        alpha = sqrt(log10(lum))
        ),
    # colour = "gray80",
    width = 0.9,
    height = 0.9
    ) +
  guides(alpha = "none") +
  # scale_alpha_continuous(
  #
  # ) +
  scale_fill_viridis_c(
    option = "A",
    na.value = alpha("black", 0.1),
    direction = 1,
    limits = c(1, NA)
    ) +
  coord_fixed() +
  scale_x_continuous(
    expand = expansion(),
    breaks = 1:100,
    position = "top"
  ) +
  scale_y_reverse(
    expand = expansion(),
    breaks = 1:12
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray"),
    panel.border = element_rect(colour = "gray20", fill = NA,
                                size = 1)
  ) +
  facet_wrap(~name, strip.position = "bottom")

target_wells <- c(well_join(1:8, 5), well_join(1:8, 7), well_join(c(2, 7), 6))

read_plate("inst/Xfiles/tecan/calibration/calTecan1.xlsx") %>%
  deconvolute_df_frames(lum, matrix_D_best) %>%
  filter(cycle_nr == 107) %>%
  mutate(adjusted = if_else(adjusted < instrument_sensitivity, 0, adjusted)) %>%
  plot_wells(adjusted, log10_fill = TRUE) +
  geom_text(aes(label = round(log10(adjusted), 2))) +
  scale_fill_viridis_c(limits = c(0, NA))

read_plate("inst/Xfiles/tecan/calibration/calTecan1.xlsx") %>%
  deconvolute_df_frames(lum, matrix_D_best) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()

read_plate("inst/Xfiles/tecan/tecanOFF1.xlsx") %>%
  deconvolute_df_frames(lum, matrix_D_best) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()



lapply(list.files("inst/Xfiles/tecan/",
                  pattern = "ON", full.names = TRUE),
       read_plate) %>%
  do.call(rbind, .) %>%
  group_by(cycle_nr, well, col, row) %>%
  summarise(
    sd = sd(lum, na.rm = TRUE),
    lum = mean(lum, na.rm = TRUE)
  ) %>%
  deconvolute_df_frames(lum, matrix_D_best) %>%
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
  deconvolute_df_frames(lum, matrix_D_best) %>%
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
  deconvolute_df_frames(lum, matrix_D_best) %>%
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


