library(tidyverse)
library(reluxr)
# source("functions.R")


# read_plate <- function(path) {
#   readxl::read_excel(
#     path = path,
#     skip = 168
#   ) %>%
#     pivot_longer(
#       cols = matches("\\w\\d{1,2}"),
#       names_to = "well",
#       values_to = "lum"
#     ) %>%
#     janitor::clean_names() %>%
#     drop_na() %>%
#     mutate(
#       row = well_to_let(well) %>% let_to_num(),
#       col = well_to_num(well),
#       well = join_well(LETTERS[row], col),
#       lum = as.numeric(lum),
#       time_s = as.numeric(time_s),
#       cycle_nr = as.numeric(cycle_nr)
#     )
# }

input_lum <- read_plate("inst/Xfiles/tecan/calibration/calTecan2.xlsx")





# computer background b and sensitivity S ---------------------------------

raw_bg <- input_lum %>%
  filter(col == 12)


lum_bg_mean <- mean(raw_bg$lum)
lum_bg_sd <- sd(raw_bg$lum)

instrument_sensitivity <- lum_bg_sd * 3





# background subtract for O -----------------------------------------------

df_observed_values <- input_lum %>%
  mutate(lum = lum - lum_bg_mean)



# Skipping the averaging of replicates ------------------------------------



# compute bleed-through factor array --------------------------------------

calc_bleed_df <- function(data, time_cutoff = 30) {
  time_averaged_df <- data %>%
    group_by(cycle_nr) %>%
    mutate(
      ratio = lum / max(lum)
    ) %>%
    ungroup() %>%
    filter(cycle_nr > time_cutoff) %>%
    group_by(well, row, col) %>%
    summarise(
      ratio_mean = mean(ratio, na.rm = TRUE),
      ratio_sd   = sd(ratio, na.rm = TRUE)
    ) %>%
    ungroup()

  background_ratios <- time_averaged_df %>%
    filter(col == 12) %>%
    summarise(
      mean = mean(if_else(ratio_mean < 0, 0, ratio_mean), na.rm = TRUE),
      sd = mean(ratio_sd, na.rm = TRUE)
    )

  # print(background_ratios)

  time_averaged_df %>%
    create_extended_tibble(
      lum_bg_ratio_mean = background_ratios$mean,
      lum_bg_ratio_sd = background_ratios$sd
    )
}

bleed_through_df <- calc_bleed_df(df_observed_values)

# time_averaged_df <- df_observed_values %>%
#   group_by(cycle_nr) %>%
#   mutate(
#     ratio = lum / max(lum)
#   ) %>%
#   ungroup() %>%
#   filter(cycle_nr > 30) %>%
#   group_by(well, row, col) %>%
#   summarise(
#     ratio_mean = mean(ratio),
#     ratio_sd = sd(ratio)
#   ) %>%
#   ungroup()
#
# lum_ratio_backgrounds <- time_averaged_df %>%
#   filter(col == 12) %>%
#   summarise(
#     mean = mean(if_else(ratio_mean < 0, 0, ratio_mean), na.rm = TRUE),
#     sd = mean(ratio_sd, na.rm = TRUE)
#   )

# lum_ratio_backgrounds
#
# bleed_through_df <- time_averaged_df %>%
#   create_extended_tibble(
#     lum_bg_ratio_mean = lum_ratio_backgrounds$mean,
#     lum_bg_ratio_sd = lum_ratio_backgrounds$sd
#   )


# arrange E in kernal deconvolution matrix --------------------------------


matrix_decon_D <- bleed_through_df %>%
  tibble_to_matrix(ratio_mean) %>%
  make_decon_matrix()


# Deconvolute Data Average ------------------------------------------------





deconvolute_data(
  filter(df_observed_values, cycle_nr == 100),
  decon_mat = matrix_decon_D,
  lum
) %>%
  plot_wells_comparison()



read_plate("inst/Xfiles/tecan/tecanOFF3.xlsx") %>%
  filter(cycle_nr == 100) %>%
  deconvolute_data(matrix_decon_D, lum) %>%
  plot_wells_comparison()

read_plate("inst/xfiles/tecan/tecanON2.xlsx") %>%
  group_by(cycle_nr) %>%
  nest() %>%
  mutate(
    adjusted = purrr::map(data, ~deconvolute_data(.,
                          decon_mat = matrix_decon_D,
                          col = lum))
  ) %>%
  select(cycle_nr, adjusted) %>%
  unnest(adjusted) %>%
  # deconvolute_data(matrix_decon_D, lum) %>%
  pivot_longer(c(lum, adjusted)) %>%
  ggplot(aes(cycle_nr, value, colour = name, group = well)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~name, ncol = 1)


read_plate("inst/xfiles/tecan/tecanON2.xlsx") %>%
  filter(cycle_nr == 100) %>%
  deconvolute_data(matrix_decon_D, lum) %>%
  plot_wells_comparison()

# Iterative Deconvolution -------------------------------------------------


# decon_frames <- function(data, decon_mat) {
#   data %>%
#     group_by(cycle_nr) %>%
#       nest() %>%
#       mutate(
#         adjusted = purrr::map(
#           .x = data,
#           .f = ~ deconvolute_data(
#             data = .,
#             decon_mat = decon_mat,
#             col = lum
#             )
#         )
#       ) %>%
#       select(cycle_nr, adjusted) %>%
#       unnest(adjusted)
# }


df_observed_values %>%
  decon_frames(matrix_decon_D) %>%
  filter(cycle_nr == 100) %>%
  plot_wells_comparison()

df_observed_values %>%
  decon_frames(matrix_decon_D) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()

bleed_through_df

bleed_through_df %>%
  mutate(ratio_mean = if_else(ratio_mean < 0, 1e-10, ratio_mean)) %>%
  plot_wells(ratio_mean, log10_fill = TRUE) +
  geom_text(aes(label = round(ratio_mean, 10)))

# working_df <- filter(df_observed_values, cycle_nr == 100)

looking_for_best <- TRUE
counter <- 0

working_df <- df_observed_values

stop()
matrix_D_storage <- list()
while (looking_for_best) {
  counter <- counter + 1

  extended_matrix <- calc_bleed_df(working_df)

  matrix_D <- extended_matrix %>%
    mutate(
      adjusted = ratio_mean + rnorm(nrow(.)) * ratio_sd
    ) %>%
    tibble_to_matrix(adjusted) %>%
    make_decon_matrix()

  adjusted_frames <- working_df %>%
    decon_frames(matrix_D)

  comparison <- adjusted_frames %>%
    filter(well != join_well(5, 5)) %>%
    mutate(
      compare = adjusted - instrument_sensitivity < 0
    ) %>%
    pull(compare)

  working_df <- adjusted_frames %>%
    mutate(
      lum = if_else(adjusted < lum | adjusted < instrument_sensitivity,
                    adjusted,
                    lum
                    )
    ) %>%
    select(-adjusted)

  if (FALSE %in% comparison) {
    if (counter == 1) {
      matrix_D_best <- matrix_D %*% diag(nrow = 96)
      matrix_D_previous <- matrix_D
      matrix_D_storage[[counter]] <- matrix_D_best
    } else {
      matrix_D_best <- matrix_D %*% matrix_D_previous
      matrix_D_previous <- matrix_D
      matrix_D_storage[[counter]] <- matrix_D_best
    }
  } else {
    looking_for_best <- FALSE
  }

  print(df_observed_values %>%
    filter(cycle_nr == 100) %>%
    deconvolute_data(matrix_D_best, lum) %>%
    plot_wells_comparison() +
    labs(title = str_glue("Iteration {counter}")))
}




matrix_D_storage %>%
  lapply(., function(x) {
    df_observed_values %>%
      filter(cycle_nr == 100) %>%
      deconvolute_data(decon_mat = x, lum) %>%
      mutate()
  })

working_df %>%
  filter(cycle_nr == 100) %>%
  plot_wells(lum)

working_df %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()



df_observed_values %>%
  decon_frames(matrix_D_best) %>%
  filter(cycle_nr == 100) %>%
  plot_wells_comparison()
  # pivot_longer(c(lum, adjusted)) %>%
  # plot_wells_time()






# while (counter < 100) {
#   counter <- counter + 1
#
#
#   matrix_D <- random_extended_matrix(bleed_through_df) %>%
#     make_decon_matrix()
#
#   if (counter == 1) {
#     best_matrix_D <- matrix_D
#   }
#
#   if (counter == 1) {
#     working_df$adjusted <- working_df$lum
#   }
#
#   working_df <- deconvolute_data(working_df, matrix_D, adjusted)
#
#   comparison_df <- working_df %>%
#     filter(well != "E05") %>%
#     mutate(
#       lower = adjusted - instrument_sensitivity <= 0
#     )
#
#
#
#
#   print(sum(comparison_df$lower))
#
#   if (!(FALSE %in% comparison_df$lower)) {
#     best_matrix_D <<- matrix_D
#     looking_for_best <- FALSE
#   } else {
#     if (counter == 1) {
#       best_matrix_D <- matrix_D
#     } else {
#       best_matrix_D <- matrix_D %*% best_matrix_D
#     }
#   }
#   print(image(best_matrix_D))
# }

working_df %>%
  plot_wells_comparison()


decon_series <- function(data, decon_mat, col) {
  data %>%
    group_by(cycle_nr) %>%
    nest() %>%
    mutate(
      adjusted = purrr::map(
        .x = data,
        .f = ~reluxr::deconvolute_data(
          data = .x,
          decon_mat = decon_mat,
          col = col
        )
      )
    ) %>%
    select(cycle_nr, adjusted) %>%
    unnest(adjusted)
}

df_observed_values %>%
  group_by(cycle_nr) %>%
  decon_series(best_matrix_D, lum)

  nest() %>%
  mutate(
    adjusted = purrr::map(data, deconvolute_data,
      decon_mat = best_matrix_D,
      col = lum
    )
  ) %>%
  select(cycle_nr, adjusted) %>%
  unnest(adjusted) %>%
  pivot_longer(c(lum, adjusted)) %>%
  plot_wells_time()
