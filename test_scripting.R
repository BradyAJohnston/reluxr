library(tidyverse)


# Setup Functions ---------------------------------------------------------


# convert letter to it's numeric representation
let_to_num <- function(x) {
  x <- stringr::str_to_upper(x)
  as.numeric(factor(x), levels = LETTERS)
}

# pad out a number to two digits, returned as a string
num_to_2d <- function(x, width = 2) {
  stringr::str_pad(x, width = width, side = "left", pad = "0"
  )
}

# given a column number and a row (either number or letter) return the
# 3-character well that is joined from the two
join_well <- function(row, col) {
  if (is.numeric(row)) {
    row <- LETTERS[row]
  }
  stringr::str_glue("{row}{num_to_2d(col)}") %>%
    as.character()
}

well_to_num <- function(x) {
  stringr::str_extract(x, "\\d+$") %>%
    as.numeric()
}

well_to_let <- function(x) {
  stringr::str_extract(x, "^\\w")
}

well_dis <- function (row, col, blank_row = 5, blank_col = 5) {
  sqrt((row - blank_row) ^ 2 + (col - blank_col) ^ 2)
}

predict_background <- function(dis,
                               height = 3,
                               well_spacing = 25) {
  (height ^ 3) / (
    height ^ 2 + (dis * well_spacing) ^ 2
  ) ^ (3/2)
}

well_plot <- function(data, row, col, fill, log10_fill = TRUE) {
  plt <- data %>%
  ggplot(aes({{ col }}, {{ row }}))

  if (log10_fill) {
    plt <- plt +
      geom_tile(
        aes(fill = log10( {{ fill }})),
        alpha = 0.9,
        colour = "gray30"
        )

  } else {
    plt <- plt +
      geom_tile(
        aes(fill = {{ fill }}),
        alpha = 0.9,
        colour = "gray30"
        )
  }

  plt +
    # geom_tile(alpha = 0.9, colour = "gray30") +
    scale_fill_viridis_c(breaks = scales::pretty_breaks())  +
    scale_x_continuous(
      name = NULL,
      expand = expansion(),
      breaks = 1:23,
      position = "top"
    ) +
    scale_y_reverse(
      name = NULL,
      expand = expansion(),
      breaks = 1:15,
      labels = LETTERS[1:15]
    ) +
    theme_linedraw() +
    theme(
      aspect.ratio = 15 / 23,
      panel.grid = element_blank(),
      axis.text.y = element_text(hjust = 0.5),
      legend.title = element_text(size = 10),
      axis.ticks = element_blank()
    )
}

overlay_plate <- function(colour = "gray10",
                          size = 1,
                          fill = "NA") {
  annotate(
    geom = "rect",
    ymin = 3.5,
    ymax = 3.5 + 8,
    xmin = 7.5,
    xmax = 7.5 + 12,
    colour = colour,
    size = size,
    fill = fill
  )
}



# Read in Calibration Data ------------------------------------------------

df_od <-
  readxl::read_excel(
    "inst/Xfiles/tecan/calibration/calTecan1.xlsx",
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
    row = well_to_let(well) %>% let_to_num(),
    col = well_to_num(well),
    well = join_well(LETTERS[row], col)
  )

df_lum <-
  readxl::read_excel("inst/Xfiles/tecan/calibration/calTecan1.xlsx",
                     skip = 168) %>%
  pivot_longer(
    cols = matches("\\w\\d{1,2}"),
    names_to = "well",
    values_to = "lum"
  ) %>%
  janitor::clean_names() %>%
  mutate(
    row = well_to_let(well) %>% let_to_num(),
    col = well_to_num(well),
    well = join_well(LETTERS[row], col),
    lum = as.numeric(lum)
  )


vec_lum <- df_lum %>%
  filter(cycle_nr == 120) %>%
  select(row, col, lum) %>%
  arrange(row, col) %>%
  pull(lum)


# Prediction of Values ----------------------------------------------------

df_lum %>%
  filter(
    cycle_nr == 50
  ) %>%
  mutate(
    dis = well_dis(row, col),
    pred_back = predict_background(dis)
  ) %>%
  ggplot(aes(dis)) +
  geom_point(aes(y = lum / max(lum))) +
  geom_smooth(aes(y = pred_back), se = FALSE, colour = "tomato") +
  scale_y_log10() +
  theme_linedraw() +
  theme(
    panel.grid.minor = element_blank()
  )



####################
df_lum <- df_lum %>%
  group_by(cycle_nr) %>%
  mutate(
    raw_lum = lum,
    lum = lum / max(lum)
    ) %>%
  ungroup()
#################

# translate to the middle of a grid, selecting cycle_nr 100
translated_df <- df_lum %>%
  select(cycle_nr, well, lum) %>%
  mutate(
    row = let_to_num(well_to_let(well)) + 3,
    col = well_to_num(well) + 7,
    well = join_well(LETTERS[row], col)
  )



df_mean_sd <- translated_df %>%
  group_by(col, row, well) %>%
    summarise(
      mean = mean(lum, na.rm = TRUE),
      sd = sd(lum, na.rm = TRUE)
      ) %>%
    ungroup() %>%
    mutate(mean.norm = mean / max(mean))




df <- expand.grid(col = 1:23, row = 1:15) %>%
  mutate(well = join_well(row, col)) %>%
  as_tibble() %>%
  left_join(df_mean_sd)

df %>%
  well_plot(row, col, mean) +
  overlay_plate() +
  labs(title = "Original plate in new layout")


# Create Averaged Values --------------------------------------------------

df <- df %>%
  mutate(
    dis = well_dis(row, col, blank_row = 8, blank_col = 12)
  )

df_mean <- df %>%
  group_by(dis) %>%
  summarise(well, row, col, mean, sd,
            mean_av = mean(mean, na.rm = TRUE),
            sd_av = mean(sd, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    mean = if_else(is.na(mean), mean_av, mean),
    sd = if_else(is.na(sd), sd_av, sd)
  )



bg_lum <- df_lum  %>%
  filter(col == 12) %>%
  summarise(mean = mean(lum, na.rm = TRUE),
            sd = sd(lum, na.rm = TRUE))

instrument_sensitivity <- bg_lum$sd * 3



df_filled <- df_mean %>%
  mutate(
    mean = case_when(
      is.na(mean) ~ bg_lum$mean,
      mean <= 0 ~ bg_lum$mean,
      TRUE ~ mean
    ),
    sd = case_when(
      is.na(sd) ~ bg_lum$sd,
      sd <= 0 ~ bg_lum$sd,
      TRUE ~ sd
    )
  )

df_filled <- df_filled %>%
  mutate(
    rand = rnorm(nrow(.), mean = 0, sd = 1)
  )



df_filled %>%
  well_plot(row, col, mean, log10_fill = TRUE) +
  labs(title = "Calibration plate with average values") +
  overlay_plate()

# Decon matrix ------------------------------------------------------------

matrix_from_tibble <- function(data, value) {
  data %>%
    select(row, col , {{ value }}) %>%
    arrange(row, col) %>%
    pivot_wider(values_from = {{ value }}, names_from = col) %>%
    column_to_rownames("row") %>%
    as.matrix()
}

tibble_from_vec <- function(vec) {
  vec %>%
    as_tibble() %>%
    mutate(
      col = rep(1:12, 8),
      row = rep(1:8, each = 12)
    )
}


matrix_mean <- df_filled %>%
  matrix_from_tibble(mean)



matrix_sd <- df_filled %>%
  matrix_from_tibble(sd)

matrix_rand <- df_filled %>%
  matrix_from_tibble(rand)


matrix_e <- matrix_mean + matrix_rand * matrix_sd



decon_col <- function(mat, rows, col = 12) {
  lapply(rows, function(x) {
    mat[x, seq(col, col + 11)] %>%
      toeplitz()
  }) %>%
    do.call(rbind, .)
}

decon_col(matrix_e, 8:15) %>%
  as_tibble()



matrix_D <- lapply(8:1, function(x) {
  decon_col(matrix_e, rows = seq(x, x + 7))
}) %>%
  do.call(cbind, .)


make_decon_matrix <- function(mat, sample_row = 8, sample_col = 12) {
  lapply(seq(sample_row, 1), function(x) {
    decon_col(mat, rows = seq(x, x + max(sample_row) - 1))
  }) %>%
    do.call(cbind, .)
}




df_adjusted <- solve(make_decon_matrix(matrix_e)) %*% vec_lum %>%
  as_tibble() %>%
  mutate(
    col = rep(c(1:12), length.out = 96),
    row = rep(1:8, each = 12)
    )


df_adjusted %>%

  well_plot(row, col, V1, log10_fill = FALSE) +
  geom_text(aes(label = round(V1, 3)), colour = "black")


compared_df <- df_adjusted %>%
  filter(row != 8 && col != 12) %>%
  mutate(
    diff_below_zero = V1 - instrument_sensitivity <= 0
  )

compared_df$diff_below_zero == TRUE


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


df_lum %>%
  drop_na() %>%
  group_by(cycle_nr) %>%
  arrange(row, col) %>%
  nest() %>%
  mutate(
    adjusted_data = purrr::map(data, ~deconvolute_data(
      data = .x,
      decon_mat = make_decon_matrix(matrix_mean)
      , raw_lum)
      )
  ) %>%
  select(cycle_nr, adjusted_data) %>%
  unnest(adjusted_data) %>%
  pivot_longer(cols = c(adjusted, raw_lum)) %>%
  mutate(
    cycle_nr = str_extract(cycle_nr, "\\d+") %>% as.numeric()
  ) %>%
  filter(cycle_nr == 100) %>%
  well_plot(row, col, value) +
  facet_grid(cols = vars(name))

stop()

  ggplot(aes(cycle_nr, value, colour = name, group = well)) +
  geom_line() +
  facet_wrap(~name) +
  theme_classic() +
  scale_y_log10()


for (i in 1:10) {

  matrix_e <- matrix_mean +       # time-averaged bleed through matrix
    matrix(rnorm(96), nrow = 8) * # randomised matrix each time
    matrix_sd                     # sd matrix from time average bleed through

  decon_matrix <- make_decon_matrix(matrix_e)

  deconvolute_data(df_mean, decon_mat = decon_matrix, col = mean)
}



