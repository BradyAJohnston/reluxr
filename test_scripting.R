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



# translate to the middle of a grid, selecting cycle_nr 100
translated_df <- df_lum %>%
  # filter(cycle_nr == 100) %>%
  select(cycle_nr, well, lum) %>%
  # left_join(
  #   y = df_od %>%
  #     # filter(cycle_nr == 100) %>%
  #     select(well, od)
  # ) %>%
  mutate(
    row = let_to_num(well_to_let(well)) + 3,
    col = well_to_num(well) + 7,
    well = join_well(LETTERS[row], col)
  )

df_mean_sd <- translated_df %>%
  group_by(col, row, well) %>%
    summarise(mean = mean(lum, na.rm = TRUE),
              sd = sd(lum, na.rm = TRUE)) %>%
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

df_mean %>%
  well_plot(row, col, mean)


bg_lum <- df_lum  %>%
  filter(col == 12) %>%
  summarise(mean = mean(lum, na.rm = TRUE),
            sd = sd(lum, na.rm = TRUE))


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


matrix_mean <- df_filled %>%
  matrix_from_tibble(mean)

matrix_sd <- df_filled %>%
  matrix_from_tibble(sd)

matrix_rand <- df_filled %>%
  matrix_from_tibble(rand)


matrix_e <- matrix_mean + matrix_sd * matrix_rand

toe


portes::ToeplitzBlock(cbind(rnorm(100), rnorm(100)), 4)

cbind(rnorm(100), rnorm(100))


# matrix E{8,12}

make_toe <- function(mat, )

toeplitz(matrix_e[1, 1:11])




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
