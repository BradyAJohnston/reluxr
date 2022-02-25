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
  filter(cycle_nr == 100) %>%
  select(well, lum) %>%
  left_join(
    y = df_od %>%
      filter(cycle_nr == 100) %>%
      select(well, od)
  ) %>%
  mutate(
    row = let_to_num(well_to_let(well)) + 3,
    col = well_to_num(well) + 7,
    well = join_well(LETTERS[row], col)
  )

#
df <- expand.grid(col = 1:23, row = 1:15) %>%
  mutate(well = join_well(row, col)) %>%
  as_tibble() %>%
  left_join(translated_df)

df %>%
  well_plot(row, col, lum) +
  overlay_plate() +
  labs(title = "Original plate in new layout")


# Create Averaged Values --------------------------------------------------

df <- df %>%
  mutate(
    dis = sqrt((row - 8)^ 2 + (col - 12) ^ 2)
  ) %>%
  group_by(dis) %>%
  summarise(well, row, col, lum, od,
            lum_av = mean(lum, na.rm = TRUE),
            od_av = mean(od, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    lum = if_else(is.na(lum), lum_av, lum),
    od = if_else(is.na(od), od_av, od)
  )


df %>%
  mutate(lum = if_else(is.na(lum), 1e5, lum)) %>%
  well_plot(row, col, lum) +
  labs(title = "Calibration plate with average values") +
  overlay_plate()

