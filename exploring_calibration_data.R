library(tidyverse)

df_od <- readxl::read_excel("inst/Xfiles/tecan/calibration/calTecan1.xlsx", skip = 45, n_max = 122) %>%
  pivot_longer(cols = matches("\\w\\d{1,2}"), names_to = "well", values_to = "OD") %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(
    row = str_extract(well, "\\w"),
    col = str_extract(well, "\\d{1,2}") %>% as.numeric(),
    well = paste0(row, str_pad(col, width = 2, side = "left", pad = "0")),
    something = as.numeric(od)
  )


df_lum <- readxl::read_excel("inst/Xfiles/tecan/calibration/calTecan1.xlsx", skip = 168) %>%
  pivot_longer(cols = matches("\\w\\d{1,2}"), names_to = "well", values_to = "lum") %>%
  janitor::clean_names() %>%
  mutate(
    row = str_extract(well, "\\w"),
    col = str_extract(well, "\\d{1,2}") %>% as.numeric(),
    well = paste0(row, str_pad(col, width = 2, side = "left", pad = "0")),
    lum = as.numeric(lum)
  )





df_od %>%
  mutate(
    row = str_extract(well, "\\w"),
    col = str_extract(well, "\\d{1,2}") %>% as.numeric(),
    od = as.numeric(od)
  ) %>%
  filter(cycle_nr == 1) %>%

  ggplot(aes(col, row)) +
  geom_tile(aes(fill = od)) +
  geom_text(aes(label = round(od, 2)), colour = "white")

df_lum %>%
  filter(cycle_nr == 100) %>%
  mutate(row = factor(row, levels = rev(LETTERS[1:8]))) %>%

  ggplot(aes(col, row)) +
  geom_tile(aes(fill = log10(lum)), alpha = 0.9, colour = "gray20") +
  scale_fill_viridis_c(option = "D") +
  theme_linedraw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 8/12) +
  scale_x_continuous(breaks = 1:12, expand = expansion()) +
  scale_y_discrete(expand = expansion())




paste_well <- function(row, col) {
  paste0(
    row,
    str_pad(col, width = 2, side = "left", pad = "0")
  )
}

df <- df_lum %>%
  dplyr::filter(cycle_nr == 100) %>%
  mutate(
    col = col + 7,
    row = LETTERS[as.numeric(factor(row, levels = LETTERS)) + 3],
    well = paste_well(row, col),
  )



new_grid <- expand.grid(row = LETTERS[1:15],
                        col = str_pad(1:23,
                                      side = "left",
                                      width = 2,
                                      pad = "0"))

joined <- new_grid %>%
  mutate(
    well = paste(row, col, sep = "")
  ) %>%
  left_join(df, by = c("well" = "well")) %>%
  as_tibble()


distance <- joined %>%
  select(
    row = row.x,
    col = col.x,
    well,
    cycle_nr,
    time_s,
    temp_c,
    # row.num,
    lum
  ) %>%
  mutate(
    row.num = as.numeric(factor(row, levels = LETTERS)),
    dis = sqrt((row.num - 8)^ 2 + (as.numeric(col) - 12) ^ 2)
  )
distance %>%
  group_by(dis) %>%
  summarise(well, lum, mean_lum = mean(lum, na.rm = TRUE)) %>%
  left_join(distance) %>%
  mutate(
    col = as.numeric(col),
    # row.num = as.numeric(factor(row, levels = LETTERS))
  ) %>%
  mutate(
    lum = if_else(is.na(lum), mean_lum, lum),
    lum = if_else(
      !((col %in% c(1:4, 20:23)) |
        ((col %in% c(1:7, 17:23)) &
           (row.num %in% 1:3 | row.num %in% 12:15))),
      lum,
      1e5
    )
  ) %>%
  ggplot(aes(col, row, fill = log10(lum))) +
  geom_tile() +
  theme(aspect.ratio = 15 / 23) +
  scale_fill_viridis_c()


