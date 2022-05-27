library(tidyverse)
library(wellr)


fl <- "inst/Xfiles/tecan/calibration/calTecan1.xlsx"

df <- readxl::read_excel(fl, skip = 168, n_max = 122) |>
  janitor::clean_names()

df1 <- df |>
  pipebind::bind(x, tidyr::pivot_longer(
    data = x,
    cols = which(wellr:::well_check(colnames(x))),
    names_to = "well",
    values_to = "fluoro"
  )) |>
  dplyr::mutate(
    well = wellr::well_format(well)
  )

ref_well <- df1 |>
  filter(fluoro == max(fluoro)) |>
  pull(well)

mat <- df1 |>
  wellr::well_df_to_mat_frames("fluoro", "time_s")

D_best <- rl_mat_decon_best(
  mat = mat,
  ref_row = wellr::well_to_rownum(ref_well),
  ref_col = wellr::well_to_colnum(ref_well),
  b_noise = 10
)

rl_decon_frames(mat, D_best) |>
  as_tibble() |>
  mutate(
    frames = row_number()
  ) |>
  pivot_longer(-frames) |>
  ggplot(aes(frames, value, group = name)) +
  geom_line() +
  scale_y_log10()


target_wells <- c(well_join(1:8, 5), well_join(1:8, 7), well_join(c(2, 7), 6))

fl <- "inst/Xfiles/tecan/tecanON1.xlsx"

df <- readxl::read_excel(fl, skip = 168, n_max = 122) |>
  janitor::clean_names()

df2 <- df |>
  pipebind::bind(x, tidyr::pivot_longer(
    data = x,
    cols = which(wellr:::well_check(colnames(x))),
    names_to = "well",
    values_to = "fluoro"
  )) |>
  dplyr::mutate(
    well = wellr::well_format(well)
  )

df2 |>
  wellr::well_df_to_mat_frames("fluoro", "time_s") |>
  rl_decon_frames(D_best) |>
  as_tibble() |>
  mutate(
    frames = row_number()
  ) |>
  pivot_longer(-frames) |>
  ggplot(aes(frames, value, group = name, colour = name %in% target_wells)) +
  geom_line() +
  scale_y_log10(limits = c(0.1, NA)) +
  scale_x_continuous(expand = expansion()) +
  labs(colour = "Sample Well") +
  theme_light()
