library(tidyverse)


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

