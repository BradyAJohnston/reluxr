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

well_dis <- function (row, col, calibrate_row = 5, calibrate_col = 5) {
  sqrt((row - calibrate_row) ^ 2 + (col - calibrate_col) ^ 2)
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














create_blank_plate <- function(n_cols, n_rows) {
  expand.grid(col = seq(n_cols), row = seq(n_rows)) %>%
    as_tibble()
}



create_extended_tibble <- function(data,
                                   calibrate_row = 5,
                                   calibrate_col = 5,
                                   lum_bg_ratio_mean,
                                   lum_bg_ratio_sd
) {

  n_rows <- max(data$row)
  n_cols <- max(data$col)

  row_adjustment <- n_rows - calibrate_row
  col_adjustment <- n_cols - calibrate_col


  data %>%

    # translate the plate as required
    mutate(
      row = row + row_adjustment,
      col = col + col_adjustment
    ) %>%

    # fill with empty values for the other spots in extended plate
    right_join(
      create_blank_plate(23, 15)
    ) %>%

    # calculate distance for all wells in extended plate
    mutate(
      dis = well_dis(
        row = row,
        col = col,
        calibrate_row = calibrate_row + row_adjustment,
        calibrate_col = calibrate_col + col_adjustment
      )
    ) %>%

    # fill in empty wells with values from corresponding wells with same
    # distance from the calibration well
    group_by(dis) %>%
    mutate(
      average_ratio_mean  = mean(ratio_mean, na.rm = TRUE),
      average_ratio_sd    = mean(ratio_sd, na.rm = TRUE)
    ) %>%
    ungroup() %>%

    # fill in remaining
    mutate(
      ratio_mean = if_else(
        is.na(ratio_mean),
        average_ratio_mean,
        ratio_mean
      ),
      ratio_sd = if_else(
        is.na(ratio_sd),
        average_ratio_sd,
        ratio_sd
      ),
      ratio_mean = if_else(
        is.na(ratio_mean),
        lum_bg_ratio_mean,
        ratio_mean
      ),
      ratio_sd = if_else(
        is.na(ratio_sd),
        lum_bg_ratio_sd,
        ratio_sd
      )
    ) %>%
    select(well, row, col,  ratio_mean, ratio_sd, dis)
}




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

decon_col <- function(mat, rows, col = 12) {
  lapply(rows, function(x) {
    mat[x, seq(col, col + 11)] %>%
      toeplitz()
  }) %>%
    do.call(rbind, .)
}

make_decon_matrix <- function(mat, sample_row = 8, sample_col = 12) {
  lapply(seq(sample_row, 1), function(x) {
    decon_col(mat, rows = seq(x, x + max(sample_row) - 1))
  }) %>%
    do.call(cbind, .)
}



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
