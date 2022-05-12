#' Title
#'
#' @param mat
#' @param rows
#' @param col
#'
#' @return
#' @export
#'
#' @examples
decon_col <- function(mat, rows, col = 12) {
  lapply(rows, function(x) {
    mat[x, seq(col, 2 * col - 1)] %>%
      toeplitz()
  }) %>%
    do.call(rbind, .)
}

#' Title
#'
#' @param mat
#' @param sample_row
#' @param sample_col
#'
#' @return
#' @export
#'
#' @examples
make_decon_matrix <- function(mat#ˇ,
                              # sample_row = 8,
                              # sample_col = 12
                              ) {
  sample_row <- ceiling(nrow(mat) / 2)
  sample_col <- ceiling(ncol(mat) / 2)

  lapply(seq(sample_row, 1), function(x) {
    decon_col(mat,
              rows = seq(x, x + max(sample_row) - 1),
              col = sample_col)
  }) %>%
    do.call(cbind, .)
}


#' Title
#'
#' @param data
#' @param decon_mat
#' @param col
#'
#' @return
#' @export
#'
#' @examples
deconvolute_data <- function(data, decon_mat, col) {
  vec_data <- data %>%
    dplyr::arrange(row, col) %>%
    dplyr::pull({{ col }})

  vec_adjusted <- solve(decon_mat) %*% vec_data

  data %>%
    dplyr::mutate(
      adjusted = vec_adjusted
    )
}

#' Create Randomised Extended Matrix for Optimisation
#'
#' @param data Extended tibble from `create_extended_tibble()`.
#'
#' @return
#' @export
#'
#' @examples
random_extended_matrix <- function(data) {
  matrix_e <- tibble_to_matrix(data, ratio_mean)
  matrix_sd <- tibble_to_matrix(data, ratio_sd)
  matrix_rand <- matrix(rnorm(15 * 23, 0, 1), ncol = 23)
s
  matrix_e + matrix_rand * matrix_sd
  # matrix_e + rnorm(1, 0, 1) * matrix_sd
}

#' Create Extended dataframe from Calibration Plate.
#'
#' @param data
#' @param calibrate_row
#' @param calibrate_col
#' @param lum_bg_ratio_mean
#' @param lum_bg_ratio_sd
#'
#' @return
#' @export
#'
#' @examples
create_extended_tibble <- function(data,
                                   calibrate_row = 5,
                                   calibrate_col = 5,
                                   lum_bg_ratio_mean,
                                   lum_bg_ratio_sd
) {
  n_rows <- max(data$row, na.rm = TRUE)
  n_cols <- max(data$col, na.rm = TRUE)

  row_adjustment <- n_rows - calibrate_row
  col_adjustment <- n_cols - calibrate_col

  data %>%
    # translate the plate as required
    dplyr::mutate(
      row = row + row_adjustment,
      col = col + col_adjustment
    ) %>%

    # fill with empty values for the other spots in extended plate
    dplyr::right_join(
      create_blank_plate(
        n_rows = 2 * n_rows - 1,
        n_cols = 2 * n_cols - 1
        ),
      by = c("row" = "row", "col" = "col")
    ) %>%

    # calculate distance for all wells in extended plate
    dplyr::mutate(
      dis = well_dis(
        row = row,
        col = col,
        calibrate_row = calibrate_row + row_adjustment,
        calibrate_col = calibrate_col + col_adjustment
      )
    ) %>%

    # fill in empty wells with values from corresponding wells with same
    # distance from the calibration well
    dplyr::group_by(dis) %>%
    dplyr::mutate(
      average_ratio_mean  = mean(ratio_mean, na.rm = TRUE),
      average_ratio_sd    = mean(ratio_sd, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%

    # fill in remaining
    dplyr::mutate(
      ratio_mean = dplyr::if_else(
        is.na(ratio_mean),
        average_ratio_mean,
        ratio_mean
      ),
      ratio_sd = dplyr::if_else(
        is.na(ratio_sd),
        average_ratio_sd,
        ratio_sd
      ),
      ratio_mean = dplyr::if_else(
        is.na(ratio_mean),
        lum_bg_ratio_mean,
        ratio_mean
      ),
      ratio_sd = dplyr::if_else(
        is.na(ratio_sd),
        lum_bg_ratio_sd,
        ratio_sd
      )
    ) %>%
    dplyr::select(well, row, col,  ratio_mean, ratio_sd, dis)
}

#' Title
#'
#' @param data
#' @param decon_mat
#'
#' @return
#' @export
#'
#' @examples
decon_frames <- function(data, decon_mat) {
  data %>%
    dplyr::group_by(cycle_nr) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      adjusted = purrr::map(
        .x = data,
        .f = ~ deconvolute_data(
          data = .,
          decon_mat = decon_mat,
          col = lum
        )
      )
    ) %>%
    dplyr::select(cycle_nr, adjusted) %>%
    tidyr::unnest(adjusted)
}


#' Calculate Bleed-Through Dataframe
#'
#' @param data
#' @param time_cutoff
#' @param calibrate_row
#' @param calibrate_col
#' @param col_time
#' @param col_value
#' @param background_wells
#'
#' @return
#' @export
#'
#' @examples
calc_bleed_df <- function(data,
                          col_time,
                          col_value,
                          time_cutoff = 30,
                          calibrate_row = 5,
                          calibrate_col = 5,
                          background_wells = well_join(1:8, 12)
                          ) {
  time_averaged_df <- data %>%
    dplyr::group_by({{ col_time }}) %>%
    dplyr::mutate(
      ratio = {{ col_value }} / max({{ col_value }})
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter({{ col_time }} > time_cutoff) %>%
    dplyr::group_by(well, row, col) %>%
    dplyr::summarise(
      ratio_mean = mean(ratio, na.rm = TRUE),
      ratio_sd   = sd(ratio, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  background_ratios <- time_averaged_df %>%
    dplyr::filter(well %in% background_wells) %>%
    dplyr::summarise(
      mean = mean(ratio_mean, na.rm = TRUE),
      sd = mean(ratio_sd, na.rm = TRUE)
    )


  time_averaged_df %>%
    create_extended_tibble(
      lum_bg_ratio_mean = background_ratios$mean,
      lum_bg_ratio_sd = background_ratios$sd,
      calibrate_row = calibrate_row,
      calibrate_col = calibrate_col
    )
}
