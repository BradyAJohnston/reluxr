#' Title
#'
#' @param mat
#' @param calibrate_row
#' @param calibrate_col
#'
#' @return
#' @export
#'
#' @examples
calc_dis_matrix <- function(mat, calibrate_row = 5, calibrate_col = 5) {
  dimensions <- dim(mat)
  df <- make_empty_plate(dimensions[1], dimensions[2])
  df$dis = well_dis(df$row, df$col, calibrate_row = calibrate_row,
                    calibrate_col = calibrate_col)
  df$value <- matrix_to_vec(mat)

  df
}
#' Title
#'
#' @param mat
#' @param calibrate_row
#' @param calibrate_col
#'
#' @return
#' @export
#'
#' @examples
matrix_to_df <- function(mat, calibrate_row = 5, calibrate_col = 5) {
  dimensions <- dim(mat)
  df <- make_empty_plate(dimensions[1], dimensions[2])
  df$value <- matrix_to_vec(mat)
  df
}

#' Title
#'
#' @param data
#' @param column
#'
#' @return
#' @export
#'
#' @examples
df_to_matrix <- function(data, column) {
  ncol = max(data$col, na.rm = TRUE)
  as.data.frame(data) %>%
    reorder_df_by_wells() %>%
    .[, column] %>%
    vec_to_matrix(ncol = ncol)
}

#' Title
#'
#' @param data
#' @param calibrate_row
#' @param calibrate_col
#'
#' @return
#' @export
#'
#' @examples
calc_dis_df <- function(data, calibrate_row = 5, calibrate_col = 5) {
  data$dis = well_dis(
    row = data$row,
    col = data$col,
    calibrate_row = calibrate_row,
    calibrate_col = calibrate_col
  )
  data
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
reorder_df_by_wells <- function(data) {
  data <- data[order(data$col), ]
  data <- data[order(data$row), ]
  data
}

#' Title
#'
#' @param nrow
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
make_empty_plate <- function(nrow = 8, ncol = 12) {
  if (length(nrow) == 1) nrow <- seq(nrow)
  if (length(ncol) == 1) ncol <- seq(ncol)

  plate <- expand.grid(
    col = ncol,
    row = nrow
  )[, c("row", "col")]
  plate$well <- well_join(plate$row, plate$col)
  plate
}


#' Title
#'
#' @param mat
#'
#' @return
#' @export
#'
#' @examples
matrix_to_vec <- function(mat) {
  lapply(seq(nrow(mat)), function(x) {
    mat[x, ]
  }) %>%
    do.call(`c`, .)
}

#' Title
#'
#' @param vec
#' @param ncol
#'
#' @return
#' @export
#'
#' @examples
vec_to_matrix <- function(vec, ncol = 12) {
  lapply(seq(length(vec) / ncol), function(x) {
    vec[seq((x - 1) * ncol + 1, x * ncol)]
  }) %>%
    do.call(rbind, .)
}

#' Title
#'
#' @param vec
#' @param mat_decon
#'
#' @return
#' @export
#'
#' @examples
deconvolute_vector <- function(vec, mat_decon) {
  t(solve(mat_decon) %*% as.vector(vec))
}

#' Title
#'
#' @param data
#' @param mat_decon
#' @param column
#'
#' @return
#' @export
#'
#' @examples
deconvolute_df_col <- function(data, mat_decon, column) {
  data <- data[order(data$col), ]
  data <- data[order(data$row), ]

  vec <- as.data.frame(data)[, column]
  data$deconvoluted <- deconvolute_vector(vec, mat_decon)
  data
}


#' Title
#'
#' @param data
#' @param value_col
#' @param time_col
#'
#' @return
#' @export
#'
#' @examples
frames_to_matrix <- function(data, value, time) {
  data <- data %>%
    dplyr::arrange(row, col)

  frames <- data %>%
    dplyr::pull({{ time }}) %>%
    unique()

  purrr::map(frames, function(x) {
    data %>%
      dplyr::filter(
        {{ time }} == x
      ) %>%
      dplyr::pull({{ value }})
  }) %>%
    purrr::reduce(rbind) %>%
    as.matrix()
}



# frames_to_matrix <- function(data, value_col = "lum", time_col = "cycle_nr") {
#   data <- as.data.frame(data)
#   frame_numbers <- order(unique(data[, cement({{ time_col }})]))
#   data <- data[order(data$col), ]
#   data <- data[order(data$row), ]
#
#   lapply(frame_numbers, function(x) {
#     data[data[, cement({{ time_col }})] == x, cement({{ value_col }})]
#   }) %>%
#     do.call(rbind, .) %>%
#     as.matrix()
# }


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
deconvolute_matrix_frames <- function(mat, mat_decon) {
  lapply(seq(nrow(mat)), function(x) {
    deconvolute_vector(
      vec = mat[x, ],
      mat_decon = mat_decon
    )
  }) %>%
    do.call(rbind, .) %>%
    as.matrix()
}

#' Title
#'
#' @param mat
#'
#' @return
#' @export
#'
#' @examples
matrix_to_frames_df <- function(mat) {
  nframes <- nrow(mat)
  nwells <- ncol(mat)
  dimensions <- dim(mat)

  # df <- make_empty_plate(n_rows_from_wells(nwells),
                         # n_cols_from_wells(nwells))
  df <- wellr::well_plate(
    nrow = wellr:::n_rows_from_wells(nwells),
    ncol = wellr:::n_cols_from_wells(nwells)
  )

  lapply(seq(nframes), function(x) {
    df$value <- mat[x, ]
    df$time <- x
    df
  }) %>%
    do.call(rbind, .)
}



mean_dis_mat <- function(data) {
  unique_dis <- unique(data$dis)


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
  df <- lapply(seq(nrow(data)), function(x) {
    data_row <- data[x, ]
    dis <- data$dis[data$well == data$well[x]]
    mean_dis <- mean(data$value[data$dis == dis], na.rm = TRUE)

    if (is.na(data$value[x])) {
      data_row$value <- mean_dis
    }

    if (is.na(data_row$value)) {
      data_row$value <- 1
    }

    data_row
  }) %>%
    do.call(rbind, .)

  reorder_df_by_wells(df)
}

#' Title
#'
#' @param data
#' @param calibrate_row
#' @param calibrate_col
#'
#' @return
#' @export
#'
#' @examples
translate_df_wells <- function(data,
                               calibrate_row = 5,
                               calibrate_col = 5) {
  n_rows <- max(data$row, na.rm = TRUE)
  n_cols <- max(data$col, na.rm = TRUE)

  row_adjustment <- n_rows - calibrate_row
  col_adjustment <- n_cols - calibrate_col

  data$row <- data$row + row_adjustment
  data$col <- data$col + col_adjustment
  data$well <- well_join(row = data$row, col = data$col)

  empty_plate <- make_empty_plate(
    nrow = 2 * n_rows - 1,
    ncol = 2 * n_cols - 1
  )
  empty_plate$value <- NA


  lapply(seq(nrow(empty_plate)), function(x) {
    plate_row <- empty_plate[x, ]
    well <- plate_row$well

    if (well %in% data$well) {
      plate_row$value <- data$value[data$well == well]
    }



    plate_row
  }) %>%
    do.call(rbind, .) %>%
    reorder_df_by_wells()
}

#' Title
#'
#' @param data
#' @param column
#' @param mat_decon
#'
#' @return
#' @export
#'
#' @examples
deconvolute_df_frames <- function(data, column, mat_decon) {
  deconvoluted_data <- data %>%
    frames_to_matrix({{ column }}) %>%
    deconvolute_matrix_frames(mat_decon) %>%
    matrix_to_frames_df()

  data$adjusted <- deconvoluted_data$value
  data
}
