#' Calculate Bleed Through Matrix
#'
#' Expand and calculate a bleed-through matrix from the given matrix around the
#' given reference well.
#'
#' @param mat A matrix to expand and calculate relative bleed-through based on a
#'   reference well.
#' @param ref_row Reference well row number.
#' @param ref_col Reference well column number.
#' @param b_noise Calculated background noise to fill the bleed-through matrix.
#' @param relative TRUE / FALSE whether to return relative values.
#' @param .f Function to apply to the expanded matrix, defaults to mean.
#'
#' @return a matrix, that is a bleed-through matrix calculated from the given
#'   matrix.
.mat_calc_bleed <-
  function(mat,
           ref_row,
           ref_col,
           b_noise = 10,
           relative = TRUE,
           .f = mean) {
    # calculate the dimensions of the bleed-through matrix
    n_row <- nrow(mat) * 2 - 1
    n_col <- ncol(mat) * 2 - 1

    adjust_well <- function(n_dim, ref) {
      (n_dim + 1) / 2 - ref
    }

    # calculate the adjustment that will be needed to make to the plate, to ensure
    # that the reference well is centered in the bleed through matrix that is
    # later calculated
    adjust_row <- adjust_well(n_row, ref_row)
    adjust_col <- adjust_well(n_col, ref_col)

    # create an empty matrix, with final dimensions of the bleedthrough matrix
    mat_expanded <- matrix(NA, nrow = n_row, ncol = n_col)


    # input the starting matrix values into the expanded matrix, adjusting
    # their positions to center the reference well in the matrix

    mat_expanded[
      seq(nrow(mat)) + adjust_row,
      seq(ncol(mat)) + adjust_col
    ] <- mat

    # calculate the distance of each well from the center of the bleed through
    # matrix, to group them and calculate means
    mat_dis <- outer(
      X = seq(n_row),
      Y = seq(n_col),
      well_dis,
      ref_row = (n_row + 1) / 2,
      ref_col = (n_col + 1) / 2
    )

    # calculate the mean for each distance value, to fill into where there are
    # NA values in the bleed-through matrix but there are values available that
    # have the same distances from the reference well
    vec_mean_fill <- sapply(c(t(mat_dis)), function(x) {
      .f(mat_expanded[mat_dis == x], na.rm = TRUE)
    })
    # turn the above vector into a matrix with the same dimensions
    mat_mean_fill <- matrix(
      data = vec_mean_fill,
      nrow = n_row,
      ncol = n_col,
      byrow = TRUE
    )

    # fill in the parts where it's NA and there is a suitable mean
    # value calculated from mat_mean_fill
    mat_expanded[is.na(mat_expanded)] <-
      mat_mean_fill[is.na(mat_expanded)]
    # mat_expanded <- mat_mean_fill

    # there still remains regions which aren't able to be filled, so they are
    # filled from the background noise values

    mat_expanded[is.na(mat_expanded)] <- b_noise

    # create the normalised relative values if requested
    if (relative) {
      mat_expanded <- mat_expanded / max(mat_expanded)
    }

    # returned the extended bleed-through matrix
    mat_expanded
  }


#' Make Deconvolution Matrix From Bleed-Through Matrix
#'
#' @param mat Bleed-through matrix from `.mat_calc_bleed()`
#'
#' @return a matrix, ready for deconvolution.
.mat_calc_deconvolution <- function(mat) {
  ref_row <- (nrow(mat) + 1) / 2
  ref_col <- (ncol(mat) + 1) / 2

  toe_col <- function(mat, rows) {
    res_list <- lapply(rows, function(x) {
      stats::toeplitz(mat[x, seq(ref_col, ncol(mat))])
    })
    do.call(rbind, res_list)
  }

  toe_mat <- lapply(seq(ref_row, 1), function(x) {
    toe_col(mat, seq(x, x + ref_row - 1))
  })

  do.call(cbind, toe_mat)
}

#' Deconvolute a Single Vector
#'
#' @param vec Numeric vector, representing the wells.
#' @param mat_decon Deconvolution matrix created through `.mat_calc_deconvolution()`.
#'
#' @return a numeric vector, the same length as `vec`.
.decon_vec <- function(vec, mat_decon) {
  solve(mat_decon) %*% vec
}

#' Deconvolute a Multi-Frame Matrix
#'
#' @param mat_frames A multi-frame matrix, with each time point a row, and each
#'   column a well.
#' @param mat_decon A deconvolution matrix created through `.mat_calc_deconvolution()`.
#'
#' @return A deconvoluted multi-frame matrix.
.deconvolute_multi_frame_matrix <- function(mat_frames, mat_decon) {
  frames <-
    t(apply(mat_frames, 1, .decon_vec, mat_decon = mat_decon))
  colnames(frames) <- colnames(mat_frames)
  frames
}

#' Calculate the Optimal Deconvolution Matrix
#'
#' @param mat A multi-frame matrix, where each row is a frame and each column is a well.
#' @param ref_row Row number for the reference well.
#' @param ref_col Column number for the reference well.
#' @param b_noise Value for the background noise.
#'
#' @return a matrix, the optimised deconvolution matrix.
rl_mat_decon_best <-
  function(mat,
           ref_row,
           ref_col,
           b_noise = 20) {
    plate_size <- ncol(mat)
    ref_index <-
      well_to_index(well_join(ref_row, ref_col), plate = plate_size)

    n_rows <- sqrt(ncol(mat) * 2 / 3)
    n_cols <- sqrt(ncol(mat) * 3 / 2)



    mat_working <- mat

    looking_for_best <- TRUE
    counter <- 0
    update_counter <- 0

    while (looking_for_best) {
      counter <- counter + 1
      update_counter <- update_counter + 1

      mat_mean <- matrix(
        apply(mat_working, 2, mean),
        nrow = n_rows,
        ncol = n_cols,
        byrow = TRUE
      )

      mat_bleed_mean <- .mat_calc_bleed(
        mat = mat_mean,
        ref_row = ref_row,
        ref_col = ref_col,
        b_noise = b_noise,
        relative = TRUE,
        .f = mean
      )

      mat_bleed_sd <- .mat_calc_bleed(
        mat = mat_mean,
        ref_row = ref_row,
        ref_col = ref_col,
        b_noise = b_noise,
        relative = TRUE,
        .f = stats::sd
      )

      mat_mean_rand <-
        mat_bleed_mean + (update_counter / 50) * stats::rnorm(1, 0, 1) * mat_bleed_sd
      mat_D_working <- .mat_calc_deconvolution(mat_mean_rand)
      mat_adjusted <- .deconvolute_multi_frame_matrix(mat_working, mat_D_working)
      mat_compared <- mat_adjusted - b_noise * 3 < 0

      perc_correct <-
        sum(mat_compared[, -ref_index]) / (length(mat_compared[, -ref_index])) * 100

      if (perc_correct < 100) {
        if (counter == 1) {
          cli::cli_progress_bar(
            name = "Optimising Deconvolution Matrix",
            status = counter,
            clear = FALSE
          )

          mat_D_best <- mat_D_working %*% diag(plate_size)
          old_perc_correct <- perc_correct
          update_counter <- 0

          update_value <-
            mat_adjusted < mat_working | mat_adjusted <= 0
          mat_working[update_value] <- mat_adjusted[update_value]
        } else {
          if (perc_correct > old_perc_correct) {
            mat_D_best <- mat_D_working %*% mat_D_best

            update_value <-
              mat_adjusted < mat_working | mat_adjusted <= 0
            mat_working[update_value] <- mat_adjusted[update_value]

            update_counter <- 0
            old_perc_correct <- perc_correct
          }
        }
      } else {
        looking_for_best <- FALSE
      }
      cli::cli_progress_update(
        status = scales::percent(old_perc_correct / 100, accuracy = 0.01),
        inc = 1
      )
    }
    cli::cli_progress_update(status = scales::percent(1), inc = 1)
    cli::cli_progress_done()

    return(mat_D_best)
  }

#' Reorder a DatFrame
#'
#' Reorders a dataframe by the time points, then by the rows then by the
#' columns.
#'
#' @param data A dataframe with a time column and a well column.
#' @param time The name of the column with the time points.
#' @param well The name of the column with the well ID information.
#'
#' @return The input dataframe reordered.
df_arrange <- function(data, time = "time", well = "well") {
  wells <- dplyr::pull(data, well)

  cols <- well_to_col_num(wells)
  rows <- well_to_row_num(wells)
  frames <- dplyr::pull(data, {{ time }})

  dplyr::arrange(data, frames, rows, cols)
}

#' Create a multi-frame matrix from a dataframe.
#'
#' Creats a matrix with each row being a time point in a multi-frame experiment,
#' and each column is a single well. The columns are concatenated by row
#' (ordered A1, A2, ... B1, B2, ...).
#'
#' @param data A dataframe with columns for the value, time and well ID.
#' @param value Name of the column which contains the values for the matrix.
#' @param time Name of the column which defines the time points for the frames
#'   (rows) of the matrix.
#' @param well Name of the column which contains the well IDs of the samples.
#' @param arrange Logical, whether to return a dataframe arranged by time and
#'   well.
#'
#' @return a matrix, with

.multi_frame_matrix_from_df <-
  function(data,
           value,
           time = "time",
           well = "well",
           arrange = FALSE) {
    data <- df_arrange(data, {{ time }}, {{ well }})
    wells <- dplyr::pull(data, {{ well }})

    cols <- well_to_col_num(wells)
    rows <- well_to_row_num(wells)
    frames <- dplyr::pull(data, {{ time }})

    n_cols <- max(cols)
    n_rows <- max(rows)

    mat <- matrix(
      dplyr::pull(data, {{ value }}),
      ncol = n_cols * n_rows,
      nrow = length(unique(frames)),
      byrow = TRUE
    )

    rownames(mat) <- unique(frames)
    colnames(mat) <- unique(wells)

    mat
  }

#' Convert a Multi-Frame Matrix to a Vector
#'
#' Takes a multi-frame matrix and converts to a vector, with each frame being
#' concatenated end-to-end. Each row / frame of the matrix is a time point, with
#' each column representing a well (ordered A1, A2, ... B1, B2, ...).
#'
#' @param mat A multi-frame matrix.
#' @param rowwise Logical, whether to concatenate the rows (default, TRUE) or
#'   the columns.
#'
#' @return A vector, with values concatenated from the matrix.

.multi_frame_matrix_to_vec <- function(mat, rowwise = TRUE) {
  if (rowwise) {
    c(t(mat))
  } else {
    c(mat)
  }
}

#' Calculate a Deconvolution Matrix
#'
#' Use the data from a calibration plate, where the plate is empty except for
#' a single well with a luminescent signal, to create a deconvolution matrix
#' that can be used to adjust other experimental results.
#'
#' The deconvolution matrix will be unique for each plate type and plate-reader,
#' so a matrix should be calculated for each combination of plate and plate
#' reader, but once this is calculated, it can be re-used to adjust future
#' experimental results.
#'
#' @param data A data frame that contains the data of the calibration plate.
#' @param ref_well The well ID of the reference well (i.e. 'E05', 'I12")
#' @param b_noise The value of the background noise, which is the average signal
#'   for the background wells that are far away from the reference well.
#' @param value Name of the column containing the luminescent values.
#' @param time Name of the column with the time values.
#' @param well Name of the column with the well ID values.
#'
#' @return a deconvolution matrix, for use in `rl_adjust_plate()`
#' @export
#'
#' @examples
#'
#' fl <- system.file(
#'   "extdata",
#'   "calibrate_tecan",
#'   "calTecan1.xlsx",
#'   package = "reluxr"
#' )
#'
#' dat <- plate_read_tecan(fl)
#'
#' dat
#'
#' mat_d <- dat |>
#'   dplyr::filter(signal != "OD600") |>
#'   dplyr::filter(time_s > 500) |>
#'   rl_calc_decon_matrix(value, time_s, ref_well = "E05", b_noise = 30)
#'
#' image(log10(mat_d))
rl_calc_decon_matrix <-
  function(data,
           value,
           b_noise,
           time = "time",
           ref_well = "I05",
           well = "well") {
    ref_row <- well_to_row_num(ref_well)
    ref_col <- well_to_col_num(ref_well)

    mat_frames <- .multi_frame_matrix_from_df(
      data = data,
      value = {{ value }},
      time = {{ time }},
      well = {{ well }}
    )

    mat_d_best <- rl_mat_decon_best(
      mat_frames,
      ref_row = ref_row,
      ref_col = ref_col,
      b_noise = b_noise
    )

    mat_d_best
  }

#' Adjust Experimental Luminescent Data
#'
#' Using a deconvolution matrix, created through `rl_calc_decon_matrix()`,
#' adjust the values in the `col_value` column to take into account
#' bleed-through from surrounding wells.
#'
#' @param data A data frame that contains the experimental data.
#' @param value The name of the column containing the values (i.e. 'lum').
#' @param time The name of the column containing the time values (i.e.
#'   'time')
#' @param mat_decon A deconvolution matrix created through
#'   `rl_calc_decon_matrix()`
#' @param well Name of the column with the well ID information.
#'
#' @importFrom rlang := .data
#'
#' @return A dataframe with the specified column having been deconvoluted, using
#'   the supplied deconvolution matrix.
#' @export
#'
#' @examples
#'
#' fl <- system.file(
#'   "extdata",
#'   "calibrate_tecan",
#'   "calTecan1.xlsx",
#'   package = "reluxr"
#' )
#'
#' dat <- plate_read_tecan(fl)
#'
#' mat_d_best <- dat |>
#'   dplyr::filter(signal != "OD600") |>
#'   dplyr::filter(time_s > 500) |>
#'   rl_calc_decon_matrix("value", "time_s", ref_well = "E05", b_noise = 30)
#'
#' dat |>
#'   dplyr::summarise(value = mean(value), .by = well) |>
#'   rl_plot_plate(value, trans = log10) +
#'   ggplot2::scale_fill_viridis_c(
#'     limits = c(1, NA)
#'   )
#'
#' dat |>
#'   dplyr::filter(signal == "LUMI") |>
#'   rl_adjust_plate(value, mat_d_best, time = time_s) |>
#'   dplyr::summarise(value = mean(value), .by = well) |>
#'   rl_plot_plate(value, trans = log10) +
#'   ggplot2::scale_fill_viridis_c(
#'     limits = c(1, NA)
#'   )
rl_adjust_plate <- function(data, value, mat_decon, time = "time", well = "well") {
  data <- df_arrange(data, {{ time }}, {{ well }})

  mat_frames <- .multi_frame_matrix_from_df(data, {{ value }}, {{ time }})

  mat_frames_deconvoluted <- .deconvolute_multi_frame_matrix(mat_frames, mat_decon)

  data <- dplyr::mutate(
    data,
    {{ value }} := .multi_frame_matrix_to_vec(mat_frames_deconvoluted)
  )

  data
}
