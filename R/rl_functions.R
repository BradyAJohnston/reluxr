#' Calculate Bleed Through Matrix
#'
#' @param mat A matrix to expand and calculate relative bleed-through based on a
#'   reference well.
#' @param ref_row Reference row number.
#' @param ref_col Reference column number.
#' @param b_noise Calculated background noise to fill the bleed-through matrix.
#' @param relative TRUE / FALSE whether to return relative values.
#' @param .f Function to apply to the expanded matrix, defaults to mean.
#'
#' @return a matrix, that is a bleed-through matrix calculated from the given
#'   matrix.
#' @export
#'
#' @examples
rl_mat_bleed <- function(mat, ref_row, ref_col, b_noise = 10, relative = TRUE, .f = mean) {

  # calculate the dimensions of the bleed-through matrix
  n_row <- nrow(mat) * 2 - 1
  n_col <- ncol(mat) * 2 - 1

  adjust_well <- function(n_dim, ref) {
    (n_dim + 1) / 2 - ref
  }

  # calculate the adjustmend that will be needed to make to the plate,
  # to ensure that the reference well is centred in the bleedthrough
  # matrix that is later calculated
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
    wellr::well_dis,
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
  mat_expanded[is.na(mat_expanded)] <- mat_mean_fill[is.na(mat_expanded)]
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


#' Make Deconvolution Matrix From Bleed-Through
#'
#' @param mat Bleed-through matrix from `rl_mat_bleed()`
#'
#' @return a matrix, ready for deconvolution.
#' @export
#'
#' @examples
rl_mat_decon <- function(mat) {
  ref_row <- (nrow(mat) + 1) / 2
  ref_col <- (ncol(mat) + 1) / 2

  toe_col <- function(mat, rows) {
    res_list <- lapply(rows, function(x) {
      toeplitz(mat[x, seq(ref_col, ncol(mat)) ])})
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
#' @param decon_mat Deconvolution matrix created through `rl_mat_decon()`.
#'
#' @return a numeric vector, the same length as `vec`.
#' @export
#'
#' @examples
rl_decon_vec <- function(vec, decon_mat) {
  solve(decon_mat) %*% vec
}

#' Deconvolute a Multi-Frame Matrix
#'
#' @param mat_frames A multi-frame matrix, with each time point a row, and each
#'   column a well.
#' @param decon_mat A deconvolution matrix created through `rl_mat_decon()`.
#'
#' @return A deconvoluted multi-frame matrix.
#' @export
#'
#' @examples
rl_decon_frames <- function(mat_frames, decon_mat) {
  frames <- t(apply(mat_frames, 1, rl_decon_vec, decon_mat = decon_mat))
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
#' @export
#'
#' @examples
rl_mat_decon_best <-
  function(mat,
           ref_row,
           ref_col,
           b_noise = 20) {
    plate_size = ncol(mat)
    ref_index <- wellr::well_to_index(wellr::well_join(ref_row, ref_col), plate = plate_size)

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

      mat_bleed_mean <- rl_mat_bleed(
        mat = mat_mean,
        ref_row = ref_row,
        ref_col = ref_col,
        b_noise = b_noise,
        relative = TRUE,
        .f = mean
      )
      mat_bleed_sd <- rl_mat_bleed(
        mat = mat_mean,
        ref_row = ref_row,
        ref_col = ref_col,
        b_noise = b_noise,
        relative = TRUE,
        .f = sd
      )

      mat_mean_rand <- mat_bleed_mean + (update_counter / 50) * rnorm(1, 0, 1) * mat_bleed_sd
      mat_D_working <- rl_mat_decon(mat_mean_rand)
      mat_adjusted <- rl_decon_frames(mat_working, mat_D_working)
      mat_compared <- mat_adjusted - b_noise * 3 < 0

      perc_correct <-
        sum(mat_compared[, -ref_index]) / (length(mat_compared[, -ref_index])) * 100

      if (perc_correct < 100) {
        if (counter == 1) {
          cli::cli_progress_bar(name = "Optimising Deconvolution Matrix", status = counter, clear = FALSE)

          mat_D_best <- mat_D_working %*% diag(plate_size)
          old_perc_correct <- perc_correct
          update_counter <- 0

          update_value <- mat_adjusted < mat_working | mat_adjusted <= 0
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
