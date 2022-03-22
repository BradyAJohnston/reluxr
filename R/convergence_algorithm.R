#' Update Frames Matrix
#'
#' Updates the old_mat with values from new_mat if they are below the old_mat or
#' less than or equal to the cutoff (default 0).
#'
#' @param old_mat Frames matrix that is updated.
#' @param new_mat Frames matrix that contains deconvoluted values.
#' @param cutoff Cutoff for comparison. Default = 0.
#'
#' @return Frames matrix
#'
update_frames <- function(old_mat, new_mat, cutoff = 0) {
  update_values <- new_mat < old_mat | new_mat <= cutoff

  old_mat[update_values] <- new_mat[update_values]
  old_mat
}

#' Converts a well ID string into a Vector index
#'
#' For indexing vectors and frames matrices, converts a string well ID ("B05")
#' into an integer for indexing into the columns of a frames matrix or a vector,
#' given the number of wells in the plate.
#'
#' @param well Well ID string to convert to index.
#' @param n_wells Number of wells in the plate.
#'
#' @return
#' @export
#'
#' @examples
well_to_index <- function(well, n_wells = 96) {
  stopifnot(is.character(well))

  col <- reluxr::well_to_colnum(well)
  row <- reluxr::well_to_rownum(well)

  index <- (row - 1) * n_cols_from_wells(n_wells) + col
  index
}


# try to apply to convergence algorithm -----------------------------------



#' Title
#'
#' @param data
#' @param value_col Column that contains observed values for creation of the
#'   deconvolution matrix.
#' @param exp_met Experimental metadata.
#'
#' @return
#' @export
#'
#' @examples
calc_matrix_D_best <- function(data,
                               value_col,
                               instrument_sensitivity = 20) {



  working_df <- data


  looking_for_best <- TRUE
  counter <- 0
  matrix_log <- list()
  lower_log <- c()
  update_timer <- 1

  while (looking_for_best) {
    counter <- counter + 1
    bleed_df <- calc_bleed_df(working_df, time_cutoff = 50)
    mean_mat <- bleed_df %>%
      tibble_to_matrix(ratio_mean)
    sd_mat <- bleed_df %>%
      tibble_to_matrix(ratio_sd)


    mean_rand_mat <-
      mean_mat + (update_timer / 50) * rnorm(1, 0, 1) * sd_mat

    matrix_D_working <- make_decon_matrix(mean_rand_mat)

    working_frames <- working_df %>%
      frames_to_matrix()

    adjusted_frames <- working_frames %>%
      deconvolute_matrix_frames(matrix_D_working)

    adjusted_frames_sans <-
      adjusted_frames[,-well_to_index(join_well(5, 5))]


    compared_frames_sans <-
      adjusted_frames_sans - instrument_sensitivity < 0



    perc_correct <-
      sum(compared_frames_sans) / max(nrow(compared_frames_sans)) / ncol(compared_frames_sans) * 100


    if (perc_correct < 100) {
      if (counter == 1) {
        cli::cli_progress_bar(
          name = "Optimising Deconvolution Matrix",
          status = counter
          )

        matrix_D_best <- matrix_D_working %*% diag(96)
        matrix_log[[counter]] <- matrix_D_working
        old_perc_correct <- perc_correct


        working_df <-
          update_frames(working_frames, adjusted_frames) %>%
          matrix_to_frames_df() %>%
          rename(cycle_nr = time,
                 lum = value)


        update_timer <- 0
      } else {
        if (perc_correct > old_perc_correct) {
          matrix_D_best <- matrix_D_working %*% matrix_D_best
          lower_log <- c(lower_log, counter)

          matrix_log[[counter]] <- matrix_D_working

          working_df <-
            update_frames(working_frames, adjusted_frames) %>%
            matrix_to_frames_df() %>%
            rename(cycle_nr = time,
                   lum = value)

          update_timer <- 0
          old_perc_correct <- perc_correct
        }

      }
    } else {
      looking_for_best <- FALSE
    }
    cli::cli_progress_update(status = scales::percent(old_perc_correct / 100, accuracy = 0.01), inc = 1)
    update_timer <- update_timer + 1
  }

  cli::cli_progress_done()

  return(matrix_D_best)
}
