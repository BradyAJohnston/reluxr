


update_frames <- function(old_mat, new_mat) {
  update_values <- new_mat < old_mat | new_mat <= 0

  old_mat[update_values] <- new_mat[update_values]
  old_mat
}





well_to_index <- function(well, n_wells = 96) {
  col <- reluxr::well_to_colnum(well)
  row <- reluxr::well_to_rownum(well)

  index <- (row - 1) * n_cols_from_wells(n_wells) + col
  index
}



# try to apply to convergence algorithm -----------------------------------



calc_matrix_D_best <- function(data) {
  looking_for_best <- TRUE

  counter <- 0

  working_df <- df_observed_values

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

          print(paste0(
            "Iteration ",
            counter,
            ", Converged:",
            round(old_perc_correct, 2),
            "%"
          ))

          update_timer <- 0
          old_perc_correct <- perc_correct
        }

      }
    } else {
      looking_for_best <- FALSE
      beepr::beep()
    }

    update_timer <- update_timer + 1



  }

  print(paste0("Iteration ", counter, ", Converged: 100%"))

  return(matrix_D_best)
}


# matriX_D_best <- calc_matrix_D_best(df_observed_values)
