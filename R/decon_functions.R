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
    mat[x, seq(col, col + 11)] %>%
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
make_decon_matrix <- function(mat, sample_row = 8, sample_col = 12) {
  lapply(seq(sample_row, 1), function(x) {
    decon_col(mat, rows = seq(x, x + max(sample_row) - 1))
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
