#' Creates blank plate with given columns and rows
#'
#' @param n_cols Number of columns.
#' @param n_rows Number of rows.
#'
#' @return
#' @export
#'
#' @examples
create_blank_plate <- function(n_cols, n_rows) {
  expand.grid(col = seq(n_cols), row = seq(n_rows)) %>%
    tibble::as_tibble()
}

#' Creates a matrix from a tibble with row and col columns.
#'
#' @param data Tibble to turn into matrix.
#' @param value Column that becomes the values.
#'
#' @return
#' @export
#'
#' @examples
tibble_to_matrix <- function(data, value) {
  data %>%
    dplyr::select(row, col , {{ value }}) %>%
    dplyr::arrange(row, col) %>%
    tidyr::pivot_wider(values_from = {{ value }}, names_from = col) %>%
    tibble::column_to_rownames("row") %>%
    as.matrix()
}

#' Turns a vector into a palte-based tibble.
#'
#' @param vec Vector of values.
#'
#' @return
#' @export
#'
#' @examples
vec_to_tibble <- function(vec, n_row = 8, n_col = 12) {
  vec %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      col = rep(seq(n_col), n_row),
      row = rep(seq(n_row), each = n_col)
    )
}


#' Title
#'
#' @param mat
#' @param value_col
#'
#' @return
#' @export
#'
#' @examples
matrix_to_tibble <- function(mat, value_col = "value") {
  mat %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(.fn = ~stringr::str_extract(.x, "\\d+")) %>%
    tibble::rownames_to_column("row") %>%
    tidyr::pivot_longer(cols = matches("\\d+"),
                 names_to = "col",
                 values_to = value_col) %>%
    dplyr::transmute(
      row = as.numeric(row),
      col = as.numeric(col),
      well = well_join(row, col),
      value = as.numeric(value)
    )
}
