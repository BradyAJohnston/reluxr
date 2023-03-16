#' @noRd
.check_columns_exist <- function(data, cols, .envir = rlang::caller_env()) {
  names <- lapply(cols, rlang::quo_name)

  result <- lapply(names, function(x) {
    if (isFALSE(rlang::has_name(data, x))) {
      cli::cli_abort(
        c(
          "x" = "Column '{x}' does not exist in the dataframe.",
          "i" = "Ensure you have supplied the correct column names in the function call."
        ),
        call = .envir
      )
    }
  })

  TRUE
}
