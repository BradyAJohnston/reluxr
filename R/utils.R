#' @noRd
.check_columns_exist <- function(data, args, .envir = rlang::caller_env()) {
  # args <- rlang::enquos(...)
  names <- lapply(args, rlang::quo_name)

  result <- lapply(names, function(x) {
    if (isFALSE(rlang::has_name(data, x))) {
      cli::cli_abort(
        c(
          "Column '{x}' does not exist in the dataframe.",
          "i" = "Ensure you have supplied the correct column names for {paste(c(names), collapse = ', ')}."
        ),
        call = .envir
      )
    }
  })
}
