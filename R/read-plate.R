#' Read luminescent values from a plate.
#'
#' @param path Path to excel file with luminescent values.
#'
#' @return
#' @export
#'
#' @examples
read_plate <- function(path) {
  readxl::read_excel(
    path = path,
    skip = 168
  ) %>%
    tidyr::pivot_longer(
      cols = matches("\\w\\d{1,2}"),
      names_to = "well",
      values_to = "lum"
    ) %>%
    janitor::clean_names() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      row = well_to_rownum(well),
      col = well_to_colnum(well),
      well = well_join(row, col),
      lum = as.numeric(lum),
      time_s = as.numeric(time_s),
      cycle_nr = as.numeric(cycle_nr)
    )
}
