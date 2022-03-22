#' Convert Letter to Number
#'
#' @param x String letter to convert.
#'
#' @return
#' @export
#'
#' @examples
let_to_num <- function(x) {
  x <- stringr::str_to_upper(x)
  as.numeric(factor(x, levels = LETTERS))
}

#' Convert Number to Letter
#'
#' @param x Number to convert to corresponding capital letter.
#'
#' @return
#' @export
#'
#' @examples
num_to_let <- function(x) {
  LETTERS[x]
}

# pad out a number to two digits, returned as a string

#' Pad Out Single Digit Numbers
#'
#' Pads out the input, to the left, with zeros, to the given width.
#'
#' @param x Number to pad out
#' @param width How many characters to pad out to.
#'
#' @return
#' @export
#'
#' @examples
num_pad <- function(x, width = 2) {
  stringr::str_pad(x, width = width, side = "left", pad = "0")
}

# given a column number and a row (either number or letter) return the
# 3-character well that is joined from the two

#' Join a row and column into a well ID.
#'
#' Joins a column number and a row letter or number into a well ID. i.e. joins
#' "A" and "1" to "A01" and joins "3 and "10" to "C10".
#'
#' @param row
#' @param col
#' @param num_width
#'
#' @return
#' @export
#'
#' @examples
well_join <- function(row, col, num_width = 2) {
  if (is.character(row)) {
    if(stringr::str_detect(row, "\\d")) {
      row <- as.numeric(row)
    }
  }

  if (is.numeric(row)) {
    row <- LETTERS[row]
  }

  as.character(paste0(row, num_pad(col, width = num_width)))
}

#' Extract number from well ID.
#'
#' @param x Well ID as string.
#'
#' @return
#' @export
#'
#' @examples
well_to_colnum <- function(x) {
  stringr::str_extract(as.character(x), "\\d+$") %>%
    as.numeric()
}

#' Extracts letter from well ID.
#'
#' @param x Well ID as string.
#'
#' @return
#' @export
#'
#' @examples
well_to_rowlet <- function(x) {
  stringr::str_extract(as.character(x), "^\\w")
}

#' Convert Well ID to Row Number
#'
#' @param x Well ID as a string.
#'
#' @return Numeric row number.
#' @export
#'
#' @examples
well_to_rownum <- function(x) {
  well_to_rowlet(x) %>%
    let_to_num()
}

#' Calculate number of columns from given number of wells.
#'
#' @param x Number of wells in plate, e.g. 384 or 96.
#'
#' @return
n_cols_from_wells <- function(x) {
  switch(as.character(x),
         "6" = 3,
         "12" = 4,
         "24" = 6,
         "96" = 12,
         "384" = 24)
}

#' Calculate number of rows from given number of wells.
#'
#' @param x Number of wells in plate, e.g. 384 or 96.
#'
#' @return
n_rows_from_wells <- function(x) {
  switch(as.character(x),
         "6" = 2,
         "12" = 3,
         "24" = 4,
         "96" = 8,
         "384" = 16)
}

#' Calculate Distance Between Wells
#'
#' Calculates distance between row and column positions, relative to a calibration well given by `calibrate_row` and `calibrate_col`.
#'
#' @param row Vector of row numbers.
#' @param col Vector of column numbers.
#' @param calibrate_row Single row number to measure from.
#' @param calibrate_col Single column number to measure from.
#'
#' @return
#' @export
#'
#' @examples
well_dis <- function (row, col, calibrate_row = 5, calibrate_col = 5) {
  sqrt((row - calibrate_row) ^ 2 + (col - calibrate_col) ^ 2)
}

#' Predict Background Bleedthrough
#'
#' @param dis Distance between wells.
#' @param height Height of the sensor.
#' @param well_spacing Spacing of the wells.
#'
#' @return
#' @export
#'
#' @examples
predict_bleedthrough <- function(dis,
                               height = 3,
                               well_spacing = 25) {
  (height ^ 3) / (
    height ^ 2 + (dis * well_spacing) ^ 2
  ) ^ (3/2)
}
