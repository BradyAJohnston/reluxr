#' @noRd
well_check <- function(x) {
  row_letter <- stringr::str_detect(
    as.character(x),
    "^[:alpha:](?=\\d)"
  )

  col_numer <- stringr::str_detect(
    as.character(x),
    "\\d{1,3}$"
  )

  row_letter & col_numer
}

#' @noRd
is_well_id <- function(x) {
  well_id_regex <- "[:alpha:]{1,2}\\d{1,3}"

  stringr::str_detect(
    stringr::str_trim(x),
    well_id_regex
  )
}

#' @noRd
well_join <- function(row, col, num_width = 2) {

  # rowlet <- ifelse(
  #   is.numeric(row),
  #   LETTERS[as.numeric(row)],
  #   stringr::str_trim(row)
  #   )

  # if row is character, coerce first to a numeric. Stop if unsuccessful
  rowlet <- sapply(row, function(x) {
    if (is.character(x)) {
      if (stringr::str_detect(x, "\\d+")) {
        y <- as.numeric(x)

        if (is.na(x)) {
          stop(paste("Cannot coerce supplied row:", x))
        }

        if (y > 26 | y < 1) {
          stop(paste("Row number", y, "cannot exceed the number of available letters (1:26)."))
        }
        # return corresponding capital letter
        LETTERS[y]
      } else {
        # print(x)
        x
      }
    } else {
      LETTERS[x]
    }
  })


  # pad out the column number for format "A01" properly.
  colnum <- stringr::str_pad(col, width = num_width, side = "left", pad = "0")

  # join the final well
  well <- as.character(paste0(rowlet, colnum))
  # quality control the well

  well
}

#' @noRd
well_to_col_num <- function(x) {
  x <- stringr::str_trim(x)

  x <- stringr::str_extract(x, "\\d+$")

  as.numeric(x)
}

#' @noRd
well_to_row_let <- function(x) {
  x <- stringr::str_trim(x)

  x <- stringr::str_to_upper(x)
  let <- stringr::str_extract(x, "^[^\\d]+")
  let
}

#' @noRd
well_to_row_num <- function(x) {
  x <- stringr::str_to_upper(x)
  row_let <- well_to_row_let(x)
  row_num <- factor(row_let, levels = LETTERS)
  as.numeric(row_num)
}

#' @noRd
well_to_index <- function(x, plate = 96, colwise = FALSE) {
  stopifnot(is.character(x))


  colnum <- well_to_col_num(x)
  rownum <- well_to_row_num(x)

  if (colwise) {
    n_rows <- n_rows_from_wells(plate)
    id <- (colnum - 1) * n_rows + rownum
  } else {
    n_cols <- n_cols_from_wells(plate)
    id <- (rownum - 1) * n_cols + colnum
  }

  id
}

#' @noRd
well_from_index <- function(x, plate = 96, num_width = 2, colwise = FALSE) {
  stopifnot(is.numeric(x))
  n_rows <- n_rows_from_wells(plate)
  n_cols <- n_cols_from_wells(plate)

  if (colwise) {
    id_row <- .count_row_down(x, n_rows)
    id_col <- .count_col_down(x, n_rows)
  } else {
    id_row <- .count_row_across(x, n_cols)
    id_col <- .count_col_across(x, n_cols)
  }

  well <- well_join(id_row, id_col)

  well
}

#' @noRd
well_format <- function(x, num_width = 2, uppercase = TRUE) {
  x <- well_join(
    row = well_to_row_num(x),
    col = well_to_col_num(x),
    num_width = num_width
  )

  if (!uppercase) {
    stringr::str_to_lower(x)
  } else {
    x
  }
}

#' @noRd
n_cols_from_wells <- function(x) {
  stopifnot(is.numeric(x))
  switch(as.character(x),
    "6" = 3,
    "12" = 4,
    "24" = 6,
    "96" = 12,
    "384" = 24
  )
}

#' @noRd
n_rows_from_wells <- function(x) {
  stopifnot(is.numeric(x))
  switch(as.character(x),
    "6" = 2,
    "12" = 3,
    "24" = 4,
    "96" = 8,
    "384" = 16
  )
}

#' @noRd
.count_row_down <- function(x, n_rows) {
  .count <- x %% n_rows
  .count[.count == 0] <- n_rows
  .count
}

#' @noRd
.count_col_down <- function(x, n_rows) {
  .count <- x %/% n_rows + 1
  .count[x %% n_rows == 0] <- .count[x %% n_rows == 0] - 1
  .count
}

#' @noRd
.count_row_across <- function(x, n_col) {
  .count <- x %/% n_col + 1
  .count[x %% n_col == 0] <- .count[x %% n_col == 0] - 1
  .count
}

#' @noRd
.count_col_across <- function(x, n_col) {
  .count <- x %% n_col
  .count[.count == 0] <- n_col
  .count
}

#' @noRd
well_dis <- function(row, col, ref_row, ref_col) {
  sqrt((row - ref_row)^2 + (col - ref_col)^2)
}

#' @noRd
well_dis_plate <- function(ref_row, ref_col, plate = 96) {
  n_cols <- n_cols_from_wells(plate)
  n_rows <- n_rows_from_wells(plate)

  outer(
    X = seq(n_rows),
    Y = seq(n_cols),
    FUN = well_dis,
    ref_row = ref_row,
    ref_col = ref_col
  )
}
