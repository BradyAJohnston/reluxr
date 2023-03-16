test_that("Check columns exist.", {

  dat <- data.frame(
    time = 1:10,
    well = LETTERS[1:10],
    value = rnorm(10)
  )

  expect_true({.check_columns_exist(dat, substitute(time))})
  expect_error(.check_columns_exist(dat, substitute(time_s)),
               regexp = "Column 'time_s' does not exist in the dataframe")

  dat2 <- data.frame(
    rime = 1:10,
    well = LETTERS[1:10],
    value = rnorm(10)
  )
  expect_true({.check_columns_exist(dat2, substitute(rime))})
  expect_error(.check_columns_exist(dat2, substitute(time)),
               regexp = "Column 'time' does not exist in the dataframe")


})
