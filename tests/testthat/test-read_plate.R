test_that("Reading tecan plate files.", {
  fl <- system.file(
    "extdata",
    "calibrate_tecan",
    "calTecan1.xlsx",
    package = "reluxr"
  )

  expect_snapshot(plate_read_tecan(fl))
})
