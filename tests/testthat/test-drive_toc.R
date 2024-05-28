# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-drive_toc.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(drive_toc())
  expect_error(drive_toc(url = "google.com"))
})

