# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-excel_extract.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(read_xl_sheets())
  expect_error(read_xl_sheets(file_name = "abc"))
  
  expect_error(read_xl_format())
  expect_error(read_xl_format(file_name = "def"))
})

# Output testing
test_that("Outputs are correct", {
  x <- read_xl_sheets(file_name = system.file("extdata", "excel_book.xlsx", package = "scicomptools"))
  expect_true(class(x) == "list")
  
  y <- read_xl_format(file_name = system.file("extdata", "excel_book.xlsx", package = "scicomptools"))
  expect_true(class(y) == "data.frame")
})


