# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-wd_loc.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(wd_loc(local = FALSE))
})

# Message testing
test_that("Messages are returned", {
  expect_message(wd_loc(local = "TRUE"))
  expect_message(wd_loc(local = "FALSE"))
})

# Output testing
test_that("Outputs are correct", {
  expect_true(class(wd_loc()) == "character")
})