# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-token_check.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(token_check(api = "some_other_api"))
})

# Message testing
test_that("Messages are returned", {
  expect_message(token_check())
})