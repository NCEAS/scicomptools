# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-issue_extract.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(issue_extract())
  expect_error(issue_extract(repo_url = "google.com"))
  expect_error(issue_extract(repo_url = "https://github.com/tidyverse/dplyr",
                             issue_nums = c("a", "b", "c")))
  expect_error(issue_extract(repo_url = "https://github.com/tidyverse/dplyr",
                             issue_nums = c(1, 3, 5, 7.9)))
  
})