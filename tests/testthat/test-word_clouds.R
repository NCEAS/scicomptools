# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-word_clouds.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(word_cloud_prep())
})

# Output testing
test_that("Outputs are correct", {
  text <- data.frame(num = 1:10,
                     birds = c("pigeon",
                               "finch",
                               "duck",
                               "swan",
                               "pigeon",
                               "seagull",
                               "cardinal",
                               "bluejay",
                               "finch",
                               "duck"))

  expect_equal(nrow(word_cloud_prep(data = text, text_column = "birds")), 7)
  expect_contains(class(word_cloud_prep(data = text, text_column = "birds")), "data.frame")
})
