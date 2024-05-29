# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-word_clouds.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(word_cloud_prep())
  
  bird_text <- data.frame(num = 1:10,
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
  expect_error(word_cloud_prep(data = bird_text, text_column = "seen_birds"))
  
  bird_text_list <- as.list(bird_text)
  expect_error(word_cloud_prep(data = bird_text_list, text_column = "birds"))
  
  logical_text <- data.frame(num = 1:3,
                             value = c(TRUE, TRUE, TRUE))
  expect_error(word_cloud_prep(data = logical_text, text_column = "value"))
})

# Output testing
test_that("Outputs are correct", {
  bird_text <- data.frame(num = 1:10,
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
  
  expect_equal(nrow(word_cloud_prep(data = bird_text, text_column = "birds")), 7)
  expect_contains(class(word_cloud_prep(data = bird_text, text_column = "birds")), "data.frame")
})
