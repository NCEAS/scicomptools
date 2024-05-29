# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-molec_wt.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(molec_wt())
  expect_error(molec_wt(element = "not_element"))
  expect_error(molec_wt(element = 119))
})

# Output testing
test_that("Outputs are correct", {
  expect_equal(molec_wt(element = "Oxygen"), 15.999)
  expect_equal(molec_wt(element = "hydrogen"), 1.007)
  expect_equal(molec_wt(element = "plutonium"), 244)
})


